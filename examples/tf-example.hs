{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Language.Terraform.Util.Text as T

import Control.Monad(void)
import Data.Default
import Data.Traversable(for)
import Data.Monoid
import Language.Terraform.Core
import Language.Terraform.Aws

----------------------------------------------------------------------
-- Configuration

s3BucketPrefix :: T.Text
s3BucketPrefix = "com-somebucketprefix-"

cidrBlock :: CidrBlock
cidrBlock = "10.30.0.0/16"

azparams :: [(NameElement, [Char], CidrBlock, CidrBlock)]


azparams =
     [ ("apse2a", "ap-southeast-2a", "10.30.0.0/19", "10.30.32.0/20")
     , ("apse2b", "ap-southeast-2b", "10.30.64.0/19", "10.30.96.0/20")
     , ("apse2c", "ap-southeast-2c", "10.30.128.0/19", "10.30.160.0/20")
     ]

----------------------------------------------------------------------     
-- The AWS networking infrastructure

data AzDetails = AzDetails {
  az_name :: T.Text,
  az_external_subnet :: AwsSubnet,
  az_internal_subnet :: AwsSubnet
  }
  
data NetworkDetails = NetworkDetails {
  nd_vpc :: AwsVpc,
  nd_azs :: [AzDetails]
  }

networking :: TF NetworkDetails
networking = do

  vpc <- awsVpc "vpc" cidrBlock def
  ig <- awsInternetGateway "gw" (vpc_id vpc) def
  rtexternal <- awsRouteTable "rtexternal" (vpc_id vpc) def
  void $ awsRoute "r" (rt_id rtexternal) "0.0.0.0/0" def
    { r_gateway_id=Just (ig_id ig)
    }

  -- Generate resources for each availability zone
  azs <- for azparams $ \(azname, availabilityZone,internalCidrBlock,externalCidrBlock) -> do
    withNameScope azname $ do
      -- External subnet
      snexternal <- awsSubnet "snexternal" (vpc_id vpc) externalCidrBlock def
      eip <- awsEip "ngeip" def
        { eip_vpc = True
        }
      ng <- awsNatGateway "ng" (eip_id eip) (sn_id snexternal) def
      dependsOn ng ig
      void $ awsRouteTableAssociation "raexternal" (sn_id snexternal) (rt_id rtexternal) def

      -- Internal subnet
      rtinternal <- awsRouteTable "rtinternal" (vpc_id vpc) def
      void $ awsRoute "r1" (rt_id rtinternal) "0.0.0.0/0" def
        { r_nat_gateway_id=Just (ng_id ng)
        }
      sninternal <- awsSubnet "sninternal" (vpc_id vpc) internalCidrBlock def
      awsRouteTableAssociation "rtainternal" (sn_id sninternal)  (rt_id rtinternal) def
      return (AzDetails azname snexternal sninternal)
  return (NetworkDetails vpc azs)

----------------------------------------------------------------------
-- An s3 bucket with some content derived from the infrastructure
  
s3 :: TF AwsS3Bucket
s3 = do
  deployBucket <- awsS3Bucket "deploy" (s3BucketPrefix <> "shared-deploy") def 
  awsS3BucketObject "tfconfig" (s3_id deployBucket) "shared/config/tf-variables.sh" def
    { s3o_content = Just $ T.unlines
      [ "# Shared infrastructure details"
      , T.template "TF_OUTPUT_s3_deploy_bucket = \"$1\"" [tfRefText (s3_id deployBucket)]
      ]
    }
  return deployBucket


----------------------------------------------------------------------
-- All "shared" infrastructure

data SharedInfrastructure = SharedInfrastructure {
  si_networkDetails :: NetworkDetails,
  si_deployBucket :: AwsS3Bucket,
  si_alertTopic :: AwsSnsTopic,
  si_alarmTopic :: AwsSnsTopic
}

namedSnsTopic :: T.Text -> TF AwsSnsTopic
namedSnsTopic n = do
  sn <- scopedName n
  awsSnsTopic n sn def

shared :: TF SharedInfrastructure
shared = do
  nd <- networking
  db <- s3
  alarms <- namedSnsTopic "alarms"
  alerts <- namedSnsTopic "alerts"
    
  return SharedInfrastructure
    { si_networkDetails = nd
    , si_deployBucket = db
    , si_alarmTopic = alarms
    , si_alertTopic = alerts
    }

----------------------------------------------------------------------
-- An application server deployed into the shared infrastructure,
-- with both uat and prod instances  
  
demoappTags = M.fromList
  [ ("tf-stack","demoapp")
  , ("cost-center","demoapp")
  ]

ingressOnPort :: Int -> IngressRuleParams
ingressOnPort port = IngressRuleParams
  { ir_from_port = port
  , ir_to_port = port
  , ir_protocol = "tcp"
  , ir_options = def
    { ir_cidr_blocks = ["0.0.0.0/0"]
    }
  }

egressAll :: EgressRuleParams
egressAll = EgressRuleParams
  { er_from_port = 0
  , er_to_port = 0
  , er_protocol = "-1"
  , er_options = def
    { er_cidr_blocks = ["0.0.0.0/0"]
    }
  }

mkPostgres :: NetworkDetails -> AwsDbSubnetGroup -> DBInstanceClass -> TF AwsDbInstance
mkPostgres nd subnetGroup instanceClass = do
  db <- awsDbInstance' "db" AwsDbInstanceParams
    { db_allocated_storage = 5
    , db_engine = "postgres"
    , db_instance_class = instanceClass
    , db_username = "postgres"
    , db_password = "password"
    , db_options = def
        { db_engine_version = "9.4.7"
        , db_publicly_accessible = True
        , db_backup_retention_period = 3
        , db_db_subnet_group_name = Just (dsg_name subnetGroup)
        }
    }
  output "dbaddress" (tfRefText (db_address db))
  return db
  
mkAppServer :: NetworkDetails -> AwsSecurityGroup -> AwsIamInstanceProfile -> InstanceType  -> TF AwsInstance
mkAppServer nd securityGroup iamInstanceProfile instanceType = do
  ec2 <- awsInstance "appserver" "ami-623c0d01" instanceType def
        { i_tags = demoappTags
        , i_subnet_id = Just (sn_id (az_external_subnet (head (nd_azs nd))))
        , i_vpc_security_group_ids = [sg_id securityGroup]
        , i_iam_instance_profile = Just (iamip_id iamInstanceProfile)
        , i_root_block_device = Just $ RootBlockDeviceParams
          { rbd_options = def
            { rbd_volume_size = Just 20
            }
          }
        }
  eip <- awsEip "appserverip" def
        { eip_instance=Just (i_id ec2)
        , eip_vpc = True
        }
  output "appserverip" (tfRefText (eip_public_ip eip))
  return ec2

iamPolicy = T.intercalate "\n"
  [ "{"
  , "\"Version\": \"2012-10-17\","
  , "\"Statement\": ["
  , "   {"
  , "     \"Action\": \"sts:AssumeRole\","
  , "     \"Principal\": { \"Service\": \"ec2.amazonaws.com\" },"
  , "     \"Effect\": \"Allow\","
  , "     \"Sid\": \"\""
  , "   }"
  , " ]"
  , "}"
  ]

publishMetricsPolicy = T.intercalate "\n"  
  [ "{"
  , "  \"Statement\": ["
  , "    {"
  , "      \"Action\": ["
  , "        \"cloudwatch:GetMetricStatistics\","
  , "        \"cloudwatch:ListMetrics\","
  , "        \"cloudwatch:PutMetricData\","
  , "        \"ec2:DescribeTags\""
  , "      ],"
  , "      \"Effect\": \"Allow\","
  , "      \"Resource\": \"*\""
  , "    }"
  , "  ]"
  , "}"
  ]

s3ReadonlyPolicy :: AwsS3Bucket -> T.Text
s3ReadonlyPolicy bucket = T.intercalate "\n"
  [ "{"
  , "  \"Version\": \"2012-10-17\","
  , "  \"Statement\": ["
  , "        {"
  , "            \"Action\": ["
  , "                \"s3:GetObject\""
  , "            ],"
  , "            \"Effect\": \"Allow\","
  , "            \"Resource\": ["
  , T.template
    "                \"arn:aws:s3:::$1/*\""
    [tfRefText (s3_id bucket)]
  , "            ]"
  , "        }"
  , "    ]"
  , "}"
  ]

highDiskAlert :: AwsSnsTopic -> AwsInstance -> TF AwsCloudwatchMetricAlarm
highDiskAlert topic ec2Instance = do
  sn <- scopedName "highdisk"
  awsCloudwatchMetricAlarm' "highdisk" AwsCloudwatchMetricAlarmParams
    { cma_alarm_name = sn
    , cma_comparison_operator = "GreaterThanThreshold"
    , cma_evaluation_periods = 1
    , cma_metric_name = "DiskSpaceUtilization"
    , cma_namespace = "System/Linux"
    , cma_period = 300
    , cma_statistic = "Average"
    , cma_threshold = 90
    , cma_options = def
      { cma_dimensions = M.fromList
        [ ("InstanceId", tfRefText (i_id ec2Instance) )
        , ("Filesystem", "/dev/xvda1")
        , ("MountPath", "/")
        ]
      , cma_alarm_description = "Sustained high disk usage for application server"
      , cma_alarm_actions = [sns_arn topic]
      }
    }

highCpuAlert :: AwsSnsTopic -> AwsInstance -> TF AwsCloudwatchMetricAlarm
highCpuAlert topic ec2Instance = do
  sn <- scopedName "highcpu"
  awsCloudwatchMetricAlarm' "highcpu" AwsCloudwatchMetricAlarmParams
    { cma_alarm_name = sn
    , cma_comparison_operator = "GreaterThanThreshold"
    , cma_evaluation_periods = 4
    , cma_metric_name = "DiskSpaceUtilization"
    , cma_namespace = "AWS/EC2"
    , cma_period = 300
    , cma_statistic = "Average"
    , cma_threshold = 90
    , cma_options = def
      { cma_dimensions = M.fromList
        [ ("InstanceId", tfRefText (i_id ec2Instance) )
        ]
      , cma_alarm_description = "Sustained high cpu usage for application server"
      , cma_alarm_actions = [sns_arn topic]
      }
    }

demoapp :: SharedInfrastructure -> TF ()
demoapp sharedInfrastructure = do
  let networkDetails = si_networkDetails sharedInfrastructure
  sg <- awsSecurityGroup "sgappserver" def
        { sg_tags = demoappTags
        , sg_vpc_id = Just  (vpc_id (nd_vpc networkDetails))
        , sg_ingress =
          [ ingressOnPort 22
          , ingressOnPort 80
          , ingressOnPort 443
          ]
        , sg_egress =
          [ egressAll
          ]
        }

  iamr <- awsIamRole "appserver" iamPolicy def
  iamip <- awsIamInstanceProfile "appserver" def
    { iamip_roles=[iamr_name iamr]
    }

  let namedPolicy name0 policy = do
        name <- scopedName name0
        awsIamRolePolicy name0 name policy (iamr_id iamr) def

  void $ namedPolicy "publishmetrics"
    publishMetricsPolicy
  void $ namedPolicy "deployaccess"
    (s3ReadonlyPolicy (si_deployBucket sharedInfrastructure))
  
  dbsg <- do
    sname <- scopedName "dsg"
    awsDbSubnetGroup "dsg" sname
      [sn_id (az_external_subnet az) | az <- nd_azs networkDetails] def

  withNameScope "prod" $ do
    ec2 <- mkAppServer networkDetails sg iamip "t2.medium"
    mkPostgres networkDetails dbsg "db.t2.medium"
    
    highDiskAlert (si_alertTopic sharedInfrastructure) ec2
    highCpuAlert (si_alertTopic sharedInfrastructure) ec2
    
  withNameScope "uat" $ do
    ec2 <- mkAppServer networkDetails sg iamip "t2.micro"
    mkPostgres networkDetails dbsg "db.t2.micro"

  return ()

----------------------------------------------------------------------
-- Combine everything and generate the terraform file    

main = generateFiles "/tmp" $ do
  sharedInfrastructure <- withNameScope "shared" $ do
    aws AwsParams
      { aws_region = "ap-southeast-2"
      , aws_options = def
      }
    shared

  withNameScope "demoapp" $ do
    demoapp sharedInfrastructure
