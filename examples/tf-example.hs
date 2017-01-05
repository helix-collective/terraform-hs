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
-- An s3 bucket with some derived content
  
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
  si_deployBucket :: AwsS3Bucket
}

shared :: TF SharedInfrastructure
shared = do
  nd <- networking
  db <- s3
  return SharedInfrastructure
    { si_networkDetails = nd
    , si_deployBucket = db
    }

----------------------------------------------------------------------
-- An application server deployed into the shared infrastructure
  
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
  
mkAppServer :: NetworkDetails -> AwsSecurityGroup -> InstanceType  -> TF ()
mkAppServer nd securityGroup instanceType = do
  ec2 <- awsInstance "appserver" "ami-623c0d01" instanceType def
        { i_tags = demoappTags
        , i_subnet_id = Just (sn_id (az_external_subnet (head (nd_azs nd))))
        , i_vpc_security_group_ids = [sg_id securityGroup]
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

demoapp :: NetworkDetails -> TF ()
demoapp networkDetails = do
  sg <- awsSecurityGroup "sgappserver" def
        { sg_tags = demoappTags
        , sg_vpc_id = Just (vpc_id (nd_vpc networkDetails))
        , sg_ingress =
          [ ingressOnPort 22
          , ingressOnPort 80
          , ingressOnPort 443
          ]
        , sg_egress =
          [ egressAll
          ]
        }
  
  withNameScope "prod" $ do
    mkAppServer networkDetails sg "t2.medium"
  withNameScope "uat" $ do
    mkAppServer networkDetails sg "t2.micro"

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
    demoapp (si_networkDetails sharedInfrastructure)