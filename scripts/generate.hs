#!/usr/bin/env stack
{- stack --stack-yaml ./stack.yaml runghc --package terraform-hs -}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Terraform.Util.Text as T

import Data.List(intercalate,intersperse)
import System.FilePath((</>))
import Data.Monoid

awsHeader :: Code
awsHeader = clines
    [ "type AwsRegion = T.Text"
    , "data AwsId a = AwsId"
    , "type CidrBlock = T.Text"
    , "type AvailabilityZone = T.Text"
    , "type Ami = T.Text"
    , "type InstanceType = T.Text"
    , "type KeyName = T.Text"
    , "type S3BucketName = T.Text"
    , "type S3Key = T.Text"
    , "type Arn = T.Text"
    , "newtype IpAddress = IpAddress T.Text"
    , "type VolumeType = T.Text"
    , "type CannedAcl = T.Text"
    , "type MetricComparisonOperator = T.Text"
    , "type MetricNamespace = T.Text"
    , "type MetricName = T.Text"
    , "type MetricStatistic = T.Text"
    , "type MetricUnit = T.Text"
    , "type DBEngine = T.Text"
    , "type DBInstanceClass = T.Text"
    , "type HostedZoneId = T.Text"
    , "type Route53RecordType = T.Text"
    , ""
    , "-- A typed ARN"
    , "newtype AwsArn t = AwsArn {"
    , "  tArn :: Arn"
    , "} deriving (Eq);"
    , ""
    , "instance ToResourceField (AwsArn t) where"
    , "  toResourceField (AwsArn t) = toResourceField t"
    , ""
    , "-- | Add an aws provider to the resource graph."
    , "--"
    , "-- See the original <https://www.terraform.io/docs/providers/aws/index.html terraform documentation>"
    , "-- for details."
    , ""
    , "newAws :: AwsParams -> TF ()"
    , "newAws params ="
    , "  mkProvider \"aws\" $ catMaybes"
    , "    [ Just (\"region\", toResourceField (aws_region params))"
    , "    , let v = aws_access_key params in if v == \"\" then Nothing else (Just (\"access_key\", toResourceField v))"
    , "    , let v = aws_secret_key params in if v == \"\" then Nothing else (Just (\"secret_key\", toResourceField v))"
    , "    ]"
    , ""
    , "data AwsParams = AwsParams"
    , "  { aws_region :: AwsRegion"
    , "  , aws_access_key :: T.Text"
    , "  , aws_secret_key :: T.Text"
    , "  }"
    , ""
    , "makeAwsParams :: AwsRegion -> AwsParams"
    , "makeAwsParams region = AwsParams region \"\" \"\""
    ]

awsResources :: [Code]
awsResources =
  [resourceCode  "aws_vpc" "vpc"
    "https://www.terraform.io/docs/providers/aws/d/vpc.html"
    [ ("cidr_block",           NamedType "CidrBlock",       Required)
    , ("instance_tenancy",     NamedType "T.Text",          Optional)
    , ("enable_dns_support",   NamedType "Bool",            OptionalWithDefault "True")
    , ("enable_dns_hostnames", NamedType "Bool",            OptionalWithDefault "False")
    , ("enable_classic_link",  NamedType "Bool",            OptionalWithDefault "False")
    , ("tags",                 TagsMap,                     OptionalWithDefault "M.empty")
    ]
    [ ("id",                  AwsIdRef "aws_vpc")
    ]

  , resourceCode  "aws_nat_gateway" "ng"
    "https://www.terraform.io/docs/providers/aws/r/nat_gateway.html"
    [ ("allocation_id", AwsIdRef "aws_eip", Required)
    , ("subnet_id",     AwsIdRef "aws_subnet",  Required)
    ]
    [ ("id",     AwsIdRef "aws_nat_gateway")
    ]

  , resourceCode  "aws_internet_gateway" "ig"
    "https://www.terraform.io/docs/providers/aws/r/internet_gateway.html"
    [ ("vpc_id", AwsIdRef "aws_vpc", Required)
    , ("tags",   TagsMap,            OptionalWithDefault "M.empty")
    ]
    [ ("id",     AwsIdRef "aws_internet_gateway")
    ]

  , resourceCode "aws_subnet" "sn"
    "https://www.terraform.io/docs/providers/aws/d/subnet.html"
    [ ("vpc_id", AwsIdRef "aws_vpc", Required)
    , ("cidr_block", NamedType "CidrBlock", Required)
    , ("map_public_ip_on_launch", NamedType "Bool", OptionalWithDefault "False")
    , ("availability_zone", NamedType "AvailabilityZone", OptionalWithDefault "\"\"")

    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    ]
    [ ("id", AwsIdRef "aws_subnet")
    ]

  , resourceCode  "aws_route_table" "rt"
    "https://www.terraform.io/docs/providers/aws/r/route_table.html"
    [ ("vpc_id", AwsIdRef "aws_vpc", Required)
    , ("tags",   TagsMap,            OptionalWithDefault "M.empty")
    ]
    [ ("id",     AwsIdRef "aws_route_table")
    ]

  , resourceCode  "aws_route" "r"
    "https://www.terraform.io/docs/providers/aws/r/route.html"
    [ ("route_table_id",         AwsIdRef "aws_route_table",      Required)
    , ("destination_cidr_block", NamedType "CidrBlock",           Required)
    , ("nat_gateway_id",         AwsIdRef "aws_nat_gateway",      Optional)
    , ("gateway_id",             AwsIdRef "aws_internet_gateway", Optional)
    ]
    []

  , resourceCode  "aws_route_table_association" "rta"
    "https://www.terraform.io/docs/providers/aws/r/route_table_association.html"
    [ ("subnet_id", AwsIdRef "aws_subnet", Required)
    , ("route_table_id", AwsIdRef "aws_route_table", Required)
    ]
    [ ("id", AwsIdRef "aws_route_table_association")
    ]

  , fieldsCode "IngressRule" "ir" True
    [ ("from_port", NamedType "Int", Required)
    , ("to_port", NamedType "Int", Required)
    , ("protocol", NamedType "T.Text", Required)
    , ("cidr_blocks", NamedType "[CidrBlock]", OptionalWithDefault "[]")
    ]

  , fieldsCode "EgressRule" "er" True
    [ ("from_port", NamedType "Int", Required)
    , ("to_port", NamedType "Int", Required)
    , ("protocol", NamedType "T.Text", Required)
    , ("cidr_blocks", NamedType "[CidrBlock]", OptionalWithDefault "[]")
    ]

  , resourceCode "aws_security_group" "sg"
    "https://www.terraform.io/docs/providers/aws/r/security_group.html"
    [ ("name", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("name_prefix", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("description", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("ingress", NamedType "[IngressRuleParams]", OptionalWithDefault "[]")
    , ("egress", NamedType "[EgressRuleParams]", OptionalWithDefault "[]")
    , ("vpc_id", AwsIdRef "aws_vpc", Optional)
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    ]
    [ ("id", AwsIdRef "aws_security_group")
    , ("owner_id", TFRef "T.Text")
    ]

  , fieldsCode "RootBlockDevice" "rbd" True
    [ ("volume_type", NamedType "VolumeType", OptionalWithDefault "\"standard\"")
    , ("volume_size", NamedType "Int", Optional)
    , ("delete_on_termination", NamedType "Bool", OptionalWithDefault "True")
    ]

  , resourceCode "aws_instance" "i"
    "https://www.terraform.io/docs/providers/aws/r/instance.html"
    [ ("ami", NamedType "Ami", Required)
    , ("availability_zone", NamedType "AvailabilityZone", OptionalWithDefault "\"\"")
    , ("ebs_optimized", NamedType "Bool", Optional)
    , ("instance_type", NamedType "InstanceType", Required)
    , ("key_name", NamedType "KeyName", Optional)
    , ("monitoring", NamedType "Bool", OptionalWithDefault "True")
    , ("subnet_id", AwsIdRef "aws_subnet", Optional)
    , ("associate_public_ip_address", NamedType "Bool", Optional)
    , ("root_block_device", NamedType "RootBlockDeviceParams", Optional)
    , ("user_data", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("iam_instance_profile", AwsIdRef "aws_iam_instance_profile", Optional)
    , ("vpc_security_group_ids", FTList (AwsIdRef "aws_security_group"), OptionalWithDefault "[]")
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    ]
    [ ("id", AwsIdRef "aws_instance")
    , ("public_ip", TFRef "IpAddress")
    , ("private_ip", TFRef "IpAddress")
    ]

  , resourceCode "aws_launch_configuration" "lc"
    "https://www.terraform.io/docs/providers/aws/r/launch_configuration.html"
    [ ("name'", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("name_prefix", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("image_id", NamedType "Ami", Required)
    , ("instance_type",  NamedType "InstanceType", Required)
    , ("iam_instance_profile", AwsIdRef "aws_iam_instance_profile", Optional)
    , ("key_name", NamedType "KeyName", Optional)
    , ("security_groups", FTList (AwsIdRef "aws_security_group"), OptionalWithDefault "[]")
    , ("associate_public_ip_address", NamedType "Bool", Optional)
    , ("user_data", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("enable_monitoring", NamedType "Bool", OptionalWithDefault "True")
    , ("ebs_optimized", NamedType "Bool", Optional)
    , ("root_block_device", NamedType "RootBlockDeviceParams", Optional)
    ]
    [ ("id", AwsIdRef "aws_launch_configuration")
    , ("name", TFRef "T.Text")
    ]

  , resourceCode "aws_autoscaling_group" "ag"
    "https://www.terraform.io/docs/providers/aws/r/autoscaling_group.html"
    [ ("name'", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("name_prefix", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("max_size", NamedType "Int", Required)
    , ("min_size", NamedType "Int", Required)
    , ("vpc_zone_identifier", FTList (AwsIdRef "aws_subnet"), OptionalWithDefault "[]")
    , ("launch_configuration", TFRef "T.Text", Required)
    , ("load_balancers", FTList (TFRef "T.Text"), OptionalWithDefault "[]")
    , ("tag", FTList (NamedType "AsgTagParams"), ExpandedList)
    ]
    [ ("id", AwsIdRef "aws_autoscaling_group")
    , ("arn", TFRef "Arn")
    , ("name", TFRef "T.Text")
    ]

  , fieldsCode "AsgTag" "asg" True
    [ ("key", NamedType "T.Text", Required)
    , ("value", NamedType "T.Text", Required)
    , ("propagate_at_launch", NamedType "Bool", Required)
    ]

  , resourceCode  "aws_eip" "eip"
    "https://www.terraform.io/docs/providers/aws/r/eip.html"
    [ ("vpc", NamedType "Bool", OptionalWithDefault "False")
    , ("instance", AwsIdRef "aws_instance", Optional)
    ]
    [ ("id", AwsIdRef "aws_eip")
    , ("private_ip", TFRef "IpAddress")
    , ("public_ip",  TFRef "IpAddress")
    ]

  , fieldsCode "AccessLogs" "al" True
    [ ("bucket", NamedType "S3BucketName", Required)
    , ("bucket_prefix", NamedType "S3Key", OptionalWithDefault "\"\"")
    , ("interval", NamedType "Int", OptionalWithDefault "60")
    , ("enabled", NamedType "Bool", OptionalWithDefault "True")
    ]

  , fieldsCode "Listener" "l" True
    [ ("instance_port", NamedType "Int", Required)
    , ("instance_protocol", NamedType "T.Text", Required)
    , ("lb_port", NamedType "Int", Required)
    , ("lb_protocol", NamedType "T.Text", Required)
    , ("ssl_certificate_id", NamedType "Arn", Optional)
    ]

  , fieldsCode "HealthCheck" "hc" True
    [ ("healthy_threshold", NamedType "Int", Required)
    , ("unhealthy_threshold", NamedType "Int", Required)
    , ("target", NamedType "T.Text", Required)
    , ("interval", NamedType "Int", Required)
    , ("timeout", NamedType "Int", Required)
    ]

  , resourceCode "aws_elb" "elb"
    "https://www.terraform.io/docs/providers/aws/r/elb.html"
    [ ("name'", NamedType "T.Text", Optional)
    , ("access_logs", NamedType "AccessLogsParams", Optional)
    , ("security_groups", FTList (AwsIdRef "aws_security_group"), OptionalWithDefault "[]")
    , ("subnets", FTList (AwsIdRef "aws_subnet"), OptionalWithDefault "[]")
    , ("instances", FTList (AwsIdRef "aws_instance"), OptionalWithDefault "[]")
    , ("listener", FTList (NamedType "ListenerParams"), Required)
    , ("health_check", NamedType "HealthCheckParams", Optional)
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    ]
    [ ("id", TFRef "T.Text")
    , ("name", TFRef "T.Text")
    , ("dns_name", TFRef "T.Text")
    , ("zone_id", TFRef "T.Text")
    ]

  , fieldsCode "BucketVersioning" "bv" True
    [ ("enabled", NamedType "Bool", OptionalWithDefault "False")
    , ("mfa_delete", NamedType "Bool", OptionalWithDefault "False")
    ]

  , fieldsCode "Expiration" "e" True
    [ ("days", NamedType "Int", Optional)
    , ("date", NamedType "T.Text", Optional)
    , ("expired_object_delete_marker", NamedType "Bool", OptionalWithDefault "False")
    ]

  , fieldsCode "LifecycleRule" "lr" True
    [ ("id", NamedType "T.Text", Optional)
    , ("prefix", NamedType "T.Text", Required)
    , ("enabled", NamedType "Bool", Required)
    , ("expiration", NamedType "ExpirationParams", Optional)
    ]

  , resourceCode "aws_elb_attachment" "elba"
    "https://www.terraform.io/docs/providers/aws/r/elb_attachment.html"
    [ ("elb", NamedType "T.Text", Required)
    , ("instance", NamedType "T.Text", Required)
    ]
    [
    ]

  , resourceCode "aws_autoscaling_attachment" "asa"
    "https://www.terraform.io/docs/providers/aws/r/autoscaling_attachment.html"
    [ ("autoscaling_group_name", NamedType "T.Text", Required)
    , ("elb", NamedType "T.Text", Optional)
    , ("alb_target_group_arn", NamedType "AwsArn AwsLbTargetGroup", Optional)
    ]
    [
    ]

  , fieldsCode "CorsRule" "cors" True
    [ ("allowed_headers", FTList (NamedType "T.Text"), OptionalWithDefault "[]")
    , ("allowed_methods", FTList (NamedType "T.Text"), Required)
    , ("allowed_origins", FTList (NamedType "T.Text"), Required)
    , ("expose_headers", FTList (NamedType "T.Text"), OptionalWithDefault "[]")
    , ("max_age_seconds", NamedType "Int", Optional)
    ]

  , resourceCode "aws_s3_bucket" "s3"
    "https://www.terraform.io/docs/providers/aws/r/s3_bucket.html"
    [ ("bucket", NamedType "T.Text", Required)
    , ("acl", NamedType "CannedAcl", OptionalWithDefault "\"private\"")
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    , ("versioning", NamedType "BucketVersioningParams", Optional)
    , ("lifecycle_rule", NamedType "LifecycleRuleParams", Optional)
    , ("cors_rule", NamedType "CorsRuleParams", Optional)
    ]
    [ ("id", TFRef "S3BucketName")
    ]

  , resourceCode "aws_s3_bucket_object" "s3o"
    "https://www.terraform.io/docs/providers/aws/d/s3_bucket_object.html"
    [ ("bucket", TFRef "S3BucketName", Required)
    , ("key", NamedType "S3Key", Required)
    , ("source", NamedType" FilePath", Optional)
    , ("content", NamedType" T.Text", Optional)
    ]
    [ ("id", TFRef "T.Text")
    , ("etag", TFRef "T.Text")
    , ("version_id", TFRef "T.Text")
    ]

  , resourceCode "aws_iam_user" "iamu"
    "https://www.terraform.io/docs/providers/aws/r/iam_user.html"
    [ ("name'", NamedType "T.Text", Required)
    , ("path", NamedType "T.Text", OptionalWithDefault "\"/\"")
    , ("force_destroy", NamedType "Bool", OptionalWithDefault "False")
    ]
    [ ("arn", TFRef "Arn")
    , ("name", TFRef "T.Text")
    , ("unique_id", TFRef "T.Text")
    ]

  , resourceCode "aws_iam_user_policy" "iamup"
    "https://www.terraform.io/docs/providers/aws/r/iam_user_policy.html"
    [ ("name", NamedType "T.Text", Required)
    , ("policy", NamedType "T.Text", Required)
    , ("user", TFRef "T.Text", Required)
    ]
    []

  , resourceCode "aws_iam_user_policy_attachment" "iamupa"
    "https://www.terraform.io/docs/providers/aws/r/iam_user_policy_attachment.html"
    [ ("user", TFRef "T.Text", Required)
    , ("policy_arn", NamedType "T.Text", Required)
    ]
    []

  , resourceCode "aws_iam_role" "iamr"
    "https://www.terraform.io/docs/providers/aws/r/iam_role.html"
    [ ("name'", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("name_prefix", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("assume_role_policy", NamedType "T.Text", Required)
    , ("path", NamedType "T.Text", OptionalWithDefault "\"\"")
    ]
    [ ("id", AwsIdRef "aws_iam_role")
    , ("arn", TFRef "Arn")
    , ("name", TFRef "T.Text")
    , ("create_date", TFRef "T.Text")
    , ("unique_id", TFRef "T.Text")
    ]

  , resourceCode "aws_iam_instance_profile" "iamip"
    "https://www.terraform.io/docs/providers/aws/r/iam_instance_profile.html"
    [ ("name", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("name_prefix", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("path", NamedType "T.Text", OptionalWithDefault "\"/\"")
    , ("roles", FTList (TFRef "T.Text"), OptionalWithDefault "[]")
    , ("role", TFRef "T.Text", Optional)
    ]
    [ ("id", AwsIdRef "aws_iam_instance_profile")
    , ("arn", TFRef "Arn")
    , ("create_date", TFRef "T.Text")
    , ("unique_id", TFRef "T.Text")
    ]

  , resourceCode "aws_iam_role_policy" "iamrp"
    "https://www.terraform.io/docs/providers/aws/r/iam_role_policy.html"
    [ ("name", NamedType "T.Text", Required)
    , ("policy", NamedType "T.Text", Required)
    , ("role", AwsIdRef "aws_iam_role", Required)
    ]
    [ ("id", AwsIdRef "aws_iam_instance_profile")
    ]

  , resourceCode "aws_sns_topic" "sns"
    "https://www.terraform.io/docs/providers/aws/r/sns_topic.html"
    [ ("name", NamedType "T.Text", Required)
    , ("display_name", NamedType "T.Text", OptionalWithDefault "\"\"")
    ]
    [ ("id", AwsIdRef "aws_sns_topic")
    , ("arn", TFRef "Arn")
    ]

  , resourceCode "aws_cloudwatch_metric_alarm" "cma"
    "https://www.terraform.io/docs/providers/aws/r/cloudwatch_metric_alarm.html"
    [ ("alarm_name", NamedType "T.Text", Required)
    , ("comparison_operator", NamedType "MetricComparisonOperator", Required)
    , ("evaluation_periods", NamedType "Int", Required)
    , ("metric_name", NamedType "MetricName", Required)
    , ("namespace", NamedType "MetricNamespace", Required)
    , ("period", NamedType "Int", Required)
    , ("statistic", NamedType "MetricStatistic", Required)
    , ("threshold", NamedType "Int", Required)
    , ("actions_enabled", NamedType "Bool", OptionalWithDefault "True")
    , ("alarm_actions", FTList (TFRef "Arn"), OptionalWithDefault "[]")
    , ("alarm_description", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("dimensions", TagsMap, OptionalWithDefault "M.empty")
    , ("insufficient_data_actions", FTList (TFRef "Arn"), OptionalWithDefault "[]")
    , ("ok_actions", FTList (TFRef "Arn"), OptionalWithDefault "[]")
    , ("unit", NamedType "MetricUnit", OptionalWithDefault "\"\"")
    ]
    [ ("id", AwsIdRef "aws_cloudwatch_metric_alarm")
    ]

  , resourceCode "aws_db_instance" "db"
    "https://www.terraform.io/docs/providers/aws/r/db_instance.html"
    [ ("allocated_storage", NamedType "Int", Required)
    , ("engine", NamedType "DBEngine", Required)
    , ("engine_version", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("identifier", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("instance_class", NamedType "DBInstanceClass", Required)
    , ("name'", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("port'", NamedType "Int", Optional)
    , ("username'", NamedType "T.Text", Required)
    , ("password", NamedType "T.Text", Required)
    , ("publicly_accessible", NamedType "Bool", OptionalWithDefault "False")
    , ("backup_retention_period", NamedType "Int", OptionalWithDefault "0")
    , ("vpc_security_group_ids", FTList (AwsIdRef "aws_security_group"), OptionalWithDefault "[]")
    , ("db_subnet_group_name", TFRef "T.Text", Optional)
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    , ("skip_final_snapshot", NamedType "Bool", OptionalWithDefault "False")
    , ("final_snapshot_identifier", NamedType "T.Text", Optional)
    ]
    [ ("id", AwsIdRef "aws_db_instance")
    , ("arn", TFRef "Arn")
    , ("name", TFRef "T.Text")
    , ("address", TFRef "T.Text")
    , ("port", TFRef "T.Text")
    , ("username", TFRef "T.Text")
    ]

  , resourceCode "aws_db_subnet_group" "dsg"
    "https://www.terraform.io/docs/providers/aws/r/db_subnet_group.html"
    [ ("name'", NamedType "T.Text", Required)
    , ("description", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("subnet_ids", FTList (AwsIdRef "aws_subnet"), Required)
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    ]
    [ ("id", AwsIdRef "aws_db_subnet_group")
    , ("name", TFRef "T.Text")
    , ("arn", TFRef "Arn")
    ]

  , resourceCode "aws_route53_zone" "r53z"
    "https://www.terraform.io/docs/providers/aws/r/route53_zone.html"
    [ ("name", NamedType "T.Text", Required)
    , ("comment", NamedType "T.Text", OptionalWithDefault "\"Managed by Terraform\"")
    , ("vpc_id", AwsIdRef "aws_vpc", Optional)
    , ("vpc_region", NamedType "AwsRegion", Optional)
    , ("force_destroy", NamedType "Bool", OptionalWithDefault "False")
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    ]
    [ ("zone_id", TFRef "HostedZoneId")
    ]

  , fieldsCode "Route53Alias" "r53a" True
    [ ("zone_id", TFRef "HostedZoneId", Required)
    , ("name", TFRef "T.Text", Required)
    , ("evaluate_target_health", NamedType "Bool", Required)
    ]

  , resourceCode "aws_route53_record" "r53r"
    "https://www.terraform.io/docs/providers/aws/r/route53_record.html"
    [ ("zone_id", TFRef "HostedZoneId", Required)
    , ("name", NamedType "T.Text", Required)
    , ("type", NamedType "Route53RecordType", Required)
    , ("ttl", NamedType "Int", Optional)
    , ("records", FTList (TFRef "IpAddress"), OptionalWithDefault "[]")
    , ("alias", NamedType "Route53AliasParams", Optional)
    ]
    [ ("fqdn", TFRef "T.Text")
    ]

  , resourceCode "aws_sqs_queue" "sqs"
    "https://www.terraform.io/docs/providers/aws/r/sqs_queue.html"
    [ ("name", NamedType "T.Text", Required)
    , ("visibility_timeout_seconds", NamedType "Int", OptionalWithDefault "30")
    , ("message_retention_seconds", NamedType "Int", OptionalWithDefault "345600")
    , ("max_message_size", NamedType "Int", OptionalWithDefault "262144")
    , ("delay_seconds", NamedType "Int", OptionalWithDefault "0")
    , ("receive_wait_time_seconds", NamedType "Int", OptionalWithDefault "0")
    , ("policy", NamedType "T.Text", Optional)
    , ("redrive_policy", NamedType "T.Text", Optional)
    , ("fifo_queue", NamedType "Bool", OptionalWithDefault "False")
    , ("content_based_deduplication", NamedType "Bool", OptionalWithDefault "False")
    ]
    [ ("id", AwsIdRef "aws_sqs_queue")
    , ("arn", TFRef "Arn")
    ]

  , resourceCode "aws_sqs_queue_policy" "sqsp"
    "https://www.terraform.io/docs/providers/aws/r/sqs_queue_policy.html"
    [ ("queue_url", NamedType "T.Text", Required)
    , ("policy", NamedType "T.Text", Required)
    ]
    [
    ]

  , resourceCode "aws_ecr_repository" "ecr"
    "https://www.terraform.io/docs/providers/aws/r/ecr_repository.html"
    [ ("name'", NamedType "T.Text", Required)
    ]
    [ ("arn", TFRef "Arn")
    , ("name", TFRef "T.Text")
    , ("registry_id", TFRef "T.Text")
    , ("repository_url", TFRef "T.Text")
    ]

  , resourceCode "aws_cloudwatch_log_group" "cwlg"
    "https://www.terraform.io/docs/providers/aws/r/cloudwatch_log_group.html"
    [ ("name'", NamedType "T.Text", Required)
    , ("name_prefix'", NamedType "T.Text", Optional)
    , ("retention_in_days'", NamedType "T.Text", Optional)
    , ("kms_key_id", NamedType "T.Text", Optional)
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    ]
    [ ("arn", TFRef "Arn")
    ]

  , fieldsCode "EbsOptions" "edeo" True
    [ ("ebs_enabled", NamedType "Bool", Required)
    , ("volume_type", NamedType "T.Text", Optional)
    , ("volume_size", NamedType "Int", Optional)
    , ("iops", NamedType "Int", Optional)
    ]

  , fieldsCode "ClusterConfig" "edcc" True
    [ ("instance_type", NamedType "InstanceType", Optional)
    , ("instance_count", NamedType "Int", Optional)
    , ("dedicated_master_enabled", NamedType "Bool", Optional)
    , ("dedicated_master_type", NamedType "InstanceType", Optional)
    , ("dedicated_master_count", NamedType "Int", Optional)
    , ("zone_awareness_enabled", NamedType "Bool", Optional)
    ]

  , fieldsCode "SnapshotOptions" "edso" True
    [ ("automated_snapshot_start_hour", NamedType "Int", Required)
    ]

  , resourceCode "aws_elasticsearch_domain" "ed"
    "https://www.terraform.io/docs/providers/aws/r/elasticsearch_domain.html"
    [ ("domain_name'", NamedType "T.Text", Required)
    , ("access_policies", NamedType "T.Text", Optional)
    , ("advanced_options", NamedType "M.Map T.Text T.Text", OptionalWithDefault "M.empty")
    , ("ebs_options", NamedType "EbsOptionsParams", Optional)
    , ("cluster_config", NamedType "ClusterConfigParams", Optional)
    , ("snapshot_options", NamedType "SnapshotOptionsParams", Optional)
    , ("elasticsearch_version", NamedType "T.Text" ,OptionalWithDefault "\"1.5\"")
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    ]
    [ ("arn", TFRef "Arn")
    , ("domain_id", TFRef "T.Text")
    , ("endpoint", TFRef "T.Text")
    ]

  , resourceCode "aws_lb" "lb"
    "https://www.terraform.io/docs/providers/aws/r/lb.html"
    [ ("name", NamedType "T.Text", Optional)
    , ("name_prefix", NamedType "T.Text", Optional)
    , ("internal", NamedType "Bool", OptionalWithDefault "False")
    , ("load_balancer_type", NamedType "LoadBalancerType", OptionalWithDefault "LB_application")
    , ("security_groups", FTList (AwsIdRef "aws_security_group"), OptionalWithDefault "[]")
    , ("access_logs", NamedType "AccessLogsParams", Optional)
    , ("subnets", FTList (AwsIdRef "aws_subnet"), OptionalWithDefault "[]")
    , ("subnet_mapping", NamedType "SubnetMappingParams", Optional)
    , ("idle_timeout", NamedType "Int", OptionalWithDefault "60")
    , ("enable_deletion_protection", NamedType "Bool", OptionalWithDefault "False")
    , ("enable_cross_zone_load_balancing", NamedType "Bool", OptionalWithDefault "False")
    , ("enable_http2", NamedType "Bool", OptionalWithDefault "True")
    , ("ip_address_type", NamedType "T.Text", Optional)
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    ]
    [ ("id", AwsIdRef "aws_lb")
    , ("arn", TFRef "AwsArn AwsLb")
    , ("dns_name", TFRef "T.Text")
    , ("canonical_hosted_zone_id", TFRef "T.Text")
    , ("zone_id", TFRef "T.Text")
    ]

  , fieldsCode "SubnetMapping" "sn" True
    [ ("subnet_id",     AwsIdRef "aws_subnet",  Required)
    , ("allocation_id", AwsIdRef "aws_eip", Required)
    ]

  , resourceCode "aws_lb_listener" "lbl"
    "https://www.terraform.io/docs/providers/aws/r/lb_listener.html"
    [ ("load_balancer_arn", NamedType "AwsArn AwsLb", Required)
    , ("port'", NamedType "Int", Required)
    , ("protocol", NamedType "LoadBalancerProtocol", OptionalWithDefault "LB_HTTP")
    , ("ssl_policy", NamedType "T.Text", Optional)
    , ("certificate_arn", NamedType "AwsArn AwsAcmCertificate", Optional)
    , ("default_action", NamedType "ListenerActionParams", Required)
    ]
    [ ("id", AwsIdRef "aws_lb_listener")
    , ("arn", TFRef "AwsArn AwsLbListener")
    ]

  , enumCode "LoadBalancerType" "LB" ["application","network"]
  , enumCode "LoadBalancerProtocol" "LB" ["TCP","HTTP", "HTTPS"]

  , fieldsCode "ListenerAction" "la" True
    [ ("target_group_arn", NamedType "AwsArn AwsLbTargetGroup",  Required)
    , ("type", NamedType "ListenerActionType", Required)
    ]

  , enumCode "ListenerActionType" "LA" ["forward"]

  , resourceCode "aws_lb_target_group" "lbtg"
    "https://www.terraform.io/docs/providers/aws/r/lb_target_group.html"
    [ ("name'", NamedType "T.Text", Optional)
    , ("name_prefix", NamedType "T.Text", Optional)
    , ("port", NamedType "Int", Required)
    , ("protocol", NamedType "LoadBalancerProtocol", Required)
    , ("vpc_id", AwsIdRef "aws_vpc", Required)
    , ("deregistration_delay", NamedType "Int", OptionalWithDefault "300")
    , ("slow_start", NamedType "Int", OptionalWithDefault "0")
    , ("proxy_protocol_v2", NamedType "Bool", Optional)
    , ("stickiness", NamedType "TargetGroupStickinessParams", Optional)
    , ("health_check", NamedType "TargetGroupHealthCheckParams", Optional)
    , ("target_type", NamedType "TargetGroupTargetType", OptionalWithDefault "TG_instance")
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    ]
    [ ("id", AwsIdRef "aws_lb_target_group")
    , ("arn", TFRef "AwsArn AwsLbTargetGroup")
    , ("name", TFRef "T.Text")
    ]

  , enumCode "TargetGroupTargetType" "TG" ["instance", "ip"]

  , fieldsCode "TargetGroupStickiness" "tgs" True
    [ ("type", NamedType "TargetGroupStickinessType", Required)
    , ("cookie_duration", NamedType "Int", OptionalWithDefault "86400")
    , ("enabled", NamedType "Bool", OptionalWithDefault "True")
    ]

  , enumCode "TargetGroupStickinessType" "TG" ["lb_cookie"]

  , fieldsCode "TargetGroupHealthCheck" "tghc" True
    [ ("interval", NamedType "Int", OptionalWithDefault "30")
    , ("path", NamedType "T.Text", Optional)
    , ("port", NamedType "T.Text", OptionalWithDefault "\"traffic-port\"")
    , ("protocol", NamedType "LoadBalancerProtocol", OptionalWithDefault "LB_HTTP")
    , ("timeout", NamedType "Int", OptionalWithDefault "5")
    , ("healthy_threshold", NamedType "Int", OptionalWithDefault "3")
    , ("unhealthy_threshold", NamedType "Int", OptionalWithDefault "3")
    , ("matcher", NamedType "T.Text", Optional)
    ]

  , resourceCode "aws_lb_target_group_attachment" "lbtga"
    "https://www.terraform.io/docs/providers/aws/r/lb_target_group_attachment.html"
    [ ("target_group_arn", NamedType "AwsArn AwsLbTargetGroup",  Required)
    , ("target_id", NamedType "T.Text", Required)
    , ("port", NamedType "Int", Optional)
    , ("availability_zone", NamedType "AvailabilityZone", Optional)
    ]
    [ ("id", AwsIdRef "aws_lb_target_group_attachment")
    ]

  , resourceCode "aws_lb_listener_rule" "lblr"
    "https://www.terraform.io/docs/providers/aws/r/lb_listener_rule.html"
    [ ("listener_arn", NamedType "AwsArn AwsLbListener",  Required)
    , ("priority", NamedType "Int", Optional)
    , ("action", NamedType "ListenerActionParams", Required)
    , ("condition", NamedType "ListenerConditionParams", Required)
    ]
    [ ("id", AwsIdRef "aws_lb_target_group_attachment")
    , ("arn", TFRef "AwsArn AwsLbListenerRule")
    ]

  , fieldsCode "ListenerCondition" "lblrc" True
    [ ("field", NamedType "ListenerConditionField",  Required)
    , ("values", FTList (NamedType "T.Text"),  Required)
    ]

  , enumCode "ListenerConditionField" "LCF" ["path-pattern","host-header"]

  , resourceCode "aws_acm_certificate" "ac"
    "https://www.terraform.io/docs/providers/aws/d/acm_certificate.html"
    [ ("domain_name", NamedType "T.Text", Required)
    , ("subject_alternative_names", FTList (NamedType "T.Text"),  OptionalWithDefault "[]")
    , ("validation_method", NamedType "CertValidationMethod", Required)
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    ]
    [ ("id", AwsIdRef "aws_acm_certificate")
    , ("arn", TFRef "AwsArn AwsAcmCertificate")
    ]

  , enumCode "CertValidationMethod" "CVM" ["DNS","EMAIL","NONE"]

  , resourceCode "aws_acm_certificate_validation" "acv"
    "https://www.terraform.io/docs/providers/aws/r/acm_certificate_validation.html"
    [ ("certificate_arn", NamedType "AwsArn AwsAcmCertificate", Required)
    , ("validation_record_fqdns", FTList (NamedType "T.Text"), OptionalWithDefault "[]")
    ]
    [
    ]
  ]

data FieldType = NamedType T.Text | TFRef T.Text | AwsIdRef T.Text | FTList FieldType | TagsMap
data FieldMode = Required | Optional | OptionalWithDefault T.Text | ExpandedList

data Code = CEmpty
          | CLine T.Text
          | CAppend Code Code
          | CIndent Code

instance Monoid Code where
  mempty = CEmpty
  mappend = CAppend

codeText :: Code -> [T.Text]
codeText c = mkLines "" c
  where
    mkLines :: T.Text -> Code -> [T.Text]
    mkLines i CEmpty = []
    mkLines i (CAppend c1 c2) = mkLines i c1 <> mkLines i c2
    mkLines i (CIndent c) = mkLines (indentStr <> i) c
    mkLines i (CLine t) = [i <> t]
    indentStr = "  "

cline :: T.Text -> Code
cline = CLine

clines :: [T.Text] -> Code
clines lines = mconcat (map CLine lines)

cblank :: Code
cblank = CLine ""

ctemplate :: T.Text -> [T.Text] -> Code
ctemplate pattern params = CLine $ T.template pattern params

cgroup :: T.Text -> T.Text -> T.Text -> [T.Text] -> Code
cgroup begin sep end [] = CLine (begin <> end)
cgroup begin sep end (t0:ts) = CLine (begin <> t0) <> cgroup1 ts
  where
    cgroup1 [] = CLine end
    cgroup1 (t1:ts) = CLine (sep <> t1) <> cgroup1 ts

enumCode :: T.Text -> T.Text -> [T.Text] -> Code
enumCode htypename fieldprefix values
  =  mconcat (intersperse cblank [decl,toResourceInstance])
  where
    decl = ctemplate "data $1 = $2 deriving (Eq)" [htypename,T.intercalate " | " [hValue v | v <- values]]
    hValue v = fieldprefix <> "_" <>  T.replace "-" "_" v
    toResourceInstance
      =  ctemplate "instance ToResourceField $1 where" [htypename]
      <> mconcat [CIndent $ ctemplate "  toResourceField $1 = \"$2\"" [hValue v,v] | v <- values]

fieldsCode :: T.Text -> T.Text -> Bool -> [(T.Text, FieldType, FieldMode)] -> Code
fieldsCode htypename fieldprefix deriveInstances args
  =  mconcat (intersperse cblank [params,lenses,makeParams,toResourceInstance])
  where
    params =
      (  ctemplate "data $1Params = $1Params" [htypename]
      <> CIndent (cgroup "{ " ", " "}"
          ( [T.template "_$1 :: $2" [hname fname,hftype ftype] | (fname,ftype,Required) <- args]
            <>
            [T.template "_$1 :: $2" [hname fname,optionalType ftype fmode] | (fname,ftype,fmode) <- args,  isOptional fmode]
          )
          <> if deriveInstances then cline "deriving (Eq)" else mempty
         )
      )
    lenses = mconcat [
         ctemplate "-- $3 :: Lens' $1Params $2" [htypename,optionalType ftype optional,hname fname]
      <> ctemplate "$3 :: Functor f => ($2 -> f ($2)) -> $1Params -> f $1Params" [htypename,optionalType ftype optional,hname fname]
      <> ctemplate "$3 k atom = fmap (\\new$3 -> atom { _$3 = new$3 }) (k (_$3 atom))" [htypename,optionalType ftype optional,hname fname]
      | (fname,ftype,optional) <- args
      ]
    makeParams
      =  ctemplate
           "make$1Params :: $2 $1Params"
           [ htypename
           , T.intercalate " " [hftype ftype <> " ->" | (_,ftype,Required) <- args]
           ]
      <> ctemplate
           "make$1Params $2 = $1Params"
           [ htypename
           , T.intercalate " " [hfnname fname | (fname,_,Required) <- args]
           ]
      <> CIndent (cgroup "{ " ", " "}"
          ( [T.template "_$1 = $2" [hname fname,hfnname fname] | (fname,ftype,Required) <- args]
            <>
            [T.template "_$1 = $2" [hname fname, optionalDefault fmode] | (fname,ftype,fmode) <- args,  isOptional fmode]
          )
         )
    toResourceInstance
      =  (ctemplate "instance ToResourceFieldMap $1Params where" [htypename])
      <> CIndent
         (cline "toResourceFieldMap params"
         <> (CIndent (cgroup "=  " "<> " ""  (map createValue args)))
         )
      <> cblank
      <> (ctemplate "instance ToResourceField $1Params where" [htypename])
      <> CIndent
         (cline "toResourceField = RF_Map . toResourceFieldMap "
         )

    createValue (fname,ftype,Required) =
      T.template "rfmField \"$1\" (_$2 params)" [dequote fname, hname fname]
    createValue (fname,ftype,Optional) =
      T.template "rfmOptionalField \"$1\" (_$2 params)" [dequote fname, hname fname]
    createValue (fname,ftype,OptionalWithDefault defv) =
      T.template "rfmOptionalDefField \"$1\" $2 (_$3 params)" [dequote fname, defv, hname fname]
    createValue (fname,ftype,ExpandedList) =
      T.template "rfmExpandedList \"$1\" (_$2 params)" [dequote fname, hname fname]

    dequote = T.takeWhile (/= '\'')

    hname n = fieldprefix <> "_" <> n

resourceCode :: T.Text -> T.Text -> T.Text -> [(T.Text, FieldType, FieldMode)] -> [(T.Text, FieldType)] -> Code
resourceCode tftypename fieldprefix docurl args attrs
  =  mconcat (intersperse cblank [function,function',value,isResourceInstance,argsTypes])
  where
    function
      =  ctemplate "-- | Add a resource of type $1 to the resource graph." [htypename tftypename]
      <> cline     "--"
      <> ctemplate "-- See the terraform <$1 $2> documentation" [docurl, tftypename]
      <> cline     "-- for details."
      <> ctemplate "-- (In this binding attribute and argument names all have the prefix '$1_')" [fieldprefix]
      <> cline     ""
      <> ctemplate
           "$1 :: NameElement -> $2($3Params -> $3Params) -> TF $3"
           [ hfnname tftypename
           , T.intercalate " " [hftype ftype <> " ->" | (_,ftype,Required) <- args]
           , htypename tftypename
           ]
      <> ctemplate
           "$1 name0 $2 modf = new$3 name0 (modf (make$3Params $2))"
           [ hfnname tftypename
           , T.intercalate " " [hfnname fname | (fname,_,Required) <- args]
           , htypename tftypename
           ]
      <> cline     ""
      <> ctemplate
           "$1' :: NameElement -> $2 TF $3"
           [ hfnname tftypename
           , T.intercalate " " [hftype ftype <> " ->" | (_,ftype,Required) <- args]
           , htypename tftypename
           ]
      <> ctemplate
           "$1' name0 $2 = new$3 name0 (make$3Params $2)"
           [ hfnname tftypename
           , T.intercalate " " [hfnname fname | (fname,_,Required) <- args]
           , htypename tftypename
           ]

    function'
      =  ctemplate "new$1 :: NameElement -> $1Params -> TF $1" [htypename tftypename]
      <> ctemplate "new$1 name0 params = do" [htypename tftypename]
      <> CIndent
        (  ctemplate "rid <- mkResource \"$1\" name0 (toResourceFieldMap params)" [tftypename]
        <> ctemplate "return $1" [htypename tftypename]
        <> CIndent (cgroup "{ " ", " "}" attrValues)
        )

    attrValues
      =  [T.template "$1 = resourceAttr rid \"$2\"" [hname fname, fname] | (fname,_) <- attrs]
      <> [T.template "$1_resource = rid" [fieldprefix]]

    argsTypes = fieldsCode (htypename tftypename) fieldprefix False args

    value =
      ( ctemplate "data $1 = $1" [htypename tftypename]
      <> CIndent (cgroup "{ " ", " "}"
          (  [T.template "$1 :: $2" [hname fname,hftype ftype] | (fname,ftype) <- attrs]
          <> [T.template "$1_resource :: ResourceId" [fieldprefix]]
          )
        )
      )

    isResourceInstance =
      ctemplate "instance IsResource $1 where" [htypename tftypename]
      <> CIndent (ctemplate "resourceId = $1_resource" [fieldprefix])

    hname n = fieldprefix <> "_" <> n

hfnname tftype = unreserve (T.toLower c1 <> cs)
  where
    (c1,cs) = T.splitAt 1 (htypename tftype)
    unreserve n = if S.member n reserved then n <> "_" else n
    reserved = S.fromList ["type","data","instance"]

htypename tftype = T.concat (map T.toTitle (T.splitOn "_" tftype))

isOptional Optional = True
isOptional (OptionalWithDefault _)  = True
isOptional ExpandedList  = True
isOptional _ = False

optionalType ftype Optional = T.template "Maybe ($1)" [hftype ftype]
optionalType ftype _ = hftype ftype

optionalDefault  Required = "??"
optionalDefault  ExpandedList = "[]"
optionalDefault  Optional = "Nothing"
optionalDefault (OptionalWithDefault def) = def

hftype (NamedType t) = t
hftype (TFRef t)
  | T.isInfixOf " " t = T.template "TFRef ($1)" [t]
  | otherwise         = T.template "TFRef $1" [t]
hftype (AwsIdRef t) = T.template "TFRef (AwsId $1)" [htypename t]
hftype (FTList t) = "[" <> hftype t <> "]"
hftype TagsMap = "M.Map T.Text T.Text"


generateModule :: FilePath -> T.Text -> Code -> [Code] -> IO ()
generateModule outdir moduleName header resources = T.writeFile filepath (T.intercalate "\n" (codeText code))
  where
    filepath = outdir </> (T.unpack moduleName <> ".hs")

    code = header0 <> cblank <> header <> csection <> mconcat (intersperse csection resources)

    csection = cblank <> cline (T.replicate 70 "-")  <> cblank
    header0 = clines
      [ "{-# LANGUAGE OverloadedStrings #-}"
      , "-- | Terraform resource definitions"
      , "--"
      , "-- This file is auto-generated. Change it by changing the script"
      , "-- that generates it."
      , "--"
      , "-- There are two variants of each function to construct a resource"
      , "-- (eg 'awsVpc' and 'awsVpc'') . The former takes the required attributes"
      , "-- as positional paramemeters. The latter (with the quote suffixed name)"
      , "-- takes a record containing all attributes. This can be more convenient"
      , "-- when there are many required arguments."
      , "--"
      , T.template "module Language.Terraform.$1 where" [moduleName]
      , ""
      , "import qualified Data.Map as M"
      , "import qualified Data.Text as T"
      , "import Data.Maybe(catMaybes)"
      , "import Data.Monoid"
      , "import Language.Terraform.Core"
      ]

generate :: FilePath -> IO ()
generate outdir = do
  generateModule outdir "Aws" awsHeader awsResources


main :: IO ()
main = generate "src/Language/Terraform"
