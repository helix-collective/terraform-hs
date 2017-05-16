{-# LANGUAGE OverloadedStrings #-}
-- | Terraform resource definitions
--
-- This file is auto-generated. Change it by changing the script
-- that generates it.
--
-- There are two variants of each function to construct a resource
-- (eg 'awsVpc' and 'awsVpc'') . The former takes the required attributes
-- as positional paramemeters. The latter (with the quote suffixed name)
-- takes a record containing all attributes. This can be more convenient
-- when there are many required arguments.
--
module Language.Terraform.Aws where

import qualified Data.Map as M
import qualified Data.Text as T

import Data.Default 
import Data.Maybe(catMaybes)
import Data.Monoid
import Language.Terraform.Core

type AwsRegion = T.Text
data AwsId a = AwsId
type CidrBlock = T.Text
type AvailabilityZone = T.Text
type Ami = T.Text
type InstanceType = T.Text
type KeyName = T.Text
type S3BucketName = T.Text
type S3Key = T.Text
type Arn = T.Text
newtype IpAddress = IpAddress T.Text
type VolumeType = T.Text
type CannedAcl = T.Text
type MetricComparisonOperator = T.Text
type MetricNamespace = T.Text
type MetricName = T.Text
type MetricStatistic = T.Text
type MetricUnit = T.Text
type DBEngine = T.Text
type DBInstanceClass = T.Text
type HostedZoneId = T.Text
type Route53RecordType = T.Text

-- | Add an aws provider to the resource graph.
--
-- See the original <https://www.terraform.io/docs/providers/aws/index.html terraform documentation>
-- for details.

aws :: AwsParams -> TF ()
aws params =
  mkProvider "aws" $ catMaybes
    [ Just ("region", toResourceField (aws_region params))
    , let v = aws_access_key (aws_options params) in if v == "" then Nothing else (Just ("access_key", toResourceField v))
    , let v = aws_secret_key (aws_options params) in if v == "" then Nothing else (Just ("secret_key", toResourceField v))
    ]

data AwsParams = AwsParams
  { aws_region :: AwsRegion
  , aws_options :: AwsOptions
  }

data AwsOptions = AwsOptions
  { aws_access_key :: T.Text
  , aws_secret_key :: T.Text
  }

instance Default AwsOptions where
  def = AwsOptions "" ""

----------------------------------------------------------------------

-- | Add a resource of type AwsVpc to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/d/vpc.html aws_vpc> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'vpc_')

awsVpc :: NameElement -> CidrBlock -> AwsVpcOptions -> TF AwsVpc
awsVpc name0 cidrBlock opts = awsVpc' name0 (AwsVpcParams cidrBlock opts)

awsVpc' :: NameElement -> AwsVpcParams -> TF AwsVpc
awsVpc' name0 params = do
  rid <- mkResource "aws_vpc" name0 (toResourceFieldMap params)
  return AwsVpc
    { vpc_id = resourceAttr rid "id"
    , vpc_resource = rid
    }

data AwsVpcParams = AwsVpcParams
  { vpc_cidr_block :: CidrBlock
  , vpc_options :: AwsVpcOptions
  }

data AwsVpcOptions = AwsVpcOptions
  { vpc_instance_tenancy :: Maybe (T.Text)
  , vpc_enable_dns_support :: Bool
  , vpc_enable_dns_hostnames :: Bool
  , vpc_enable_classic_link :: Bool
  , vpc_tags :: M.Map T.Text T.Text
  }

instance Default AwsVpcOptions where
  def = AwsVpcOptions Nothing True False False M.empty

instance ToResourceFieldMap AwsVpcParams where
  toResourceFieldMap params
    =  rfmField "cidr_block" (vpc_cidr_block params)
    <> rfmOptionalField "instance_tenancy" (vpc_instance_tenancy (vpc_options params))
    <> rfmOptionalDefField "enable_dns_support" True (vpc_enable_dns_support (vpc_options params))
    <> rfmOptionalDefField "enable_dns_hostnames" False (vpc_enable_dns_hostnames (vpc_options params))
    <> rfmOptionalDefField "enable_classic_link" False (vpc_enable_classic_link (vpc_options params))
    <> rfmOptionalDefField "tags" M.empty (vpc_tags (vpc_options params))
    

instance ToResourceField AwsVpcParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsVpc = AwsVpc
  { vpc_id :: TFRef (AwsId AwsVpc)
  , vpc_resource :: ResourceId
  }

instance IsResource AwsVpc where
  resourceId = vpc_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsNatGateway to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/nat_gateway.html aws_nat_gateway> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'ng_')

awsNatGateway :: NameElement -> TFRef (AwsId AwsEip) -> TFRef (AwsId AwsSubnet) -> AwsNatGatewayOptions -> TF AwsNatGateway
awsNatGateway name0 allocationId subnetId opts = awsNatGateway' name0 (AwsNatGatewayParams allocationId subnetId opts)

awsNatGateway' :: NameElement -> AwsNatGatewayParams -> TF AwsNatGateway
awsNatGateway' name0 params = do
  rid <- mkResource "aws_nat_gateway" name0 (toResourceFieldMap params)
  return AwsNatGateway
    { ng_id = resourceAttr rid "id"
    , ng_resource = rid
    }

data AwsNatGatewayParams = AwsNatGatewayParams
  { ng_allocation_id :: TFRef (AwsId AwsEip)
  , ng_subnet_id :: TFRef (AwsId AwsSubnet)
  , ng_options :: AwsNatGatewayOptions
  }

data AwsNatGatewayOptions = AwsNatGatewayOptions
  { }

instance Default AwsNatGatewayOptions where
  def = AwsNatGatewayOptions 

instance ToResourceFieldMap AwsNatGatewayParams where
  toResourceFieldMap params
    =  rfmField "allocation_id" (ng_allocation_id params)
    <> rfmField "subnet_id" (ng_subnet_id params)
    

instance ToResourceField AwsNatGatewayParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsNatGateway = AwsNatGateway
  { ng_id :: TFRef (AwsId AwsNatGateway)
  , ng_resource :: ResourceId
  }

instance IsResource AwsNatGateway where
  resourceId = ng_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsInternetGateway to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/internet_gateway.html aws_internet_gateway> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'ig_')

awsInternetGateway :: NameElement -> TFRef (AwsId AwsVpc) -> AwsInternetGatewayOptions -> TF AwsInternetGateway
awsInternetGateway name0 vpcId opts = awsInternetGateway' name0 (AwsInternetGatewayParams vpcId opts)

awsInternetGateway' :: NameElement -> AwsInternetGatewayParams -> TF AwsInternetGateway
awsInternetGateway' name0 params = do
  rid <- mkResource "aws_internet_gateway" name0 (toResourceFieldMap params)
  return AwsInternetGateway
    { ig_id = resourceAttr rid "id"
    , ig_resource = rid
    }

data AwsInternetGatewayParams = AwsInternetGatewayParams
  { ig_vpc_id :: TFRef (AwsId AwsVpc)
  , ig_options :: AwsInternetGatewayOptions
  }

data AwsInternetGatewayOptions = AwsInternetGatewayOptions
  { ig_tags :: M.Map T.Text T.Text
  }

instance Default AwsInternetGatewayOptions where
  def = AwsInternetGatewayOptions M.empty

instance ToResourceFieldMap AwsInternetGatewayParams where
  toResourceFieldMap params
    =  rfmField "vpc_id" (ig_vpc_id params)
    <> rfmOptionalDefField "tags" M.empty (ig_tags (ig_options params))
    

instance ToResourceField AwsInternetGatewayParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsInternetGateway = AwsInternetGateway
  { ig_id :: TFRef (AwsId AwsInternetGateway)
  , ig_resource :: ResourceId
  }

instance IsResource AwsInternetGateway where
  resourceId = ig_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsSubnet to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/d/subnet.html aws_subnet> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'sn_')

awsSubnet :: NameElement -> TFRef (AwsId AwsVpc) -> CidrBlock -> AwsSubnetOptions -> TF AwsSubnet
awsSubnet name0 vpcId cidrBlock opts = awsSubnet' name0 (AwsSubnetParams vpcId cidrBlock opts)

awsSubnet' :: NameElement -> AwsSubnetParams -> TF AwsSubnet
awsSubnet' name0 params = do
  rid <- mkResource "aws_subnet" name0 (toResourceFieldMap params)
  return AwsSubnet
    { sn_id = resourceAttr rid "id"
    , sn_resource = rid
    }

data AwsSubnetParams = AwsSubnetParams
  { sn_vpc_id :: TFRef (AwsId AwsVpc)
  , sn_cidr_block :: CidrBlock
  , sn_options :: AwsSubnetOptions
  }

data AwsSubnetOptions = AwsSubnetOptions
  { sn_map_public_ip_on_launch :: Bool
  , sn_availability_zone :: AvailabilityZone
  , sn_tags :: M.Map T.Text T.Text
  }

instance Default AwsSubnetOptions where
  def = AwsSubnetOptions False "" M.empty

instance ToResourceFieldMap AwsSubnetParams where
  toResourceFieldMap params
    =  rfmField "vpc_id" (sn_vpc_id params)
    <> rfmField "cidr_block" (sn_cidr_block params)
    <> rfmOptionalDefField "map_public_ip_on_launch" False (sn_map_public_ip_on_launch (sn_options params))
    <> rfmOptionalDefField "availability_zone" "" (sn_availability_zone (sn_options params))
    <> rfmOptionalDefField "tags" M.empty (sn_tags (sn_options params))
    

instance ToResourceField AwsSubnetParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsSubnet = AwsSubnet
  { sn_id :: TFRef (AwsId AwsSubnet)
  , sn_resource :: ResourceId
  }

instance IsResource AwsSubnet where
  resourceId = sn_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsRouteTable to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/route_table.html aws_route_table> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'rt_')

awsRouteTable :: NameElement -> TFRef (AwsId AwsVpc) -> AwsRouteTableOptions -> TF AwsRouteTable
awsRouteTable name0 vpcId opts = awsRouteTable' name0 (AwsRouteTableParams vpcId opts)

awsRouteTable' :: NameElement -> AwsRouteTableParams -> TF AwsRouteTable
awsRouteTable' name0 params = do
  rid <- mkResource "aws_route_table" name0 (toResourceFieldMap params)
  return AwsRouteTable
    { rt_id = resourceAttr rid "id"
    , rt_resource = rid
    }

data AwsRouteTableParams = AwsRouteTableParams
  { rt_vpc_id :: TFRef (AwsId AwsVpc)
  , rt_options :: AwsRouteTableOptions
  }

data AwsRouteTableOptions = AwsRouteTableOptions
  { rt_tags :: M.Map T.Text T.Text
  }

instance Default AwsRouteTableOptions where
  def = AwsRouteTableOptions M.empty

instance ToResourceFieldMap AwsRouteTableParams where
  toResourceFieldMap params
    =  rfmField "vpc_id" (rt_vpc_id params)
    <> rfmOptionalDefField "tags" M.empty (rt_tags (rt_options params))
    

instance ToResourceField AwsRouteTableParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsRouteTable = AwsRouteTable
  { rt_id :: TFRef (AwsId AwsRouteTable)
  , rt_resource :: ResourceId
  }

instance IsResource AwsRouteTable where
  resourceId = rt_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsRoute to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/route.html aws_route> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'r_')

awsRoute :: NameElement -> TFRef (AwsId AwsRouteTable) -> CidrBlock -> AwsRouteOptions -> TF AwsRoute
awsRoute name0 routeTableId destinationCidrBlock opts = awsRoute' name0 (AwsRouteParams routeTableId destinationCidrBlock opts)

awsRoute' :: NameElement -> AwsRouteParams -> TF AwsRoute
awsRoute' name0 params = do
  rid <- mkResource "aws_route" name0 (toResourceFieldMap params)
  return AwsRoute
    { r_resource = rid
    }

data AwsRouteParams = AwsRouteParams
  { r_route_table_id :: TFRef (AwsId AwsRouteTable)
  , r_destination_cidr_block :: CidrBlock
  , r_options :: AwsRouteOptions
  }

data AwsRouteOptions = AwsRouteOptions
  { r_nat_gateway_id :: Maybe (TFRef (AwsId AwsNatGateway))
  , r_gateway_id :: Maybe (TFRef (AwsId AwsInternetGateway))
  }

instance Default AwsRouteOptions where
  def = AwsRouteOptions Nothing Nothing

instance ToResourceFieldMap AwsRouteParams where
  toResourceFieldMap params
    =  rfmField "route_table_id" (r_route_table_id params)
    <> rfmField "destination_cidr_block" (r_destination_cidr_block params)
    <> rfmOptionalField "nat_gateway_id" (r_nat_gateway_id (r_options params))
    <> rfmOptionalField "gateway_id" (r_gateway_id (r_options params))
    

instance ToResourceField AwsRouteParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsRoute = AwsRoute
  { r_resource :: ResourceId
  }

instance IsResource AwsRoute where
  resourceId = r_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsRouteTableAssociation to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/route_table_association.html aws_route_table_association> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'rta_')

awsRouteTableAssociation :: NameElement -> TFRef (AwsId AwsSubnet) -> TFRef (AwsId AwsRouteTable) -> AwsRouteTableAssociationOptions -> TF AwsRouteTableAssociation
awsRouteTableAssociation name0 subnetId routeTableId opts = awsRouteTableAssociation' name0 (AwsRouteTableAssociationParams subnetId routeTableId opts)

awsRouteTableAssociation' :: NameElement -> AwsRouteTableAssociationParams -> TF AwsRouteTableAssociation
awsRouteTableAssociation' name0 params = do
  rid <- mkResource "aws_route_table_association" name0 (toResourceFieldMap params)
  return AwsRouteTableAssociation
    { rta_id = resourceAttr rid "id"
    , rta_resource = rid
    }

data AwsRouteTableAssociationParams = AwsRouteTableAssociationParams
  { rta_subnet_id :: TFRef (AwsId AwsSubnet)
  , rta_route_table_id :: TFRef (AwsId AwsRouteTable)
  , rta_options :: AwsRouteTableAssociationOptions
  }

data AwsRouteTableAssociationOptions = AwsRouteTableAssociationOptions
  { }

instance Default AwsRouteTableAssociationOptions where
  def = AwsRouteTableAssociationOptions 

instance ToResourceFieldMap AwsRouteTableAssociationParams where
  toResourceFieldMap params
    =  rfmField "subnet_id" (rta_subnet_id params)
    <> rfmField "route_table_id" (rta_route_table_id params)
    

instance ToResourceField AwsRouteTableAssociationParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsRouteTableAssociation = AwsRouteTableAssociation
  { rta_id :: TFRef (AwsId AwsRouteTableAssociation)
  , rta_resource :: ResourceId
  }

instance IsResource AwsRouteTableAssociation where
  resourceId = rta_resource

----------------------------------------------------------------------

data IngressRuleParams = IngressRuleParams
  { ir_from_port :: Int
  , ir_to_port :: Int
  , ir_protocol :: T.Text
  , ir_options :: IngressRuleOptions
  }
  deriving (Eq)

data IngressRuleOptions = IngressRuleOptions
  { ir_cidr_blocks :: [CidrBlock]
  }
  deriving (Eq)

instance Default IngressRuleOptions where
  def = IngressRuleOptions []

instance ToResourceFieldMap IngressRuleParams where
  toResourceFieldMap params
    =  rfmField "from_port" (ir_from_port params)
    <> rfmField "to_port" (ir_to_port params)
    <> rfmField "protocol" (ir_protocol params)
    <> rfmOptionalDefField "cidr_blocks" [] (ir_cidr_blocks (ir_options params))
    

instance ToResourceField IngressRuleParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data EgressRuleParams = EgressRuleParams
  { er_from_port :: Int
  , er_to_port :: Int
  , er_protocol :: T.Text
  , er_options :: EgressRuleOptions
  }
  deriving (Eq)

data EgressRuleOptions = EgressRuleOptions
  { er_cidr_blocks :: [CidrBlock]
  }
  deriving (Eq)

instance Default EgressRuleOptions where
  def = EgressRuleOptions []

instance ToResourceFieldMap EgressRuleParams where
  toResourceFieldMap params
    =  rfmField "from_port" (er_from_port params)
    <> rfmField "to_port" (er_to_port params)
    <> rfmField "protocol" (er_protocol params)
    <> rfmOptionalDefField "cidr_blocks" [] (er_cidr_blocks (er_options params))
    

instance ToResourceField EgressRuleParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsSecurityGroup to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/security_group.html aws_security_group> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'sg_')

awsSecurityGroup :: NameElement ->  AwsSecurityGroupOptions -> TF AwsSecurityGroup
awsSecurityGroup name0  opts = awsSecurityGroup' name0 (AwsSecurityGroupParams  opts)

awsSecurityGroup' :: NameElement -> AwsSecurityGroupParams -> TF AwsSecurityGroup
awsSecurityGroup' name0 params = do
  rid <- mkResource "aws_security_group" name0 (toResourceFieldMap params)
  return AwsSecurityGroup
    { sg_id = resourceAttr rid "id"
    , sg_owner_id = resourceAttr rid "owner_id"
    , sg_resource = rid
    }

data AwsSecurityGroupParams = AwsSecurityGroupParams
  { sg_options :: AwsSecurityGroupOptions
  }

data AwsSecurityGroupOptions = AwsSecurityGroupOptions
  { sg_name :: T.Text
  , sg_name_prefix :: T.Text
  , sg_description :: T.Text
  , sg_ingress :: [IngressRuleParams]
  , sg_egress :: [EgressRuleParams]
  , sg_vpc_id :: Maybe (TFRef (AwsId AwsVpc))
  , sg_tags :: M.Map T.Text T.Text
  }

instance Default AwsSecurityGroupOptions where
  def = AwsSecurityGroupOptions "" "" "" [] [] Nothing M.empty

instance ToResourceFieldMap AwsSecurityGroupParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "name" "" (sg_name (sg_options params))
    <> rfmOptionalDefField "name_prefix" "" (sg_name_prefix (sg_options params))
    <> rfmOptionalDefField "description" "" (sg_description (sg_options params))
    <> rfmOptionalDefField "ingress" [] (sg_ingress (sg_options params))
    <> rfmOptionalDefField "egress" [] (sg_egress (sg_options params))
    <> rfmOptionalField "vpc_id" (sg_vpc_id (sg_options params))
    <> rfmOptionalDefField "tags" M.empty (sg_tags (sg_options params))
    

instance ToResourceField AwsSecurityGroupParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsSecurityGroup = AwsSecurityGroup
  { sg_id :: TFRef (AwsId AwsSecurityGroup)
  , sg_owner_id :: TFRef T.Text
  , sg_resource :: ResourceId
  }

instance IsResource AwsSecurityGroup where
  resourceId = sg_resource

----------------------------------------------------------------------

data RootBlockDeviceParams = RootBlockDeviceParams
  { rbd_options :: RootBlockDeviceOptions
  }
  deriving (Eq)

data RootBlockDeviceOptions = RootBlockDeviceOptions
  { rbd_volume_type :: VolumeType
  , rbd_volume_size :: Maybe (Int)
  , rbd_delete_on_termination :: Bool
  }
  deriving (Eq)

instance Default RootBlockDeviceOptions where
  def = RootBlockDeviceOptions "standard" Nothing True

instance ToResourceFieldMap RootBlockDeviceParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "volume_type" "standard" (rbd_volume_type (rbd_options params))
    <> rfmOptionalField "volume_size" (rbd_volume_size (rbd_options params))
    <> rfmOptionalDefField "delete_on_termination" True (rbd_delete_on_termination (rbd_options params))
    

instance ToResourceField RootBlockDeviceParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsInstance to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/instance.html aws_instance> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'i_')

awsInstance :: NameElement -> Ami -> InstanceType -> AwsInstanceOptions -> TF AwsInstance
awsInstance name0 ami instanceType opts = awsInstance' name0 (AwsInstanceParams ami instanceType opts)

awsInstance' :: NameElement -> AwsInstanceParams -> TF AwsInstance
awsInstance' name0 params = do
  rid <- mkResource "aws_instance" name0 (toResourceFieldMap params)
  return AwsInstance
    { i_id = resourceAttr rid "id"
    , i_public_ip = resourceAttr rid "public_ip"
    , i_private_ip = resourceAttr rid "private_ip"
    , i_resource = rid
    }

data AwsInstanceParams = AwsInstanceParams
  { i_ami :: Ami
  , i_instance_type :: InstanceType
  , i_options :: AwsInstanceOptions
  }

data AwsInstanceOptions = AwsInstanceOptions
  { i_availability_zone :: AvailabilityZone
  , i_ebs_optimized :: Maybe (Bool)
  , i_key_name :: Maybe (KeyName)
  , i_monitoring :: Bool
  , i_subnet_id :: Maybe (TFRef (AwsId AwsSubnet))
  , i_associate_public_ip_address :: Maybe (Bool)
  , i_root_block_device :: Maybe (RootBlockDeviceParams)
  , i_user_data :: T.Text
  , i_iam_instance_profile :: Maybe (TFRef (AwsId AwsIamInstanceProfile))
  , i_vpc_security_group_ids :: [TFRef (AwsId AwsSecurityGroup)]
  , i_tags :: M.Map T.Text T.Text
  }

instance Default AwsInstanceOptions where
  def = AwsInstanceOptions "" Nothing Nothing True Nothing Nothing Nothing "" Nothing [] M.empty

instance ToResourceFieldMap AwsInstanceParams where
  toResourceFieldMap params
    =  rfmField "ami" (i_ami params)
    <> rfmOptionalDefField "availability_zone" "" (i_availability_zone (i_options params))
    <> rfmOptionalField "ebs_optimized" (i_ebs_optimized (i_options params))
    <> rfmField "instance_type" (i_instance_type params)
    <> rfmOptionalField "key_name" (i_key_name (i_options params))
    <> rfmOptionalDefField "monitoring" True (i_monitoring (i_options params))
    <> rfmOptionalField "subnet_id" (i_subnet_id (i_options params))
    <> rfmOptionalField "associate_public_ip_address" (i_associate_public_ip_address (i_options params))
    <> rfmOptionalField "root_block_device" (i_root_block_device (i_options params))
    <> rfmOptionalDefField "user_data" "" (i_user_data (i_options params))
    <> rfmOptionalField "iam_instance_profile" (i_iam_instance_profile (i_options params))
    <> rfmOptionalDefField "vpc_security_group_ids" [] (i_vpc_security_group_ids (i_options params))
    <> rfmOptionalDefField "tags" M.empty (i_tags (i_options params))
    

instance ToResourceField AwsInstanceParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsInstance = AwsInstance
  { i_id :: TFRef (AwsId AwsInstance)
  , i_public_ip :: TFRef IpAddress
  , i_private_ip :: TFRef IpAddress
  , i_resource :: ResourceId
  }

instance IsResource AwsInstance where
  resourceId = i_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsLaunchConfiguration to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/launch_configuration.html aws_launch_configuration> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'lc_')

awsLaunchConfiguration :: NameElement -> Ami -> InstanceType -> AwsLaunchConfigurationOptions -> TF AwsLaunchConfiguration
awsLaunchConfiguration name0 imageId instanceType opts = awsLaunchConfiguration' name0 (AwsLaunchConfigurationParams imageId instanceType opts)

awsLaunchConfiguration' :: NameElement -> AwsLaunchConfigurationParams -> TF AwsLaunchConfiguration
awsLaunchConfiguration' name0 params = do
  rid <- mkResource "aws_launch_configuration" name0 (toResourceFieldMap params)
  return AwsLaunchConfiguration
    { lc_id = resourceAttr rid "id"
    , lc_name = resourceAttr rid "name"
    , lc_resource = rid
    }

data AwsLaunchConfigurationParams = AwsLaunchConfigurationParams
  { lc_image_id :: Ami
  , lc_instance_type :: InstanceType
  , lc_options :: AwsLaunchConfigurationOptions
  }

data AwsLaunchConfigurationOptions = AwsLaunchConfigurationOptions
  { lc_name' :: T.Text
  , lc_name_prefix :: T.Text
  , lc_iam_instance_profile :: Maybe (TFRef (AwsId AwsIamInstanceProfile))
  , lc_key_name :: Maybe (KeyName)
  , lc_security_groups :: [TFRef (AwsId AwsSecurityGroup)]
  , lc_associate_public_ip_address :: Maybe (Bool)
  , lc_user_data :: T.Text
  , lc_enable_monitoring :: Bool
  , lc_ebs_optimized :: Maybe (Bool)
  , lc_root_block_device :: Maybe (RootBlockDeviceParams)
  }

instance Default AwsLaunchConfigurationOptions where
  def = AwsLaunchConfigurationOptions "" "" Nothing Nothing [] Nothing "" True Nothing Nothing

instance ToResourceFieldMap AwsLaunchConfigurationParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "name" "" (lc_name' (lc_options params))
    <> rfmOptionalDefField "name_prefix" "" (lc_name_prefix (lc_options params))
    <> rfmField "image_id" (lc_image_id params)
    <> rfmField "instance_type" (lc_instance_type params)
    <> rfmOptionalField "iam_instance_profile" (lc_iam_instance_profile (lc_options params))
    <> rfmOptionalField "key_name" (lc_key_name (lc_options params))
    <> rfmOptionalDefField "security_groups" [] (lc_security_groups (lc_options params))
    <> rfmOptionalField "associate_public_ip_address" (lc_associate_public_ip_address (lc_options params))
    <> rfmOptionalDefField "user_data" "" (lc_user_data (lc_options params))
    <> rfmOptionalDefField "enable_monitoring" True (lc_enable_monitoring (lc_options params))
    <> rfmOptionalField "ebs_optimized" (lc_ebs_optimized (lc_options params))
    <> rfmOptionalField "root_block_device" (lc_root_block_device (lc_options params))
    

instance ToResourceField AwsLaunchConfigurationParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsLaunchConfiguration = AwsLaunchConfiguration
  { lc_id :: TFRef (AwsId AwsLaunchConfiguration)
  , lc_name :: TFRef T.Text
  , lc_resource :: ResourceId
  }

instance IsResource AwsLaunchConfiguration where
  resourceId = lc_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsAutoscalingGroup to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/autoscaling_group.html aws_autoscaling_group> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'ag_')

awsAutoscalingGroup :: NameElement -> Int -> Int -> TFRef T.Text -> AwsAutoscalingGroupOptions -> TF AwsAutoscalingGroup
awsAutoscalingGroup name0 maxSize minSize launchConfiguration opts = awsAutoscalingGroup' name0 (AwsAutoscalingGroupParams maxSize minSize launchConfiguration opts)

awsAutoscalingGroup' :: NameElement -> AwsAutoscalingGroupParams -> TF AwsAutoscalingGroup
awsAutoscalingGroup' name0 params = do
  rid <- mkResource "aws_autoscaling_group" name0 (toResourceFieldMap params)
  return AwsAutoscalingGroup
    { ag_id = resourceAttr rid "id"
    , ag_arn = resourceAttr rid "arn"
    , ag_name = resourceAttr rid "name"
    , ag_resource = rid
    }

data AwsAutoscalingGroupParams = AwsAutoscalingGroupParams
  { ag_max_size :: Int
  , ag_min_size :: Int
  , ag_launch_configuration :: TFRef T.Text
  , ag_options :: AwsAutoscalingGroupOptions
  }

data AwsAutoscalingGroupOptions = AwsAutoscalingGroupOptions
  { ag_name' :: T.Text
  , ag_name_prefix :: T.Text
  , ag_vpc_zone_identifier :: [TFRef (AwsId AwsSubnet)]
  , ag_load_balancers :: [TFRef T.Text]
  , ag_tag :: [AsgTagParams]
  }

instance Default AwsAutoscalingGroupOptions where
  def = AwsAutoscalingGroupOptions "" "" [] [] []

instance ToResourceFieldMap AwsAutoscalingGroupParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "name" "" (ag_name' (ag_options params))
    <> rfmOptionalDefField "name_prefix" "" (ag_name_prefix (ag_options params))
    <> rfmField "max_size" (ag_max_size params)
    <> rfmField "min_size" (ag_min_size params)
    <> rfmOptionalDefField "vpc_zone_identifier" [] (ag_vpc_zone_identifier (ag_options params))
    <> rfmField "launch_configuration" (ag_launch_configuration params)
    <> rfmOptionalDefField "load_balancers" [] (ag_load_balancers (ag_options params))
    <> rfmExpandedList "tag" (ag_tag (ag_options params))
    

instance ToResourceField AwsAutoscalingGroupParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsAutoscalingGroup = AwsAutoscalingGroup
  { ag_id :: TFRef (AwsId AwsAutoscalingGroup)
  , ag_arn :: TFRef Arn
  , ag_name :: TFRef T.Text
  , ag_resource :: ResourceId
  }

instance IsResource AwsAutoscalingGroup where
  resourceId = ag_resource

----------------------------------------------------------------------

data AsgTagParams = AsgTagParams
  { asg_key :: T.Text
  , asg_value :: T.Text
  , asg_propagate_at_launch :: Bool
  , asg_options :: AsgTagOptions
  }
  deriving (Eq)

data AsgTagOptions = AsgTagOptions
  { }
  deriving (Eq)

instance Default AsgTagOptions where
  def = AsgTagOptions 

instance ToResourceFieldMap AsgTagParams where
  toResourceFieldMap params
    =  rfmField "key" (asg_key params)
    <> rfmField "value" (asg_value params)
    <> rfmField "propagate_at_launch" (asg_propagate_at_launch params)
    

instance ToResourceField AsgTagParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsEip to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/eip.html aws_eip> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'eip_')

awsEip :: NameElement ->  AwsEipOptions -> TF AwsEip
awsEip name0  opts = awsEip' name0 (AwsEipParams  opts)

awsEip' :: NameElement -> AwsEipParams -> TF AwsEip
awsEip' name0 params = do
  rid <- mkResource "aws_eip" name0 (toResourceFieldMap params)
  return AwsEip
    { eip_id = resourceAttr rid "id"
    , eip_private_ip = resourceAttr rid "private_ip"
    , eip_public_ip = resourceAttr rid "public_ip"
    , eip_resource = rid
    }

data AwsEipParams = AwsEipParams
  { eip_options :: AwsEipOptions
  }

data AwsEipOptions = AwsEipOptions
  { eip_vpc :: Bool
  , eip_instance :: Maybe (TFRef (AwsId AwsInstance))
  }

instance Default AwsEipOptions where
  def = AwsEipOptions False Nothing

instance ToResourceFieldMap AwsEipParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "vpc" False (eip_vpc (eip_options params))
    <> rfmOptionalField "instance" (eip_instance (eip_options params))
    

instance ToResourceField AwsEipParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsEip = AwsEip
  { eip_id :: TFRef (AwsId AwsEip)
  , eip_private_ip :: TFRef IpAddress
  , eip_public_ip :: TFRef IpAddress
  , eip_resource :: ResourceId
  }

instance IsResource AwsEip where
  resourceId = eip_resource

----------------------------------------------------------------------

data AccessLogsParams = AccessLogsParams
  { al_bucket :: S3BucketName
  , al_options :: AccessLogsOptions
  }
  deriving (Eq)

data AccessLogsOptions = AccessLogsOptions
  { al_bucket_prefix :: S3Key
  , al_interval :: Int
  , al_enabled :: Bool
  }
  deriving (Eq)

instance Default AccessLogsOptions where
  def = AccessLogsOptions "" 60 True

instance ToResourceFieldMap AccessLogsParams where
  toResourceFieldMap params
    =  rfmField "bucket" (al_bucket params)
    <> rfmOptionalDefField "bucket_prefix" "" (al_bucket_prefix (al_options params))
    <> rfmOptionalDefField "interval" 60 (al_interval (al_options params))
    <> rfmOptionalDefField "enabled" True (al_enabled (al_options params))
    

instance ToResourceField AccessLogsParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data ListenerParams = ListenerParams
  { l_instance_port :: Int
  , l_instance_protocol :: T.Text
  , l_lb_port :: Int
  , l_lb_protocol :: T.Text
  , l_options :: ListenerOptions
  }
  deriving (Eq)

data ListenerOptions = ListenerOptions
  { l_ssl_certificate_id :: Maybe (Arn)
  }
  deriving (Eq)

instance Default ListenerOptions where
  def = ListenerOptions Nothing

instance ToResourceFieldMap ListenerParams where
  toResourceFieldMap params
    =  rfmField "instance_port" (l_instance_port params)
    <> rfmField "instance_protocol" (l_instance_protocol params)
    <> rfmField "lb_port" (l_lb_port params)
    <> rfmField "lb_protocol" (l_lb_protocol params)
    <> rfmOptionalField "ssl_certificate_id" (l_ssl_certificate_id (l_options params))
    

instance ToResourceField ListenerParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data HealthCheckParams = HealthCheckParams
  { hc_healthy_threshold :: Int
  , hc_unhealthy_threshold :: Int
  , hc_target :: T.Text
  , hc_interval :: Int
  , hc_timeout :: Int
  , hc_options :: HealthCheckOptions
  }
  deriving (Eq)

data HealthCheckOptions = HealthCheckOptions
  { }
  deriving (Eq)

instance Default HealthCheckOptions where
  def = HealthCheckOptions 

instance ToResourceFieldMap HealthCheckParams where
  toResourceFieldMap params
    =  rfmField "healthy_threshold" (hc_healthy_threshold params)
    <> rfmField "unhealthy_threshold" (hc_unhealthy_threshold params)
    <> rfmField "target" (hc_target params)
    <> rfmField "interval" (hc_interval params)
    <> rfmField "timeout" (hc_timeout params)
    

instance ToResourceField HealthCheckParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsElb to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/elb.html aws_elb> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'elb_')

awsElb :: NameElement -> [ListenerParams] -> AwsElbOptions -> TF AwsElb
awsElb name0 listener opts = awsElb' name0 (AwsElbParams listener opts)

awsElb' :: NameElement -> AwsElbParams -> TF AwsElb
awsElb' name0 params = do
  rid <- mkResource "aws_elb" name0 (toResourceFieldMap params)
  return AwsElb
    { elb_id = resourceAttr rid "id"
    , elb_name = resourceAttr rid "name"
    , elb_dns_name = resourceAttr rid "dns_name"
    , elb_zone_id = resourceAttr rid "zone_id"
    , elb_resource = rid
    }

data AwsElbParams = AwsElbParams
  { elb_listener :: [ListenerParams]
  , elb_options :: AwsElbOptions
  }

data AwsElbOptions = AwsElbOptions
  { elb_name' :: Maybe (T.Text)
  , elb_access_logs :: Maybe (AccessLogsParams)
  , elb_security_groups :: [TFRef (AwsId AwsSecurityGroup)]
  , elb_subnets :: [TFRef (AwsId AwsSubnet)]
  , elb_instances :: [TFRef (AwsId AwsInstance)]
  , elb_health_check :: Maybe (HealthCheckParams)
  , elb_tags :: M.Map T.Text T.Text
  }

instance Default AwsElbOptions where
  def = AwsElbOptions Nothing Nothing [] [] [] Nothing M.empty

instance ToResourceFieldMap AwsElbParams where
  toResourceFieldMap params
    =  rfmOptionalField "name" (elb_name' (elb_options params))
    <> rfmOptionalField "access_logs" (elb_access_logs (elb_options params))
    <> rfmOptionalDefField "security_groups" [] (elb_security_groups (elb_options params))
    <> rfmOptionalDefField "subnets" [] (elb_subnets (elb_options params))
    <> rfmOptionalDefField "instances" [] (elb_instances (elb_options params))
    <> rfmField "listener" (elb_listener params)
    <> rfmOptionalField "health_check" (elb_health_check (elb_options params))
    <> rfmOptionalDefField "tags" M.empty (elb_tags (elb_options params))
    

instance ToResourceField AwsElbParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsElb = AwsElb
  { elb_id :: TFRef T.Text
  , elb_name :: TFRef T.Text
  , elb_dns_name :: TFRef T.Text
  , elb_zone_id :: TFRef T.Text
  , elb_resource :: ResourceId
  }

instance IsResource AwsElb where
  resourceId = elb_resource

----------------------------------------------------------------------

data BucketVersioningParams = BucketVersioningParams
  { bv_options :: BucketVersioningOptions
  }
  deriving (Eq)

data BucketVersioningOptions = BucketVersioningOptions
  { bv_enabled :: Bool
  , bv_mfa_delete :: Bool
  }
  deriving (Eq)

instance Default BucketVersioningOptions where
  def = BucketVersioningOptions False False

instance ToResourceFieldMap BucketVersioningParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "enabled" False (bv_enabled (bv_options params))
    <> rfmOptionalDefField "mfa_delete" False (bv_mfa_delete (bv_options params))
    

instance ToResourceField BucketVersioningParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data ExpirationParams = ExpirationParams
  { e_options :: ExpirationOptions
  }
  deriving (Eq)

data ExpirationOptions = ExpirationOptions
  { e_days :: Maybe (Int)
  , e_date :: Maybe (T.Text)
  , e_expired_object_delete_marker :: Bool
  }
  deriving (Eq)

instance Default ExpirationOptions where
  def = ExpirationOptions Nothing Nothing False

instance ToResourceFieldMap ExpirationParams where
  toResourceFieldMap params
    =  rfmOptionalField "days" (e_days (e_options params))
    <> rfmOptionalField "date" (e_date (e_options params))
    <> rfmOptionalDefField "expired_object_delete_marker" False (e_expired_object_delete_marker (e_options params))
    

instance ToResourceField ExpirationParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data LifecycleRuleParams = LifecycleRuleParams
  { lr_prefix :: T.Text
  , lr_enabled :: Bool
  , lr_options :: LifecycleRuleOptions
  }
  deriving (Eq)

data LifecycleRuleOptions = LifecycleRuleOptions
  { lr_id :: Maybe (T.Text)
  , lr_expiration :: Maybe (ExpirationParams)
  }
  deriving (Eq)

instance Default LifecycleRuleOptions where
  def = LifecycleRuleOptions Nothing Nothing

instance ToResourceFieldMap LifecycleRuleParams where
  toResourceFieldMap params
    =  rfmOptionalField "id" (lr_id (lr_options params))
    <> rfmField "prefix" (lr_prefix params)
    <> rfmField "enabled" (lr_enabled params)
    <> rfmOptionalField "expiration" (lr_expiration (lr_options params))
    

instance ToResourceField LifecycleRuleParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsS3Bucket to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/s3_bucket.html aws_s3_bucket> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 's3_')

awsS3Bucket :: NameElement -> T.Text -> AwsS3BucketOptions -> TF AwsS3Bucket
awsS3Bucket name0 bucket opts = awsS3Bucket' name0 (AwsS3BucketParams bucket opts)

awsS3Bucket' :: NameElement -> AwsS3BucketParams -> TF AwsS3Bucket
awsS3Bucket' name0 params = do
  rid <- mkResource "aws_s3_bucket" name0 (toResourceFieldMap params)
  return AwsS3Bucket
    { s3_id = resourceAttr rid "id"
    , s3_resource = rid
    }

data AwsS3BucketParams = AwsS3BucketParams
  { s3_bucket :: T.Text
  , s3_options :: AwsS3BucketOptions
  }

data AwsS3BucketOptions = AwsS3BucketOptions
  { s3_acl :: CannedAcl
  , s3_tags :: M.Map T.Text T.Text
  , s3_versioning :: Maybe (BucketVersioningParams)
  , s3_lifecycle_rule :: Maybe (LifecycleRuleParams)
  }

instance Default AwsS3BucketOptions where
  def = AwsS3BucketOptions "private" M.empty Nothing Nothing

instance ToResourceFieldMap AwsS3BucketParams where
  toResourceFieldMap params
    =  rfmField "bucket" (s3_bucket params)
    <> rfmOptionalDefField "acl" "private" (s3_acl (s3_options params))
    <> rfmOptionalDefField "tags" M.empty (s3_tags (s3_options params))
    <> rfmOptionalField "versioning" (s3_versioning (s3_options params))
    <> rfmOptionalField "lifecycle_rule" (s3_lifecycle_rule (s3_options params))
    

instance ToResourceField AwsS3BucketParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsS3Bucket = AwsS3Bucket
  { s3_id :: TFRef S3BucketName
  , s3_resource :: ResourceId
  }

instance IsResource AwsS3Bucket where
  resourceId = s3_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsS3BucketObject to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/d/s3_bucket_object.html aws_s3_bucket_object> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 's3o_')

awsS3BucketObject :: NameElement -> TFRef S3BucketName -> S3Key -> AwsS3BucketObjectOptions -> TF AwsS3BucketObject
awsS3BucketObject name0 bucket key opts = awsS3BucketObject' name0 (AwsS3BucketObjectParams bucket key opts)

awsS3BucketObject' :: NameElement -> AwsS3BucketObjectParams -> TF AwsS3BucketObject
awsS3BucketObject' name0 params = do
  rid <- mkResource "aws_s3_bucket_object" name0 (toResourceFieldMap params)
  return AwsS3BucketObject
    { s3o_id = resourceAttr rid "id"
    , s3o_etag = resourceAttr rid "etag"
    , s3o_version_id = resourceAttr rid "version_id"
    , s3o_resource = rid
    }

data AwsS3BucketObjectParams = AwsS3BucketObjectParams
  { s3o_bucket :: TFRef S3BucketName
  , s3o_key :: S3Key
  , s3o_options :: AwsS3BucketObjectOptions
  }

data AwsS3BucketObjectOptions = AwsS3BucketObjectOptions
  { s3o_source :: Maybe ( FilePath)
  , s3o_content :: Maybe ( T.Text)
  }

instance Default AwsS3BucketObjectOptions where
  def = AwsS3BucketObjectOptions Nothing Nothing

instance ToResourceFieldMap AwsS3BucketObjectParams where
  toResourceFieldMap params
    =  rfmField "bucket" (s3o_bucket params)
    <> rfmField "key" (s3o_key params)
    <> rfmOptionalField "source" (s3o_source (s3o_options params))
    <> rfmOptionalField "content" (s3o_content (s3o_options params))
    

instance ToResourceField AwsS3BucketObjectParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsS3BucketObject = AwsS3BucketObject
  { s3o_id :: TFRef T.Text
  , s3o_etag :: TFRef T.Text
  , s3o_version_id :: TFRef T.Text
  , s3o_resource :: ResourceId
  }

instance IsResource AwsS3BucketObject where
  resourceId = s3o_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsIamUser to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/iam_user.html aws_iam_user> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'iamu_')

awsIamUser :: NameElement -> T.Text -> AwsIamUserOptions -> TF AwsIamUser
awsIamUser name0 name' opts = awsIamUser' name0 (AwsIamUserParams name' opts)

awsIamUser' :: NameElement -> AwsIamUserParams -> TF AwsIamUser
awsIamUser' name0 params = do
  rid <- mkResource "aws_iam_user" name0 (toResourceFieldMap params)
  return AwsIamUser
    { iamu_arn = resourceAttr rid "arn"
    , iamu_name = resourceAttr rid "name"
    , iamu_unique_id = resourceAttr rid "unique_id"
    , iamu_resource = rid
    }

data AwsIamUserParams = AwsIamUserParams
  { iamu_name' :: T.Text
  , iamu_options :: AwsIamUserOptions
  }

data AwsIamUserOptions = AwsIamUserOptions
  { iamu_path :: T.Text
  , iamu_force_destroy :: Bool
  }

instance Default AwsIamUserOptions where
  def = AwsIamUserOptions "/" False

instance ToResourceFieldMap AwsIamUserParams where
  toResourceFieldMap params
    =  rfmField "name" (iamu_name' params)
    <> rfmOptionalDefField "path" "/" (iamu_path (iamu_options params))
    <> rfmOptionalDefField "force_destroy" False (iamu_force_destroy (iamu_options params))
    

instance ToResourceField AwsIamUserParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsIamUser = AwsIamUser
  { iamu_arn :: TFRef Arn
  , iamu_name :: TFRef T.Text
  , iamu_unique_id :: TFRef T.Text
  , iamu_resource :: ResourceId
  }

instance IsResource AwsIamUser where
  resourceId = iamu_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsIamUserPolicy to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/iam_user_policy.html aws_iam_user_policy> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'iamup_')

awsIamUserPolicy :: NameElement -> T.Text -> T.Text -> TFRef T.Text -> AwsIamUserPolicyOptions -> TF AwsIamUserPolicy
awsIamUserPolicy name0 name policy user opts = awsIamUserPolicy' name0 (AwsIamUserPolicyParams name policy user opts)

awsIamUserPolicy' :: NameElement -> AwsIamUserPolicyParams -> TF AwsIamUserPolicy
awsIamUserPolicy' name0 params = do
  rid <- mkResource "aws_iam_user_policy" name0 (toResourceFieldMap params)
  return AwsIamUserPolicy
    { iamup_resource = rid
    }

data AwsIamUserPolicyParams = AwsIamUserPolicyParams
  { iamup_name :: T.Text
  , iamup_policy :: T.Text
  , iamup_user :: TFRef T.Text
  , iamup_options :: AwsIamUserPolicyOptions
  }

data AwsIamUserPolicyOptions = AwsIamUserPolicyOptions
  { }

instance Default AwsIamUserPolicyOptions where
  def = AwsIamUserPolicyOptions 

instance ToResourceFieldMap AwsIamUserPolicyParams where
  toResourceFieldMap params
    =  rfmField "name" (iamup_name params)
    <> rfmField "policy" (iamup_policy params)
    <> rfmField "user" (iamup_user params)
    

instance ToResourceField AwsIamUserPolicyParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsIamUserPolicy = AwsIamUserPolicy
  { iamup_resource :: ResourceId
  }

instance IsResource AwsIamUserPolicy where
  resourceId = iamup_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsIamUserPolicyAttachment to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/iam_user_policy_attachment.html aws_iam_user_policy_attachment> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'iamupa_')

awsIamUserPolicyAttachment :: NameElement -> TFRef T.Text -> T.Text -> AwsIamUserPolicyAttachmentOptions -> TF AwsIamUserPolicyAttachment
awsIamUserPolicyAttachment name0 user policyArn opts = awsIamUserPolicyAttachment' name0 (AwsIamUserPolicyAttachmentParams user policyArn opts)

awsIamUserPolicyAttachment' :: NameElement -> AwsIamUserPolicyAttachmentParams -> TF AwsIamUserPolicyAttachment
awsIamUserPolicyAttachment' name0 params = do
  rid <- mkResource "aws_iam_user_policy_attachment" name0 (toResourceFieldMap params)
  return AwsIamUserPolicyAttachment
    { iamupa_resource = rid
    }

data AwsIamUserPolicyAttachmentParams = AwsIamUserPolicyAttachmentParams
  { iamupa_user :: TFRef T.Text
  , iamupa_policy_arn :: T.Text
  , iamupa_options :: AwsIamUserPolicyAttachmentOptions
  }

data AwsIamUserPolicyAttachmentOptions = AwsIamUserPolicyAttachmentOptions
  { }

instance Default AwsIamUserPolicyAttachmentOptions where
  def = AwsIamUserPolicyAttachmentOptions 

instance ToResourceFieldMap AwsIamUserPolicyAttachmentParams where
  toResourceFieldMap params
    =  rfmField "user" (iamupa_user params)
    <> rfmField "policy_arn" (iamupa_policy_arn params)
    

instance ToResourceField AwsIamUserPolicyAttachmentParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsIamUserPolicyAttachment = AwsIamUserPolicyAttachment
  { iamupa_resource :: ResourceId
  }

instance IsResource AwsIamUserPolicyAttachment where
  resourceId = iamupa_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsIamRole to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/iam_role.html aws_iam_role> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'iamr_')

awsIamRole :: NameElement -> T.Text -> AwsIamRoleOptions -> TF AwsIamRole
awsIamRole name0 assumeRolePolicy opts = awsIamRole' name0 (AwsIamRoleParams assumeRolePolicy opts)

awsIamRole' :: NameElement -> AwsIamRoleParams -> TF AwsIamRole
awsIamRole' name0 params = do
  rid <- mkResource "aws_iam_role" name0 (toResourceFieldMap params)
  return AwsIamRole
    { iamr_id = resourceAttr rid "id"
    , iamr_arn = resourceAttr rid "arn"
    , iamr_name = resourceAttr rid "name"
    , iamr_create_date = resourceAttr rid "create_date"
    , iamr_unique_id = resourceAttr rid "unique_id"
    , iamr_resource = rid
    }

data AwsIamRoleParams = AwsIamRoleParams
  { iamr_assume_role_policy :: T.Text
  , iamr_options :: AwsIamRoleOptions
  }

data AwsIamRoleOptions = AwsIamRoleOptions
  { iamr_name' :: T.Text
  , iamr_name_prefix :: T.Text
  , iamr_path :: T.Text
  }

instance Default AwsIamRoleOptions where
  def = AwsIamRoleOptions "" "" ""

instance ToResourceFieldMap AwsIamRoleParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "name" "" (iamr_name' (iamr_options params))
    <> rfmOptionalDefField "name_prefix" "" (iamr_name_prefix (iamr_options params))
    <> rfmField "assume_role_policy" (iamr_assume_role_policy params)
    <> rfmOptionalDefField "path" "" (iamr_path (iamr_options params))
    

instance ToResourceField AwsIamRoleParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsIamRole = AwsIamRole
  { iamr_id :: TFRef (AwsId AwsIamRole)
  , iamr_arn :: TFRef Arn
  , iamr_name :: TFRef T.Text
  , iamr_create_date :: TFRef T.Text
  , iamr_unique_id :: TFRef T.Text
  , iamr_resource :: ResourceId
  }

instance IsResource AwsIamRole where
  resourceId = iamr_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsIamInstanceProfile to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/iam_instance_profile.html aws_iam_instance_profile> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'iamip_')

awsIamInstanceProfile :: NameElement ->  AwsIamInstanceProfileOptions -> TF AwsIamInstanceProfile
awsIamInstanceProfile name0  opts = awsIamInstanceProfile' name0 (AwsIamInstanceProfileParams  opts)

awsIamInstanceProfile' :: NameElement -> AwsIamInstanceProfileParams -> TF AwsIamInstanceProfile
awsIamInstanceProfile' name0 params = do
  rid <- mkResource "aws_iam_instance_profile" name0 (toResourceFieldMap params)
  return AwsIamInstanceProfile
    { iamip_id = resourceAttr rid "id"
    , iamip_arn = resourceAttr rid "arn"
    , iamip_create_date = resourceAttr rid "create_date"
    , iamip_unique_id = resourceAttr rid "unique_id"
    , iamip_resource = rid
    }

data AwsIamInstanceProfileParams = AwsIamInstanceProfileParams
  { iamip_options :: AwsIamInstanceProfileOptions
  }

data AwsIamInstanceProfileOptions = AwsIamInstanceProfileOptions
  { iamip_name :: T.Text
  , iamip_name_prefix :: T.Text
  , iamip_path :: T.Text
  , iamip_roles :: [TFRef T.Text]
  }

instance Default AwsIamInstanceProfileOptions where
  def = AwsIamInstanceProfileOptions "" "" "/" []

instance ToResourceFieldMap AwsIamInstanceProfileParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "name" "" (iamip_name (iamip_options params))
    <> rfmOptionalDefField "name_prefix" "" (iamip_name_prefix (iamip_options params))
    <> rfmOptionalDefField "path" "/" (iamip_path (iamip_options params))
    <> rfmOptionalDefField "roles" [] (iamip_roles (iamip_options params))
    

instance ToResourceField AwsIamInstanceProfileParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsIamInstanceProfile = AwsIamInstanceProfile
  { iamip_id :: TFRef (AwsId AwsIamInstanceProfile)
  , iamip_arn :: TFRef Arn
  , iamip_create_date :: TFRef T.Text
  , iamip_unique_id :: TFRef T.Text
  , iamip_resource :: ResourceId
  }

instance IsResource AwsIamInstanceProfile where
  resourceId = iamip_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsIamRolePolicy to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/iam_role_policy.html aws_iam_role_policy> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'iamrp_')

awsIamRolePolicy :: NameElement -> T.Text -> T.Text -> TFRef (AwsId AwsIamRole) -> AwsIamRolePolicyOptions -> TF AwsIamRolePolicy
awsIamRolePolicy name0 name policy role opts = awsIamRolePolicy' name0 (AwsIamRolePolicyParams name policy role opts)

awsIamRolePolicy' :: NameElement -> AwsIamRolePolicyParams -> TF AwsIamRolePolicy
awsIamRolePolicy' name0 params = do
  rid <- mkResource "aws_iam_role_policy" name0 (toResourceFieldMap params)
  return AwsIamRolePolicy
    { iamrp_id = resourceAttr rid "id"
    , iamrp_resource = rid
    }

data AwsIamRolePolicyParams = AwsIamRolePolicyParams
  { iamrp_name :: T.Text
  , iamrp_policy :: T.Text
  , iamrp_role :: TFRef (AwsId AwsIamRole)
  , iamrp_options :: AwsIamRolePolicyOptions
  }

data AwsIamRolePolicyOptions = AwsIamRolePolicyOptions
  { }

instance Default AwsIamRolePolicyOptions where
  def = AwsIamRolePolicyOptions 

instance ToResourceFieldMap AwsIamRolePolicyParams where
  toResourceFieldMap params
    =  rfmField "name" (iamrp_name params)
    <> rfmField "policy" (iamrp_policy params)
    <> rfmField "role" (iamrp_role params)
    

instance ToResourceField AwsIamRolePolicyParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsIamRolePolicy = AwsIamRolePolicy
  { iamrp_id :: TFRef (AwsId AwsIamInstanceProfile)
  , iamrp_resource :: ResourceId
  }

instance IsResource AwsIamRolePolicy where
  resourceId = iamrp_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsSnsTopic to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/sns_topic.html aws_sns_topic> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'sns_')

awsSnsTopic :: NameElement -> T.Text -> AwsSnsTopicOptions -> TF AwsSnsTopic
awsSnsTopic name0 name opts = awsSnsTopic' name0 (AwsSnsTopicParams name opts)

awsSnsTopic' :: NameElement -> AwsSnsTopicParams -> TF AwsSnsTopic
awsSnsTopic' name0 params = do
  rid <- mkResource "aws_sns_topic" name0 (toResourceFieldMap params)
  return AwsSnsTopic
    { sns_id = resourceAttr rid "id"
    , sns_arn = resourceAttr rid "arn"
    , sns_resource = rid
    }

data AwsSnsTopicParams = AwsSnsTopicParams
  { sns_name :: T.Text
  , sns_options :: AwsSnsTopicOptions
  }

data AwsSnsTopicOptions = AwsSnsTopicOptions
  { sns_display_name :: T.Text
  }

instance Default AwsSnsTopicOptions where
  def = AwsSnsTopicOptions ""

instance ToResourceFieldMap AwsSnsTopicParams where
  toResourceFieldMap params
    =  rfmField "name" (sns_name params)
    <> rfmOptionalDefField "display_name" "" (sns_display_name (sns_options params))
    

instance ToResourceField AwsSnsTopicParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsSnsTopic = AwsSnsTopic
  { sns_id :: TFRef (AwsId AwsSnsTopic)
  , sns_arn :: TFRef Arn
  , sns_resource :: ResourceId
  }

instance IsResource AwsSnsTopic where
  resourceId = sns_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsCloudwatchMetricAlarm to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/cloudwatch_metric_alarm.html aws_cloudwatch_metric_alarm> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'cma_')

awsCloudwatchMetricAlarm :: NameElement -> T.Text -> MetricComparisonOperator -> Int -> MetricName -> MetricNamespace -> Int -> MetricStatistic -> Int -> AwsCloudwatchMetricAlarmOptions -> TF AwsCloudwatchMetricAlarm
awsCloudwatchMetricAlarm name0 alarmName comparisonOperator evaluationPeriods metricName namespace period statistic threshold opts = awsCloudwatchMetricAlarm' name0 (AwsCloudwatchMetricAlarmParams alarmName comparisonOperator evaluationPeriods metricName namespace period statistic threshold opts)

awsCloudwatchMetricAlarm' :: NameElement -> AwsCloudwatchMetricAlarmParams -> TF AwsCloudwatchMetricAlarm
awsCloudwatchMetricAlarm' name0 params = do
  rid <- mkResource "aws_cloudwatch_metric_alarm" name0 (toResourceFieldMap params)
  return AwsCloudwatchMetricAlarm
    { cma_id = resourceAttr rid "id"
    , cma_resource = rid
    }

data AwsCloudwatchMetricAlarmParams = AwsCloudwatchMetricAlarmParams
  { cma_alarm_name :: T.Text
  , cma_comparison_operator :: MetricComparisonOperator
  , cma_evaluation_periods :: Int
  , cma_metric_name :: MetricName
  , cma_namespace :: MetricNamespace
  , cma_period :: Int
  , cma_statistic :: MetricStatistic
  , cma_threshold :: Int
  , cma_options :: AwsCloudwatchMetricAlarmOptions
  }

data AwsCloudwatchMetricAlarmOptions = AwsCloudwatchMetricAlarmOptions
  { cma_actions_enabled :: Bool
  , cma_alarm_actions :: [TFRef Arn]
  , cma_alarm_description :: T.Text
  , cma_dimensions :: M.Map T.Text T.Text
  , cma_insufficient_data_actions :: [TFRef Arn]
  , cma_ok_actions :: [TFRef Arn]
  , cma_unit :: MetricUnit
  }

instance Default AwsCloudwatchMetricAlarmOptions where
  def = AwsCloudwatchMetricAlarmOptions True [] "" M.empty [] [] ""

instance ToResourceFieldMap AwsCloudwatchMetricAlarmParams where
  toResourceFieldMap params
    =  rfmField "alarm_name" (cma_alarm_name params)
    <> rfmField "comparison_operator" (cma_comparison_operator params)
    <> rfmField "evaluation_periods" (cma_evaluation_periods params)
    <> rfmField "metric_name" (cma_metric_name params)
    <> rfmField "namespace" (cma_namespace params)
    <> rfmField "period" (cma_period params)
    <> rfmField "statistic" (cma_statistic params)
    <> rfmField "threshold" (cma_threshold params)
    <> rfmOptionalDefField "actions_enabled" True (cma_actions_enabled (cma_options params))
    <> rfmOptionalDefField "alarm_actions" [] (cma_alarm_actions (cma_options params))
    <> rfmOptionalDefField "alarm_description" "" (cma_alarm_description (cma_options params))
    <> rfmOptionalDefField "dimensions" M.empty (cma_dimensions (cma_options params))
    <> rfmOptionalDefField "insufficient_data_actions" [] (cma_insufficient_data_actions (cma_options params))
    <> rfmOptionalDefField "ok_actions" [] (cma_ok_actions (cma_options params))
    <> rfmOptionalDefField "unit" "" (cma_unit (cma_options params))
    

instance ToResourceField AwsCloudwatchMetricAlarmParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsCloudwatchMetricAlarm = AwsCloudwatchMetricAlarm
  { cma_id :: TFRef (AwsId AwsCloudwatchMetricAlarm)
  , cma_resource :: ResourceId
  }

instance IsResource AwsCloudwatchMetricAlarm where
  resourceId = cma_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsDbInstance to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/db_instance.html aws_db_instance> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'db_')

awsDbInstance :: NameElement -> Int -> DBEngine -> DBInstanceClass -> T.Text -> T.Text -> AwsDbInstanceOptions -> TF AwsDbInstance
awsDbInstance name0 allocatedStorage engine instanceClass username' password opts = awsDbInstance' name0 (AwsDbInstanceParams allocatedStorage engine instanceClass username' password opts)

awsDbInstance' :: NameElement -> AwsDbInstanceParams -> TF AwsDbInstance
awsDbInstance' name0 params = do
  rid <- mkResource "aws_db_instance" name0 (toResourceFieldMap params)
  return AwsDbInstance
    { db_id = resourceAttr rid "id"
    , db_arn = resourceAttr rid "arn"
    , db_name = resourceAttr rid "name"
    , db_address = resourceAttr rid "address"
    , db_port = resourceAttr rid "port"
    , db_username = resourceAttr rid "username"
    , db_resource = rid
    }

data AwsDbInstanceParams = AwsDbInstanceParams
  { db_allocated_storage :: Int
  , db_engine :: DBEngine
  , db_instance_class :: DBInstanceClass
  , db_username' :: T.Text
  , db_password :: T.Text
  , db_options :: AwsDbInstanceOptions
  }

data AwsDbInstanceOptions = AwsDbInstanceOptions
  { db_engine_version :: T.Text
  , db_identifier :: T.Text
  , db_name' :: T.Text
  , db_port' :: Maybe (Int)
  , db_publicly_accessible :: Bool
  , db_backup_retention_period :: Int
  , db_vpc_security_group_ids :: [TFRef (AwsId AwsSecurityGroup)]
  , db_db_subnet_group_name :: Maybe (TFRef T.Text)
  , db_tags :: M.Map T.Text T.Text
  }

instance Default AwsDbInstanceOptions where
  def = AwsDbInstanceOptions "" "" "" Nothing False 0 [] Nothing M.empty

instance ToResourceFieldMap AwsDbInstanceParams where
  toResourceFieldMap params
    =  rfmField "allocated_storage" (db_allocated_storage params)
    <> rfmField "engine" (db_engine params)
    <> rfmOptionalDefField "engine_version" "" (db_engine_version (db_options params))
    <> rfmOptionalDefField "identifier" "" (db_identifier (db_options params))
    <> rfmField "instance_class" (db_instance_class params)
    <> rfmOptionalDefField "name" "" (db_name' (db_options params))
    <> rfmOptionalField "port" (db_port' (db_options params))
    <> rfmField "username" (db_username' params)
    <> rfmField "password" (db_password params)
    <> rfmOptionalDefField "publicly_accessible" False (db_publicly_accessible (db_options params))
    <> rfmOptionalDefField "backup_retention_period" 0 (db_backup_retention_period (db_options params))
    <> rfmOptionalDefField "vpc_security_group_ids" [] (db_vpc_security_group_ids (db_options params))
    <> rfmOptionalField "db_subnet_group_name" (db_db_subnet_group_name (db_options params))
    <> rfmOptionalDefField "tags" M.empty (db_tags (db_options params))
    

instance ToResourceField AwsDbInstanceParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsDbInstance = AwsDbInstance
  { db_id :: TFRef (AwsId AwsDbInstance)
  , db_arn :: TFRef Arn
  , db_name :: TFRef T.Text
  , db_address :: TFRef T.Text
  , db_port :: TFRef T.Text
  , db_username :: TFRef T.Text
  , db_resource :: ResourceId
  }

instance IsResource AwsDbInstance where
  resourceId = db_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsDbSubnetGroup to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/db_subnet_group.html aws_db_subnet_group> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'dsg_')

awsDbSubnetGroup :: NameElement -> T.Text -> [TFRef (AwsId AwsSubnet)] -> AwsDbSubnetGroupOptions -> TF AwsDbSubnetGroup
awsDbSubnetGroup name0 name' subnetIds opts = awsDbSubnetGroup' name0 (AwsDbSubnetGroupParams name' subnetIds opts)

awsDbSubnetGroup' :: NameElement -> AwsDbSubnetGroupParams -> TF AwsDbSubnetGroup
awsDbSubnetGroup' name0 params = do
  rid <- mkResource "aws_db_subnet_group" name0 (toResourceFieldMap params)
  return AwsDbSubnetGroup
    { dsg_id = resourceAttr rid "id"
    , dsg_name = resourceAttr rid "name"
    , dsg_arn = resourceAttr rid "arn"
    , dsg_resource = rid
    }

data AwsDbSubnetGroupParams = AwsDbSubnetGroupParams
  { dsg_name' :: T.Text
  , dsg_subnet_ids :: [TFRef (AwsId AwsSubnet)]
  , dsg_options :: AwsDbSubnetGroupOptions
  }

data AwsDbSubnetGroupOptions = AwsDbSubnetGroupOptions
  { dsg_description :: T.Text
  , dsg_tags :: M.Map T.Text T.Text
  }

instance Default AwsDbSubnetGroupOptions where
  def = AwsDbSubnetGroupOptions "" M.empty

instance ToResourceFieldMap AwsDbSubnetGroupParams where
  toResourceFieldMap params
    =  rfmField "name" (dsg_name' params)
    <> rfmOptionalDefField "description" "" (dsg_description (dsg_options params))
    <> rfmField "subnet_ids" (dsg_subnet_ids params)
    <> rfmOptionalDefField "tags" M.empty (dsg_tags (dsg_options params))
    

instance ToResourceField AwsDbSubnetGroupParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsDbSubnetGroup = AwsDbSubnetGroup
  { dsg_id :: TFRef (AwsId AwsDbSubnetGroup)
  , dsg_name :: TFRef T.Text
  , dsg_arn :: TFRef Arn
  , dsg_resource :: ResourceId
  }

instance IsResource AwsDbSubnetGroup where
  resourceId = dsg_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsRoute53Zone to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/route53_zone.html aws_route53_zone> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'r53z_')

awsRoute53Zone :: NameElement -> T.Text -> AwsRoute53ZoneOptions -> TF AwsRoute53Zone
awsRoute53Zone name0 name opts = awsRoute53Zone' name0 (AwsRoute53ZoneParams name opts)

awsRoute53Zone' :: NameElement -> AwsRoute53ZoneParams -> TF AwsRoute53Zone
awsRoute53Zone' name0 params = do
  rid <- mkResource "aws_route53_zone" name0 (toResourceFieldMap params)
  return AwsRoute53Zone
    { r53z_zone_id = resourceAttr rid "zone_id"
    , r53z_resource = rid
    }

data AwsRoute53ZoneParams = AwsRoute53ZoneParams
  { r53z_name :: T.Text
  , r53z_options :: AwsRoute53ZoneOptions
  }

data AwsRoute53ZoneOptions = AwsRoute53ZoneOptions
  { r53z_comment :: T.Text
  , r53z_vpc_id :: Maybe (TFRef (AwsId AwsVpc))
  , r53z_vpc_region :: Maybe (AwsRegion)
  , r53z_force_destroy :: Bool
  , r53z_tags :: M.Map T.Text T.Text
  }

instance Default AwsRoute53ZoneOptions where
  def = AwsRoute53ZoneOptions "Managed by Terraform" Nothing Nothing False M.empty

instance ToResourceFieldMap AwsRoute53ZoneParams where
  toResourceFieldMap params
    =  rfmField "name" (r53z_name params)
    <> rfmOptionalDefField "comment" "Managed by Terraform" (r53z_comment (r53z_options params))
    <> rfmOptionalField "vpc_id" (r53z_vpc_id (r53z_options params))
    <> rfmOptionalField "vpc_region" (r53z_vpc_region (r53z_options params))
    <> rfmOptionalDefField "force_destroy" False (r53z_force_destroy (r53z_options params))
    <> rfmOptionalDefField "tags" M.empty (r53z_tags (r53z_options params))
    

instance ToResourceField AwsRoute53ZoneParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsRoute53Zone = AwsRoute53Zone
  { r53z_zone_id :: TFRef HostedZoneId
  , r53z_resource :: ResourceId
  }

instance IsResource AwsRoute53Zone where
  resourceId = r53z_resource

----------------------------------------------------------------------

data Route53AliasParams = Route53AliasParams
  { r53a_zone_id :: TFRef HostedZoneId
  , r53a_name :: TFRef T.Text
  , r53a_evaluate_target_health :: Bool
  , r53a_options :: Route53AliasOptions
  }
  deriving (Eq)

data Route53AliasOptions = Route53AliasOptions
  { }
  deriving (Eq)

instance Default Route53AliasOptions where
  def = Route53AliasOptions 

instance ToResourceFieldMap Route53AliasParams where
  toResourceFieldMap params
    =  rfmField "zone_id" (r53a_zone_id params)
    <> rfmField "name" (r53a_name params)
    <> rfmField "evaluate_target_health" (r53a_evaluate_target_health params)
    

instance ToResourceField Route53AliasParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsRoute53Record to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/route53_record.html aws_route53_record> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'r53r_')

awsRoute53Record :: NameElement -> TFRef HostedZoneId -> T.Text -> Route53RecordType -> AwsRoute53RecordOptions -> TF AwsRoute53Record
awsRoute53Record name0 zoneId name type_ opts = awsRoute53Record' name0 (AwsRoute53RecordParams zoneId name type_ opts)

awsRoute53Record' :: NameElement -> AwsRoute53RecordParams -> TF AwsRoute53Record
awsRoute53Record' name0 params = do
  rid <- mkResource "aws_route53_record" name0 (toResourceFieldMap params)
  return AwsRoute53Record
    { r53r_resource = rid
    }

data AwsRoute53RecordParams = AwsRoute53RecordParams
  { r53r_zone_id :: TFRef HostedZoneId
  , r53r_name :: T.Text
  , r53r_type :: Route53RecordType
  , r53r_options :: AwsRoute53RecordOptions
  }

data AwsRoute53RecordOptions = AwsRoute53RecordOptions
  { r53r_ttl :: Maybe (Int)
  , r53r_records :: [TFRef IpAddress]
  , r53r_alias :: Maybe (Route53AliasParams)
  }

instance Default AwsRoute53RecordOptions where
  def = AwsRoute53RecordOptions Nothing [] Nothing

instance ToResourceFieldMap AwsRoute53RecordParams where
  toResourceFieldMap params
    =  rfmField "zone_id" (r53r_zone_id params)
    <> rfmField "name" (r53r_name params)
    <> rfmField "type" (r53r_type params)
    <> rfmOptionalField "ttl" (r53r_ttl (r53r_options params))
    <> rfmOptionalDefField "records" [] (r53r_records (r53r_options params))
    <> rfmOptionalField "alias" (r53r_alias (r53r_options params))
    

instance ToResourceField AwsRoute53RecordParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsRoute53Record = AwsRoute53Record
  { r53r_resource :: ResourceId
  }

instance IsResource AwsRoute53Record where
  resourceId = r53r_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsSqsQueue to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/sqs_queue.html aws_sqs_queue> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'sqs_')

awsSqsQueue :: NameElement -> T.Text -> AwsSqsQueueOptions -> TF AwsSqsQueue
awsSqsQueue name0 name opts = awsSqsQueue' name0 (AwsSqsQueueParams name opts)

awsSqsQueue' :: NameElement -> AwsSqsQueueParams -> TF AwsSqsQueue
awsSqsQueue' name0 params = do
  rid <- mkResource "aws_sqs_queue" name0 (toResourceFieldMap params)
  return AwsSqsQueue
    { sqs_id = resourceAttr rid "id"
    , sqs_arn = resourceAttr rid "arn"
    , sqs_resource = rid
    }

data AwsSqsQueueParams = AwsSqsQueueParams
  { sqs_name :: T.Text
  , sqs_options :: AwsSqsQueueOptions
  }

data AwsSqsQueueOptions = AwsSqsQueueOptions
  { sqs_visibility_timeout_seconds :: Int
  , sqs_message_retention_seconds :: Int
  , sqs_max_message_size :: Int
  , sqs_delay_seconds :: Int
  , sqs_receive_wait_time_seconds :: Int
  , sqs_policy :: Maybe (T.Text)
  , sqs_redrive_policy :: Maybe (T.Text)
  , sqs_fifo_queue :: Bool
  , sqs_content_based_deduplication :: Bool
  }

instance Default AwsSqsQueueOptions where
  def = AwsSqsQueueOptions 30 345600 262144 0 0 Nothing Nothing False False

instance ToResourceFieldMap AwsSqsQueueParams where
  toResourceFieldMap params
    =  rfmField "name" (sqs_name params)
    <> rfmOptionalDefField "visibility_timeout_seconds" 30 (sqs_visibility_timeout_seconds (sqs_options params))
    <> rfmOptionalDefField "message_retention_seconds" 345600 (sqs_message_retention_seconds (sqs_options params))
    <> rfmOptionalDefField "max_message_size" 262144 (sqs_max_message_size (sqs_options params))
    <> rfmOptionalDefField "delay_seconds" 0 (sqs_delay_seconds (sqs_options params))
    <> rfmOptionalDefField "receive_wait_time_seconds" 0 (sqs_receive_wait_time_seconds (sqs_options params))
    <> rfmOptionalField "policy" (sqs_policy (sqs_options params))
    <> rfmOptionalField "redrive_policy" (sqs_redrive_policy (sqs_options params))
    <> rfmOptionalDefField "fifo_queue" False (sqs_fifo_queue (sqs_options params))
    <> rfmOptionalDefField "content_based_deduplication" False (sqs_content_based_deduplication (sqs_options params))
    

instance ToResourceField AwsSqsQueueParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsSqsQueue = AwsSqsQueue
  { sqs_id :: TFRef (AwsId AwsSqsQueue)
  , sqs_arn :: TFRef Arn
  , sqs_resource :: ResourceId
  }

instance IsResource AwsSqsQueue where
  resourceId = sqs_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsSqsQueuePolicy to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/sqs_queue_policy.html aws_sqs_queue_policy> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'sqsp_')

awsSqsQueuePolicy :: NameElement -> T.Text -> T.Text -> AwsSqsQueuePolicyOptions -> TF AwsSqsQueuePolicy
awsSqsQueuePolicy name0 queueUrl policy opts = awsSqsQueuePolicy' name0 (AwsSqsQueuePolicyParams queueUrl policy opts)

awsSqsQueuePolicy' :: NameElement -> AwsSqsQueuePolicyParams -> TF AwsSqsQueuePolicy
awsSqsQueuePolicy' name0 params = do
  rid <- mkResource "aws_sqs_queue_policy" name0 (toResourceFieldMap params)
  return AwsSqsQueuePolicy
    { sqsp_resource = rid
    }

data AwsSqsQueuePolicyParams = AwsSqsQueuePolicyParams
  { sqsp_queue_url :: T.Text
  , sqsp_policy :: T.Text
  , sqsp_options :: AwsSqsQueuePolicyOptions
  }

data AwsSqsQueuePolicyOptions = AwsSqsQueuePolicyOptions
  { }

instance Default AwsSqsQueuePolicyOptions where
  def = AwsSqsQueuePolicyOptions 

instance ToResourceFieldMap AwsSqsQueuePolicyParams where
  toResourceFieldMap params
    =  rfmField "queue_url" (sqsp_queue_url params)
    <> rfmField "policy" (sqsp_policy params)
    

instance ToResourceField AwsSqsQueuePolicyParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsSqsQueuePolicy = AwsSqsQueuePolicy
  { sqsp_resource :: ResourceId
  }

instance IsResource AwsSqsQueuePolicy where
  resourceId = sqsp_resource

----------------------------------------------------------------------

-- | Add a resource of type AwsEcrRepository to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/ecr_repository.html aws_ecr_repository> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'ecr_')

awsEcrRepository :: NameElement -> T.Text -> AwsEcrRepositoryOptions -> TF AwsEcrRepository
awsEcrRepository name0 name' opts = awsEcrRepository' name0 (AwsEcrRepositoryParams name' opts)

awsEcrRepository' :: NameElement -> AwsEcrRepositoryParams -> TF AwsEcrRepository
awsEcrRepository' name0 params = do
  rid <- mkResource "aws_ecr_repository" name0 (toResourceFieldMap params)
  return AwsEcrRepository
    { ecr_arn = resourceAttr rid "arn"
    , ecr_name = resourceAttr rid "name"
    , ecr_registry_id = resourceAttr rid "registry_id"
    , ecr_repository_url = resourceAttr rid "repository_url"
    , ecr_resource = rid
    }

data AwsEcrRepositoryParams = AwsEcrRepositoryParams
  { ecr_name' :: T.Text
  , ecr_options :: AwsEcrRepositoryOptions
  }

data AwsEcrRepositoryOptions = AwsEcrRepositoryOptions
  { }

instance Default AwsEcrRepositoryOptions where
  def = AwsEcrRepositoryOptions 

instance ToResourceFieldMap AwsEcrRepositoryParams where
  toResourceFieldMap params
    =  rfmField "name" (ecr_name' params)
    

instance ToResourceField AwsEcrRepositoryParams where
  toResourceField = RF_Map . toResourceFieldMap 

data AwsEcrRepository = AwsEcrRepository
  { ecr_arn :: TFRef Arn
  , ecr_name :: TFRef T.Text
  , ecr_registry_id :: TFRef T.Text
  , ecr_repository_url :: TFRef T.Text
  , ecr_resource :: ResourceId
  }

instance IsResource AwsEcrRepository where
  resourceId = ecr_resource