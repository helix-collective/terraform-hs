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

-- A typed ARN
newtype AwsArn t = AwsArn {
  tArn :: Arn
} deriving (Eq);

instance ToResourceField (AwsArn t) where
  toResourceField (AwsArn t) = toResourceField t

-- | Add an aws provider to the resource graph.
--
-- See the original <https://www.terraform.io/docs/providers/aws/index.html terraform documentation>
-- for details.

newAws :: AwsParams -> TF ()
newAws params =
  mkProvider "aws" $ catMaybes
    [ Just ("region", toResourceField (aws_region params))
    , let v = aws_access_key params in if v == "" then Nothing else (Just ("access_key", toResourceField v))
    , let v = aws_secret_key params in if v == "" then Nothing else (Just ("secret_key", toResourceField v))
    ]

data AwsParams = AwsParams
  { aws_region :: AwsRegion
  , aws_access_key :: T.Text
  , aws_secret_key :: T.Text
  }

makeAwsParams :: AwsRegion -> AwsParams
makeAwsParams region = AwsParams region "" ""

----------------------------------------------------------------------

-- | Add a resource of type AwsVpc to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/d/vpc.html aws_vpc> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'vpc_')

awsVpc :: NameElement -> CidrBlock ->(AwsVpcParams -> AwsVpcParams) -> TF AwsVpc
awsVpc name0 cidrBlock modf = newAwsVpc name0 (modf (makeAwsVpcParams cidrBlock))

awsVpc' :: NameElement -> CidrBlock -> TF AwsVpc
awsVpc' name0 cidrBlock = newAwsVpc name0 (makeAwsVpcParams cidrBlock)

newAwsVpc :: NameElement -> AwsVpcParams -> TF AwsVpc
newAwsVpc name0 params = do
  rid <- mkResource "aws_vpc" name0 (toResourceFieldMap params)
  return AwsVpc
    { vpc_id = resourceAttr rid "id"
    , vpc_resource = rid
    }

data AwsVpc = AwsVpc
  { vpc_id :: TFRef (AwsId AwsVpc)
  , vpc_resource :: ResourceId
  }

instance IsResource AwsVpc where
  resourceId = vpc_resource

data AwsVpcParams = AwsVpcParams
  { _vpc_cidr_block :: CidrBlock
  , _vpc_instance_tenancy :: Maybe (T.Text)
  , _vpc_enable_dns_support :: Bool
  , _vpc_enable_dns_hostnames :: Bool
  , _vpc_enable_classic_link :: Bool
  , _vpc_tags :: M.Map T.Text T.Text
  }

-- vpc_cidr_block :: Lens' AwsVpcParams CidrBlock
vpc_cidr_block :: Functor f => (CidrBlock -> f (CidrBlock)) -> AwsVpcParams -> f AwsVpcParams
vpc_cidr_block k atom = fmap (\newvpc_cidr_block -> atom { _vpc_cidr_block = newvpc_cidr_block }) (k (_vpc_cidr_block atom))
-- vpc_instance_tenancy :: Lens' AwsVpcParams Maybe (T.Text)
vpc_instance_tenancy :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsVpcParams -> f AwsVpcParams
vpc_instance_tenancy k atom = fmap (\newvpc_instance_tenancy -> atom { _vpc_instance_tenancy = newvpc_instance_tenancy }) (k (_vpc_instance_tenancy atom))
-- vpc_enable_dns_support :: Lens' AwsVpcParams Bool
vpc_enable_dns_support :: Functor f => (Bool -> f (Bool)) -> AwsVpcParams -> f AwsVpcParams
vpc_enable_dns_support k atom = fmap (\newvpc_enable_dns_support -> atom { _vpc_enable_dns_support = newvpc_enable_dns_support }) (k (_vpc_enable_dns_support atom))
-- vpc_enable_dns_hostnames :: Lens' AwsVpcParams Bool
vpc_enable_dns_hostnames :: Functor f => (Bool -> f (Bool)) -> AwsVpcParams -> f AwsVpcParams
vpc_enable_dns_hostnames k atom = fmap (\newvpc_enable_dns_hostnames -> atom { _vpc_enable_dns_hostnames = newvpc_enable_dns_hostnames }) (k (_vpc_enable_dns_hostnames atom))
-- vpc_enable_classic_link :: Lens' AwsVpcParams Bool
vpc_enable_classic_link :: Functor f => (Bool -> f (Bool)) -> AwsVpcParams -> f AwsVpcParams
vpc_enable_classic_link k atom = fmap (\newvpc_enable_classic_link -> atom { _vpc_enable_classic_link = newvpc_enable_classic_link }) (k (_vpc_enable_classic_link atom))
-- vpc_tags :: Lens' AwsVpcParams M.Map T.Text T.Text
vpc_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsVpcParams -> f AwsVpcParams
vpc_tags k atom = fmap (\newvpc_tags -> atom { _vpc_tags = newvpc_tags }) (k (_vpc_tags atom))

makeAwsVpcParams :: CidrBlock -> AwsVpcParams
makeAwsVpcParams cidrBlock = AwsVpcParams
  { _vpc_cidr_block = cidrBlock
  , _vpc_instance_tenancy = Nothing
  , _vpc_enable_dns_support = True
  , _vpc_enable_dns_hostnames = False
  , _vpc_enable_classic_link = False
  , _vpc_tags = M.empty
  }

instance ToResourceFieldMap AwsVpcParams where
  toResourceFieldMap params
    =  rfmField "cidr_block" (_vpc_cidr_block params)
    <> rfmOptionalField "instance_tenancy" (_vpc_instance_tenancy params)
    <> rfmOptionalDefField "enable_dns_support" True (_vpc_enable_dns_support params)
    <> rfmOptionalDefField "enable_dns_hostnames" False (_vpc_enable_dns_hostnames params)
    <> rfmOptionalDefField "enable_classic_link" False (_vpc_enable_classic_link params)
    <> rfmOptionalDefField "tags" M.empty (_vpc_tags params)
    

instance ToResourceField AwsVpcParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsNatGateway to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/nat_gateway.html aws_nat_gateway> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'ng_')

awsNatGateway :: NameElement -> TFRef (AwsId AwsEip) -> TFRef (AwsId AwsSubnet) ->(AwsNatGatewayParams -> AwsNatGatewayParams) -> TF AwsNatGateway
awsNatGateway name0 allocationId subnetId modf = newAwsNatGateway name0 (modf (makeAwsNatGatewayParams allocationId subnetId))

awsNatGateway' :: NameElement -> TFRef (AwsId AwsEip) -> TFRef (AwsId AwsSubnet) -> TF AwsNatGateway
awsNatGateway' name0 allocationId subnetId = newAwsNatGateway name0 (makeAwsNatGatewayParams allocationId subnetId)

newAwsNatGateway :: NameElement -> AwsNatGatewayParams -> TF AwsNatGateway
newAwsNatGateway name0 params = do
  rid <- mkResource "aws_nat_gateway" name0 (toResourceFieldMap params)
  return AwsNatGateway
    { ng_id = resourceAttr rid "id"
    , ng_resource = rid
    }

data AwsNatGateway = AwsNatGateway
  { ng_id :: TFRef (AwsId AwsNatGateway)
  , ng_resource :: ResourceId
  }

instance IsResource AwsNatGateway where
  resourceId = ng_resource

data AwsNatGatewayParams = AwsNatGatewayParams
  { _ng_allocation_id :: TFRef (AwsId AwsEip)
  , _ng_subnet_id :: TFRef (AwsId AwsSubnet)
  }

-- ng_allocation_id :: Lens' AwsNatGatewayParams TFRef (AwsId AwsEip)
ng_allocation_id :: Functor f => (TFRef (AwsId AwsEip) -> f (TFRef (AwsId AwsEip))) -> AwsNatGatewayParams -> f AwsNatGatewayParams
ng_allocation_id k atom = fmap (\newng_allocation_id -> atom { _ng_allocation_id = newng_allocation_id }) (k (_ng_allocation_id atom))
-- ng_subnet_id :: Lens' AwsNatGatewayParams TFRef (AwsId AwsSubnet)
ng_subnet_id :: Functor f => (TFRef (AwsId AwsSubnet) -> f (TFRef (AwsId AwsSubnet))) -> AwsNatGatewayParams -> f AwsNatGatewayParams
ng_subnet_id k atom = fmap (\newng_subnet_id -> atom { _ng_subnet_id = newng_subnet_id }) (k (_ng_subnet_id atom))

makeAwsNatGatewayParams :: TFRef (AwsId AwsEip) -> TFRef (AwsId AwsSubnet) -> AwsNatGatewayParams
makeAwsNatGatewayParams allocationId subnetId = AwsNatGatewayParams
  { _ng_allocation_id = allocationId
  , _ng_subnet_id = subnetId
  }

instance ToResourceFieldMap AwsNatGatewayParams where
  toResourceFieldMap params
    =  rfmField "allocation_id" (_ng_allocation_id params)
    <> rfmField "subnet_id" (_ng_subnet_id params)
    

instance ToResourceField AwsNatGatewayParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsInternetGateway to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/internet_gateway.html aws_internet_gateway> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'ig_')

awsInternetGateway :: NameElement -> TFRef (AwsId AwsVpc) ->(AwsInternetGatewayParams -> AwsInternetGatewayParams) -> TF AwsInternetGateway
awsInternetGateway name0 vpcId modf = newAwsInternetGateway name0 (modf (makeAwsInternetGatewayParams vpcId))

awsInternetGateway' :: NameElement -> TFRef (AwsId AwsVpc) -> TF AwsInternetGateway
awsInternetGateway' name0 vpcId = newAwsInternetGateway name0 (makeAwsInternetGatewayParams vpcId)

newAwsInternetGateway :: NameElement -> AwsInternetGatewayParams -> TF AwsInternetGateway
newAwsInternetGateway name0 params = do
  rid <- mkResource "aws_internet_gateway" name0 (toResourceFieldMap params)
  return AwsInternetGateway
    { ig_id = resourceAttr rid "id"
    , ig_resource = rid
    }

data AwsInternetGateway = AwsInternetGateway
  { ig_id :: TFRef (AwsId AwsInternetGateway)
  , ig_resource :: ResourceId
  }

instance IsResource AwsInternetGateway where
  resourceId = ig_resource

data AwsInternetGatewayParams = AwsInternetGatewayParams
  { _ig_vpc_id :: TFRef (AwsId AwsVpc)
  , _ig_tags :: M.Map T.Text T.Text
  }

-- ig_vpc_id :: Lens' AwsInternetGatewayParams TFRef (AwsId AwsVpc)
ig_vpc_id :: Functor f => (TFRef (AwsId AwsVpc) -> f (TFRef (AwsId AwsVpc))) -> AwsInternetGatewayParams -> f AwsInternetGatewayParams
ig_vpc_id k atom = fmap (\newig_vpc_id -> atom { _ig_vpc_id = newig_vpc_id }) (k (_ig_vpc_id atom))
-- ig_tags :: Lens' AwsInternetGatewayParams M.Map T.Text T.Text
ig_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsInternetGatewayParams -> f AwsInternetGatewayParams
ig_tags k atom = fmap (\newig_tags -> atom { _ig_tags = newig_tags }) (k (_ig_tags atom))

makeAwsInternetGatewayParams :: TFRef (AwsId AwsVpc) -> AwsInternetGatewayParams
makeAwsInternetGatewayParams vpcId = AwsInternetGatewayParams
  { _ig_vpc_id = vpcId
  , _ig_tags = M.empty
  }

instance ToResourceFieldMap AwsInternetGatewayParams where
  toResourceFieldMap params
    =  rfmField "vpc_id" (_ig_vpc_id params)
    <> rfmOptionalDefField "tags" M.empty (_ig_tags params)
    

instance ToResourceField AwsInternetGatewayParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsSubnet to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/d/subnet.html aws_subnet> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'sn_')

awsSubnet :: NameElement -> TFRef (AwsId AwsVpc) -> CidrBlock ->(AwsSubnetParams -> AwsSubnetParams) -> TF AwsSubnet
awsSubnet name0 vpcId cidrBlock modf = newAwsSubnet name0 (modf (makeAwsSubnetParams vpcId cidrBlock))

awsSubnet' :: NameElement -> TFRef (AwsId AwsVpc) -> CidrBlock -> TF AwsSubnet
awsSubnet' name0 vpcId cidrBlock = newAwsSubnet name0 (makeAwsSubnetParams vpcId cidrBlock)

newAwsSubnet :: NameElement -> AwsSubnetParams -> TF AwsSubnet
newAwsSubnet name0 params = do
  rid <- mkResource "aws_subnet" name0 (toResourceFieldMap params)
  return AwsSubnet
    { sn_id = resourceAttr rid "id"
    , sn_resource = rid
    }

data AwsSubnet = AwsSubnet
  { sn_id :: TFRef (AwsId AwsSubnet)
  , sn_resource :: ResourceId
  }

instance IsResource AwsSubnet where
  resourceId = sn_resource

data AwsSubnetParams = AwsSubnetParams
  { _sn_vpc_id :: TFRef (AwsId AwsVpc)
  , _sn_cidr_block :: CidrBlock
  , _sn_map_public_ip_on_launch :: Bool
  , _sn_availability_zone :: AvailabilityZone
  , _sn_tags :: M.Map T.Text T.Text
  }

-- sn_vpc_id :: Lens' AwsSubnetParams TFRef (AwsId AwsVpc)
sn_vpc_id :: Functor f => (TFRef (AwsId AwsVpc) -> f (TFRef (AwsId AwsVpc))) -> AwsSubnetParams -> f AwsSubnetParams
sn_vpc_id k atom = fmap (\newsn_vpc_id -> atom { _sn_vpc_id = newsn_vpc_id }) (k (_sn_vpc_id atom))
-- sn_cidr_block :: Lens' AwsSubnetParams CidrBlock
sn_cidr_block :: Functor f => (CidrBlock -> f (CidrBlock)) -> AwsSubnetParams -> f AwsSubnetParams
sn_cidr_block k atom = fmap (\newsn_cidr_block -> atom { _sn_cidr_block = newsn_cidr_block }) (k (_sn_cidr_block atom))
-- sn_map_public_ip_on_launch :: Lens' AwsSubnetParams Bool
sn_map_public_ip_on_launch :: Functor f => (Bool -> f (Bool)) -> AwsSubnetParams -> f AwsSubnetParams
sn_map_public_ip_on_launch k atom = fmap (\newsn_map_public_ip_on_launch -> atom { _sn_map_public_ip_on_launch = newsn_map_public_ip_on_launch }) (k (_sn_map_public_ip_on_launch atom))
-- sn_availability_zone :: Lens' AwsSubnetParams AvailabilityZone
sn_availability_zone :: Functor f => (AvailabilityZone -> f (AvailabilityZone)) -> AwsSubnetParams -> f AwsSubnetParams
sn_availability_zone k atom = fmap (\newsn_availability_zone -> atom { _sn_availability_zone = newsn_availability_zone }) (k (_sn_availability_zone atom))
-- sn_tags :: Lens' AwsSubnetParams M.Map T.Text T.Text
sn_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsSubnetParams -> f AwsSubnetParams
sn_tags k atom = fmap (\newsn_tags -> atom { _sn_tags = newsn_tags }) (k (_sn_tags atom))

makeAwsSubnetParams :: TFRef (AwsId AwsVpc) -> CidrBlock -> AwsSubnetParams
makeAwsSubnetParams vpcId cidrBlock = AwsSubnetParams
  { _sn_vpc_id = vpcId
  , _sn_cidr_block = cidrBlock
  , _sn_map_public_ip_on_launch = False
  , _sn_availability_zone = ""
  , _sn_tags = M.empty
  }

instance ToResourceFieldMap AwsSubnetParams where
  toResourceFieldMap params
    =  rfmField "vpc_id" (_sn_vpc_id params)
    <> rfmField "cidr_block" (_sn_cidr_block params)
    <> rfmOptionalDefField "map_public_ip_on_launch" False (_sn_map_public_ip_on_launch params)
    <> rfmOptionalDefField "availability_zone" "" (_sn_availability_zone params)
    <> rfmOptionalDefField "tags" M.empty (_sn_tags params)
    

instance ToResourceField AwsSubnetParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsRouteTable to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/route_table.html aws_route_table> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'rt_')

awsRouteTable :: NameElement -> TFRef (AwsId AwsVpc) ->(AwsRouteTableParams -> AwsRouteTableParams) -> TF AwsRouteTable
awsRouteTable name0 vpcId modf = newAwsRouteTable name0 (modf (makeAwsRouteTableParams vpcId))

awsRouteTable' :: NameElement -> TFRef (AwsId AwsVpc) -> TF AwsRouteTable
awsRouteTable' name0 vpcId = newAwsRouteTable name0 (makeAwsRouteTableParams vpcId)

newAwsRouteTable :: NameElement -> AwsRouteTableParams -> TF AwsRouteTable
newAwsRouteTable name0 params = do
  rid <- mkResource "aws_route_table" name0 (toResourceFieldMap params)
  return AwsRouteTable
    { rt_id = resourceAttr rid "id"
    , rt_resource = rid
    }

data AwsRouteTable = AwsRouteTable
  { rt_id :: TFRef (AwsId AwsRouteTable)
  , rt_resource :: ResourceId
  }

instance IsResource AwsRouteTable where
  resourceId = rt_resource

data AwsRouteTableParams = AwsRouteTableParams
  { _rt_vpc_id :: TFRef (AwsId AwsVpc)
  , _rt_tags :: M.Map T.Text T.Text
  }

-- rt_vpc_id :: Lens' AwsRouteTableParams TFRef (AwsId AwsVpc)
rt_vpc_id :: Functor f => (TFRef (AwsId AwsVpc) -> f (TFRef (AwsId AwsVpc))) -> AwsRouteTableParams -> f AwsRouteTableParams
rt_vpc_id k atom = fmap (\newrt_vpc_id -> atom { _rt_vpc_id = newrt_vpc_id }) (k (_rt_vpc_id atom))
-- rt_tags :: Lens' AwsRouteTableParams M.Map T.Text T.Text
rt_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsRouteTableParams -> f AwsRouteTableParams
rt_tags k atom = fmap (\newrt_tags -> atom { _rt_tags = newrt_tags }) (k (_rt_tags atom))

makeAwsRouteTableParams :: TFRef (AwsId AwsVpc) -> AwsRouteTableParams
makeAwsRouteTableParams vpcId = AwsRouteTableParams
  { _rt_vpc_id = vpcId
  , _rt_tags = M.empty
  }

instance ToResourceFieldMap AwsRouteTableParams where
  toResourceFieldMap params
    =  rfmField "vpc_id" (_rt_vpc_id params)
    <> rfmOptionalDefField "tags" M.empty (_rt_tags params)
    

instance ToResourceField AwsRouteTableParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsRoute to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/route.html aws_route> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'r_')

awsRoute :: NameElement -> TFRef (AwsId AwsRouteTable) -> CidrBlock ->(AwsRouteParams -> AwsRouteParams) -> TF AwsRoute
awsRoute name0 routeTableId destinationCidrBlock modf = newAwsRoute name0 (modf (makeAwsRouteParams routeTableId destinationCidrBlock))

awsRoute' :: NameElement -> TFRef (AwsId AwsRouteTable) -> CidrBlock -> TF AwsRoute
awsRoute' name0 routeTableId destinationCidrBlock = newAwsRoute name0 (makeAwsRouteParams routeTableId destinationCidrBlock)

newAwsRoute :: NameElement -> AwsRouteParams -> TF AwsRoute
newAwsRoute name0 params = do
  rid <- mkResource "aws_route" name0 (toResourceFieldMap params)
  return AwsRoute
    { r_resource = rid
    }

data AwsRoute = AwsRoute
  { r_resource :: ResourceId
  }

instance IsResource AwsRoute where
  resourceId = r_resource

data AwsRouteParams = AwsRouteParams
  { _r_route_table_id :: TFRef (AwsId AwsRouteTable)
  , _r_destination_cidr_block :: CidrBlock
  , _r_nat_gateway_id :: Maybe (TFRef (AwsId AwsNatGateway))
  , _r_gateway_id :: Maybe (TFRef (AwsId AwsInternetGateway))
  }

-- r_route_table_id :: Lens' AwsRouteParams TFRef (AwsId AwsRouteTable)
r_route_table_id :: Functor f => (TFRef (AwsId AwsRouteTable) -> f (TFRef (AwsId AwsRouteTable))) -> AwsRouteParams -> f AwsRouteParams
r_route_table_id k atom = fmap (\newr_route_table_id -> atom { _r_route_table_id = newr_route_table_id }) (k (_r_route_table_id atom))
-- r_destination_cidr_block :: Lens' AwsRouteParams CidrBlock
r_destination_cidr_block :: Functor f => (CidrBlock -> f (CidrBlock)) -> AwsRouteParams -> f AwsRouteParams
r_destination_cidr_block k atom = fmap (\newr_destination_cidr_block -> atom { _r_destination_cidr_block = newr_destination_cidr_block }) (k (_r_destination_cidr_block atom))
-- r_nat_gateway_id :: Lens' AwsRouteParams Maybe (TFRef (AwsId AwsNatGateway))
r_nat_gateway_id :: Functor f => (Maybe (TFRef (AwsId AwsNatGateway)) -> f (Maybe (TFRef (AwsId AwsNatGateway)))) -> AwsRouteParams -> f AwsRouteParams
r_nat_gateway_id k atom = fmap (\newr_nat_gateway_id -> atom { _r_nat_gateway_id = newr_nat_gateway_id }) (k (_r_nat_gateway_id atom))
-- r_gateway_id :: Lens' AwsRouteParams Maybe (TFRef (AwsId AwsInternetGateway))
r_gateway_id :: Functor f => (Maybe (TFRef (AwsId AwsInternetGateway)) -> f (Maybe (TFRef (AwsId AwsInternetGateway)))) -> AwsRouteParams -> f AwsRouteParams
r_gateway_id k atom = fmap (\newr_gateway_id -> atom { _r_gateway_id = newr_gateway_id }) (k (_r_gateway_id atom))

makeAwsRouteParams :: TFRef (AwsId AwsRouteTable) -> CidrBlock -> AwsRouteParams
makeAwsRouteParams routeTableId destinationCidrBlock = AwsRouteParams
  { _r_route_table_id = routeTableId
  , _r_destination_cidr_block = destinationCidrBlock
  , _r_nat_gateway_id = Nothing
  , _r_gateway_id = Nothing
  }

instance ToResourceFieldMap AwsRouteParams where
  toResourceFieldMap params
    =  rfmField "route_table_id" (_r_route_table_id params)
    <> rfmField "destination_cidr_block" (_r_destination_cidr_block params)
    <> rfmOptionalField "nat_gateway_id" (_r_nat_gateway_id params)
    <> rfmOptionalField "gateway_id" (_r_gateway_id params)
    

instance ToResourceField AwsRouteParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsRouteTableAssociation to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/route_table_association.html aws_route_table_association> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'rta_')

awsRouteTableAssociation :: NameElement -> TFRef (AwsId AwsSubnet) -> TFRef (AwsId AwsRouteTable) ->(AwsRouteTableAssociationParams -> AwsRouteTableAssociationParams) -> TF AwsRouteTableAssociation
awsRouteTableAssociation name0 subnetId routeTableId modf = newAwsRouteTableAssociation name0 (modf (makeAwsRouteTableAssociationParams subnetId routeTableId))

awsRouteTableAssociation' :: NameElement -> TFRef (AwsId AwsSubnet) -> TFRef (AwsId AwsRouteTable) -> TF AwsRouteTableAssociation
awsRouteTableAssociation' name0 subnetId routeTableId = newAwsRouteTableAssociation name0 (makeAwsRouteTableAssociationParams subnetId routeTableId)

newAwsRouteTableAssociation :: NameElement -> AwsRouteTableAssociationParams -> TF AwsRouteTableAssociation
newAwsRouteTableAssociation name0 params = do
  rid <- mkResource "aws_route_table_association" name0 (toResourceFieldMap params)
  return AwsRouteTableAssociation
    { rta_id = resourceAttr rid "id"
    , rta_resource = rid
    }

data AwsRouteTableAssociation = AwsRouteTableAssociation
  { rta_id :: TFRef (AwsId AwsRouteTableAssociation)
  , rta_resource :: ResourceId
  }

instance IsResource AwsRouteTableAssociation where
  resourceId = rta_resource

data AwsRouteTableAssociationParams = AwsRouteTableAssociationParams
  { _rta_subnet_id :: TFRef (AwsId AwsSubnet)
  , _rta_route_table_id :: TFRef (AwsId AwsRouteTable)
  }

-- rta_subnet_id :: Lens' AwsRouteTableAssociationParams TFRef (AwsId AwsSubnet)
rta_subnet_id :: Functor f => (TFRef (AwsId AwsSubnet) -> f (TFRef (AwsId AwsSubnet))) -> AwsRouteTableAssociationParams -> f AwsRouteTableAssociationParams
rta_subnet_id k atom = fmap (\newrta_subnet_id -> atom { _rta_subnet_id = newrta_subnet_id }) (k (_rta_subnet_id atom))
-- rta_route_table_id :: Lens' AwsRouteTableAssociationParams TFRef (AwsId AwsRouteTable)
rta_route_table_id :: Functor f => (TFRef (AwsId AwsRouteTable) -> f (TFRef (AwsId AwsRouteTable))) -> AwsRouteTableAssociationParams -> f AwsRouteTableAssociationParams
rta_route_table_id k atom = fmap (\newrta_route_table_id -> atom { _rta_route_table_id = newrta_route_table_id }) (k (_rta_route_table_id atom))

makeAwsRouteTableAssociationParams :: TFRef (AwsId AwsSubnet) -> TFRef (AwsId AwsRouteTable) -> AwsRouteTableAssociationParams
makeAwsRouteTableAssociationParams subnetId routeTableId = AwsRouteTableAssociationParams
  { _rta_subnet_id = subnetId
  , _rta_route_table_id = routeTableId
  }

instance ToResourceFieldMap AwsRouteTableAssociationParams where
  toResourceFieldMap params
    =  rfmField "subnet_id" (_rta_subnet_id params)
    <> rfmField "route_table_id" (_rta_route_table_id params)
    

instance ToResourceField AwsRouteTableAssociationParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data IngressRuleParams = IngressRuleParams
  { _ir_from_port :: Int
  , _ir_to_port :: Int
  , _ir_protocol :: T.Text
  , _ir_cidr_blocks :: [CidrBlock]
  }
  deriving (Eq)

-- ir_from_port :: Lens' IngressRuleParams Int
ir_from_port :: Functor f => (Int -> f (Int)) -> IngressRuleParams -> f IngressRuleParams
ir_from_port k atom = fmap (\newir_from_port -> atom { _ir_from_port = newir_from_port }) (k (_ir_from_port atom))
-- ir_to_port :: Lens' IngressRuleParams Int
ir_to_port :: Functor f => (Int -> f (Int)) -> IngressRuleParams -> f IngressRuleParams
ir_to_port k atom = fmap (\newir_to_port -> atom { _ir_to_port = newir_to_port }) (k (_ir_to_port atom))
-- ir_protocol :: Lens' IngressRuleParams T.Text
ir_protocol :: Functor f => (T.Text -> f (T.Text)) -> IngressRuleParams -> f IngressRuleParams
ir_protocol k atom = fmap (\newir_protocol -> atom { _ir_protocol = newir_protocol }) (k (_ir_protocol atom))
-- ir_cidr_blocks :: Lens' IngressRuleParams [CidrBlock]
ir_cidr_blocks :: Functor f => ([CidrBlock] -> f ([CidrBlock])) -> IngressRuleParams -> f IngressRuleParams
ir_cidr_blocks k atom = fmap (\newir_cidr_blocks -> atom { _ir_cidr_blocks = newir_cidr_blocks }) (k (_ir_cidr_blocks atom))

makeIngressRuleParams :: Int -> Int -> T.Text -> IngressRuleParams
makeIngressRuleParams fromPort toPort protocol = IngressRuleParams
  { _ir_from_port = fromPort
  , _ir_to_port = toPort
  , _ir_protocol = protocol
  , _ir_cidr_blocks = []
  }

instance ToResourceFieldMap IngressRuleParams where
  toResourceFieldMap params
    =  rfmField "from_port" (_ir_from_port params)
    <> rfmField "to_port" (_ir_to_port params)
    <> rfmField "protocol" (_ir_protocol params)
    <> rfmOptionalDefField "cidr_blocks" [] (_ir_cidr_blocks params)
    

instance ToResourceField IngressRuleParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data EgressRuleParams = EgressRuleParams
  { _er_from_port :: Int
  , _er_to_port :: Int
  , _er_protocol :: T.Text
  , _er_cidr_blocks :: [CidrBlock]
  }
  deriving (Eq)

-- er_from_port :: Lens' EgressRuleParams Int
er_from_port :: Functor f => (Int -> f (Int)) -> EgressRuleParams -> f EgressRuleParams
er_from_port k atom = fmap (\newer_from_port -> atom { _er_from_port = newer_from_port }) (k (_er_from_port atom))
-- er_to_port :: Lens' EgressRuleParams Int
er_to_port :: Functor f => (Int -> f (Int)) -> EgressRuleParams -> f EgressRuleParams
er_to_port k atom = fmap (\newer_to_port -> atom { _er_to_port = newer_to_port }) (k (_er_to_port atom))
-- er_protocol :: Lens' EgressRuleParams T.Text
er_protocol :: Functor f => (T.Text -> f (T.Text)) -> EgressRuleParams -> f EgressRuleParams
er_protocol k atom = fmap (\newer_protocol -> atom { _er_protocol = newer_protocol }) (k (_er_protocol atom))
-- er_cidr_blocks :: Lens' EgressRuleParams [CidrBlock]
er_cidr_blocks :: Functor f => ([CidrBlock] -> f ([CidrBlock])) -> EgressRuleParams -> f EgressRuleParams
er_cidr_blocks k atom = fmap (\newer_cidr_blocks -> atom { _er_cidr_blocks = newer_cidr_blocks }) (k (_er_cidr_blocks atom))

makeEgressRuleParams :: Int -> Int -> T.Text -> EgressRuleParams
makeEgressRuleParams fromPort toPort protocol = EgressRuleParams
  { _er_from_port = fromPort
  , _er_to_port = toPort
  , _er_protocol = protocol
  , _er_cidr_blocks = []
  }

instance ToResourceFieldMap EgressRuleParams where
  toResourceFieldMap params
    =  rfmField "from_port" (_er_from_port params)
    <> rfmField "to_port" (_er_to_port params)
    <> rfmField "protocol" (_er_protocol params)
    <> rfmOptionalDefField "cidr_blocks" [] (_er_cidr_blocks params)
    

instance ToResourceField EgressRuleParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsSecurityGroup to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/security_group.html aws_security_group> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'sg_')

awsSecurityGroup :: NameElement -> (AwsSecurityGroupParams -> AwsSecurityGroupParams) -> TF AwsSecurityGroup
awsSecurityGroup name0  modf = newAwsSecurityGroup name0 (modf (makeAwsSecurityGroupParams ))

awsSecurityGroup' :: NameElement ->  TF AwsSecurityGroup
awsSecurityGroup' name0  = newAwsSecurityGroup name0 (makeAwsSecurityGroupParams )

newAwsSecurityGroup :: NameElement -> AwsSecurityGroupParams -> TF AwsSecurityGroup
newAwsSecurityGroup name0 params = do
  rid <- mkResource "aws_security_group" name0 (toResourceFieldMap params)
  return AwsSecurityGroup
    { sg_id = resourceAttr rid "id"
    , sg_owner_id = resourceAttr rid "owner_id"
    , sg_resource = rid
    }

data AwsSecurityGroup = AwsSecurityGroup
  { sg_id :: TFRef (AwsId AwsSecurityGroup)
  , sg_owner_id :: TFRef T.Text
  , sg_resource :: ResourceId
  }

instance IsResource AwsSecurityGroup where
  resourceId = sg_resource

data AwsSecurityGroupParams = AwsSecurityGroupParams
  { _sg_name :: T.Text
  , _sg_name_prefix :: T.Text
  , _sg_description :: T.Text
  , _sg_ingress :: [IngressRuleParams]
  , _sg_egress :: [EgressRuleParams]
  , _sg_vpc_id :: Maybe (TFRef (AwsId AwsVpc))
  , _sg_tags :: M.Map T.Text T.Text
  }

-- sg_name :: Lens' AwsSecurityGroupParams T.Text
sg_name :: Functor f => (T.Text -> f (T.Text)) -> AwsSecurityGroupParams -> f AwsSecurityGroupParams
sg_name k atom = fmap (\newsg_name -> atom { _sg_name = newsg_name }) (k (_sg_name atom))
-- sg_name_prefix :: Lens' AwsSecurityGroupParams T.Text
sg_name_prefix :: Functor f => (T.Text -> f (T.Text)) -> AwsSecurityGroupParams -> f AwsSecurityGroupParams
sg_name_prefix k atom = fmap (\newsg_name_prefix -> atom { _sg_name_prefix = newsg_name_prefix }) (k (_sg_name_prefix atom))
-- sg_description :: Lens' AwsSecurityGroupParams T.Text
sg_description :: Functor f => (T.Text -> f (T.Text)) -> AwsSecurityGroupParams -> f AwsSecurityGroupParams
sg_description k atom = fmap (\newsg_description -> atom { _sg_description = newsg_description }) (k (_sg_description atom))
-- sg_ingress :: Lens' AwsSecurityGroupParams [IngressRuleParams]
sg_ingress :: Functor f => ([IngressRuleParams] -> f ([IngressRuleParams])) -> AwsSecurityGroupParams -> f AwsSecurityGroupParams
sg_ingress k atom = fmap (\newsg_ingress -> atom { _sg_ingress = newsg_ingress }) (k (_sg_ingress atom))
-- sg_egress :: Lens' AwsSecurityGroupParams [EgressRuleParams]
sg_egress :: Functor f => ([EgressRuleParams] -> f ([EgressRuleParams])) -> AwsSecurityGroupParams -> f AwsSecurityGroupParams
sg_egress k atom = fmap (\newsg_egress -> atom { _sg_egress = newsg_egress }) (k (_sg_egress atom))
-- sg_vpc_id :: Lens' AwsSecurityGroupParams Maybe (TFRef (AwsId AwsVpc))
sg_vpc_id :: Functor f => (Maybe (TFRef (AwsId AwsVpc)) -> f (Maybe (TFRef (AwsId AwsVpc)))) -> AwsSecurityGroupParams -> f AwsSecurityGroupParams
sg_vpc_id k atom = fmap (\newsg_vpc_id -> atom { _sg_vpc_id = newsg_vpc_id }) (k (_sg_vpc_id atom))
-- sg_tags :: Lens' AwsSecurityGroupParams M.Map T.Text T.Text
sg_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsSecurityGroupParams -> f AwsSecurityGroupParams
sg_tags k atom = fmap (\newsg_tags -> atom { _sg_tags = newsg_tags }) (k (_sg_tags atom))

makeAwsSecurityGroupParams ::  AwsSecurityGroupParams
makeAwsSecurityGroupParams  = AwsSecurityGroupParams
  { _sg_name = ""
  , _sg_name_prefix = ""
  , _sg_description = ""
  , _sg_ingress = []
  , _sg_egress = []
  , _sg_vpc_id = Nothing
  , _sg_tags = M.empty
  }

instance ToResourceFieldMap AwsSecurityGroupParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "name" "" (_sg_name params)
    <> rfmOptionalDefField "name_prefix" "" (_sg_name_prefix params)
    <> rfmOptionalDefField "description" "" (_sg_description params)
    <> rfmOptionalDefField "ingress" [] (_sg_ingress params)
    <> rfmOptionalDefField "egress" [] (_sg_egress params)
    <> rfmOptionalField "vpc_id" (_sg_vpc_id params)
    <> rfmOptionalDefField "tags" M.empty (_sg_tags params)
    

instance ToResourceField AwsSecurityGroupParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data RootBlockDeviceParams = RootBlockDeviceParams
  { _rbd_volume_type :: VolumeType
  , _rbd_volume_size :: Maybe (Int)
  , _rbd_delete_on_termination :: Bool
  }
  deriving (Eq)

-- rbd_volume_type :: Lens' RootBlockDeviceParams VolumeType
rbd_volume_type :: Functor f => (VolumeType -> f (VolumeType)) -> RootBlockDeviceParams -> f RootBlockDeviceParams
rbd_volume_type k atom = fmap (\newrbd_volume_type -> atom { _rbd_volume_type = newrbd_volume_type }) (k (_rbd_volume_type atom))
-- rbd_volume_size :: Lens' RootBlockDeviceParams Maybe (Int)
rbd_volume_size :: Functor f => (Maybe (Int) -> f (Maybe (Int))) -> RootBlockDeviceParams -> f RootBlockDeviceParams
rbd_volume_size k atom = fmap (\newrbd_volume_size -> atom { _rbd_volume_size = newrbd_volume_size }) (k (_rbd_volume_size atom))
-- rbd_delete_on_termination :: Lens' RootBlockDeviceParams Bool
rbd_delete_on_termination :: Functor f => (Bool -> f (Bool)) -> RootBlockDeviceParams -> f RootBlockDeviceParams
rbd_delete_on_termination k atom = fmap (\newrbd_delete_on_termination -> atom { _rbd_delete_on_termination = newrbd_delete_on_termination }) (k (_rbd_delete_on_termination atom))

makeRootBlockDeviceParams ::  RootBlockDeviceParams
makeRootBlockDeviceParams  = RootBlockDeviceParams
  { _rbd_volume_type = "standard"
  , _rbd_volume_size = Nothing
  , _rbd_delete_on_termination = True
  }

instance ToResourceFieldMap RootBlockDeviceParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "volume_type" "standard" (_rbd_volume_type params)
    <> rfmOptionalField "volume_size" (_rbd_volume_size params)
    <> rfmOptionalDefField "delete_on_termination" True (_rbd_delete_on_termination params)
    

instance ToResourceField RootBlockDeviceParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsInstance to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/instance.html aws_instance> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'i_')

awsInstance :: NameElement -> Ami -> InstanceType ->(AwsInstanceParams -> AwsInstanceParams) -> TF AwsInstance
awsInstance name0 ami instanceType modf = newAwsInstance name0 (modf (makeAwsInstanceParams ami instanceType))

awsInstance' :: NameElement -> Ami -> InstanceType -> TF AwsInstance
awsInstance' name0 ami instanceType = newAwsInstance name0 (makeAwsInstanceParams ami instanceType)

newAwsInstance :: NameElement -> AwsInstanceParams -> TF AwsInstance
newAwsInstance name0 params = do
  rid <- mkResource "aws_instance" name0 (toResourceFieldMap params)
  return AwsInstance
    { i_id = resourceAttr rid "id"
    , i_public_ip = resourceAttr rid "public_ip"
    , i_private_ip = resourceAttr rid "private_ip"
    , i_resource = rid
    }

data AwsInstance = AwsInstance
  { i_id :: TFRef (AwsId AwsInstance)
  , i_public_ip :: TFRef IpAddress
  , i_private_ip :: TFRef IpAddress
  , i_resource :: ResourceId
  }

instance IsResource AwsInstance where
  resourceId = i_resource

data AwsInstanceParams = AwsInstanceParams
  { _i_ami :: Ami
  , _i_instance_type :: InstanceType
  , _i_availability_zone :: AvailabilityZone
  , _i_ebs_optimized :: Maybe (Bool)
  , _i_key_name :: Maybe (KeyName)
  , _i_monitoring :: Bool
  , _i_subnet_id :: Maybe (TFRef (AwsId AwsSubnet))
  , _i_associate_public_ip_address :: Maybe (Bool)
  , _i_root_block_device :: Maybe (RootBlockDeviceParams)
  , _i_user_data :: T.Text
  , _i_iam_instance_profile :: Maybe (TFRef (AwsId AwsIamInstanceProfile))
  , _i_vpc_security_group_ids :: [TFRef (AwsId AwsSecurityGroup)]
  , _i_tags :: M.Map T.Text T.Text
  }

-- i_ami :: Lens' AwsInstanceParams Ami
i_ami :: Functor f => (Ami -> f (Ami)) -> AwsInstanceParams -> f AwsInstanceParams
i_ami k atom = fmap (\newi_ami -> atom { _i_ami = newi_ami }) (k (_i_ami atom))
-- i_availability_zone :: Lens' AwsInstanceParams AvailabilityZone
i_availability_zone :: Functor f => (AvailabilityZone -> f (AvailabilityZone)) -> AwsInstanceParams -> f AwsInstanceParams
i_availability_zone k atom = fmap (\newi_availability_zone -> atom { _i_availability_zone = newi_availability_zone }) (k (_i_availability_zone atom))
-- i_ebs_optimized :: Lens' AwsInstanceParams Maybe (Bool)
i_ebs_optimized :: Functor f => (Maybe (Bool) -> f (Maybe (Bool))) -> AwsInstanceParams -> f AwsInstanceParams
i_ebs_optimized k atom = fmap (\newi_ebs_optimized -> atom { _i_ebs_optimized = newi_ebs_optimized }) (k (_i_ebs_optimized atom))
-- i_instance_type :: Lens' AwsInstanceParams InstanceType
i_instance_type :: Functor f => (InstanceType -> f (InstanceType)) -> AwsInstanceParams -> f AwsInstanceParams
i_instance_type k atom = fmap (\newi_instance_type -> atom { _i_instance_type = newi_instance_type }) (k (_i_instance_type atom))
-- i_key_name :: Lens' AwsInstanceParams Maybe (KeyName)
i_key_name :: Functor f => (Maybe (KeyName) -> f (Maybe (KeyName))) -> AwsInstanceParams -> f AwsInstanceParams
i_key_name k atom = fmap (\newi_key_name -> atom { _i_key_name = newi_key_name }) (k (_i_key_name atom))
-- i_monitoring :: Lens' AwsInstanceParams Bool
i_monitoring :: Functor f => (Bool -> f (Bool)) -> AwsInstanceParams -> f AwsInstanceParams
i_monitoring k atom = fmap (\newi_monitoring -> atom { _i_monitoring = newi_monitoring }) (k (_i_monitoring atom))
-- i_subnet_id :: Lens' AwsInstanceParams Maybe (TFRef (AwsId AwsSubnet))
i_subnet_id :: Functor f => (Maybe (TFRef (AwsId AwsSubnet)) -> f (Maybe (TFRef (AwsId AwsSubnet)))) -> AwsInstanceParams -> f AwsInstanceParams
i_subnet_id k atom = fmap (\newi_subnet_id -> atom { _i_subnet_id = newi_subnet_id }) (k (_i_subnet_id atom))
-- i_associate_public_ip_address :: Lens' AwsInstanceParams Maybe (Bool)
i_associate_public_ip_address :: Functor f => (Maybe (Bool) -> f (Maybe (Bool))) -> AwsInstanceParams -> f AwsInstanceParams
i_associate_public_ip_address k atom = fmap (\newi_associate_public_ip_address -> atom { _i_associate_public_ip_address = newi_associate_public_ip_address }) (k (_i_associate_public_ip_address atom))
-- i_root_block_device :: Lens' AwsInstanceParams Maybe (RootBlockDeviceParams)
i_root_block_device :: Functor f => (Maybe (RootBlockDeviceParams) -> f (Maybe (RootBlockDeviceParams))) -> AwsInstanceParams -> f AwsInstanceParams
i_root_block_device k atom = fmap (\newi_root_block_device -> atom { _i_root_block_device = newi_root_block_device }) (k (_i_root_block_device atom))
-- i_user_data :: Lens' AwsInstanceParams T.Text
i_user_data :: Functor f => (T.Text -> f (T.Text)) -> AwsInstanceParams -> f AwsInstanceParams
i_user_data k atom = fmap (\newi_user_data -> atom { _i_user_data = newi_user_data }) (k (_i_user_data atom))
-- i_iam_instance_profile :: Lens' AwsInstanceParams Maybe (TFRef (AwsId AwsIamInstanceProfile))
i_iam_instance_profile :: Functor f => (Maybe (TFRef (AwsId AwsIamInstanceProfile)) -> f (Maybe (TFRef (AwsId AwsIamInstanceProfile)))) -> AwsInstanceParams -> f AwsInstanceParams
i_iam_instance_profile k atom = fmap (\newi_iam_instance_profile -> atom { _i_iam_instance_profile = newi_iam_instance_profile }) (k (_i_iam_instance_profile atom))
-- i_vpc_security_group_ids :: Lens' AwsInstanceParams [TFRef (AwsId AwsSecurityGroup)]
i_vpc_security_group_ids :: Functor f => ([TFRef (AwsId AwsSecurityGroup)] -> f ([TFRef (AwsId AwsSecurityGroup)])) -> AwsInstanceParams -> f AwsInstanceParams
i_vpc_security_group_ids k atom = fmap (\newi_vpc_security_group_ids -> atom { _i_vpc_security_group_ids = newi_vpc_security_group_ids }) (k (_i_vpc_security_group_ids atom))
-- i_tags :: Lens' AwsInstanceParams M.Map T.Text T.Text
i_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsInstanceParams -> f AwsInstanceParams
i_tags k atom = fmap (\newi_tags -> atom { _i_tags = newi_tags }) (k (_i_tags atom))

makeAwsInstanceParams :: Ami -> InstanceType -> AwsInstanceParams
makeAwsInstanceParams ami instanceType = AwsInstanceParams
  { _i_ami = ami
  , _i_instance_type = instanceType
  , _i_availability_zone = ""
  , _i_ebs_optimized = Nothing
  , _i_key_name = Nothing
  , _i_monitoring = True
  , _i_subnet_id = Nothing
  , _i_associate_public_ip_address = Nothing
  , _i_root_block_device = Nothing
  , _i_user_data = ""
  , _i_iam_instance_profile = Nothing
  , _i_vpc_security_group_ids = []
  , _i_tags = M.empty
  }

instance ToResourceFieldMap AwsInstanceParams where
  toResourceFieldMap params
    =  rfmField "ami" (_i_ami params)
    <> rfmOptionalDefField "availability_zone" "" (_i_availability_zone params)
    <> rfmOptionalField "ebs_optimized" (_i_ebs_optimized params)
    <> rfmField "instance_type" (_i_instance_type params)
    <> rfmOptionalField "key_name" (_i_key_name params)
    <> rfmOptionalDefField "monitoring" True (_i_monitoring params)
    <> rfmOptionalField "subnet_id" (_i_subnet_id params)
    <> rfmOptionalField "associate_public_ip_address" (_i_associate_public_ip_address params)
    <> rfmOptionalField "root_block_device" (_i_root_block_device params)
    <> rfmOptionalDefField "user_data" "" (_i_user_data params)
    <> rfmOptionalField "iam_instance_profile" (_i_iam_instance_profile params)
    <> rfmOptionalDefField "vpc_security_group_ids" [] (_i_vpc_security_group_ids params)
    <> rfmOptionalDefField "tags" M.empty (_i_tags params)
    

instance ToResourceField AwsInstanceParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsLaunchConfiguration to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/launch_configuration.html aws_launch_configuration> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'lc_')

awsLaunchConfiguration :: NameElement -> Ami -> InstanceType ->(AwsLaunchConfigurationParams -> AwsLaunchConfigurationParams) -> TF AwsLaunchConfiguration
awsLaunchConfiguration name0 imageId instanceType modf = newAwsLaunchConfiguration name0 (modf (makeAwsLaunchConfigurationParams imageId instanceType))

awsLaunchConfiguration' :: NameElement -> Ami -> InstanceType -> TF AwsLaunchConfiguration
awsLaunchConfiguration' name0 imageId instanceType = newAwsLaunchConfiguration name0 (makeAwsLaunchConfigurationParams imageId instanceType)

newAwsLaunchConfiguration :: NameElement -> AwsLaunchConfigurationParams -> TF AwsLaunchConfiguration
newAwsLaunchConfiguration name0 params = do
  rid <- mkResource "aws_launch_configuration" name0 (toResourceFieldMap params)
  return AwsLaunchConfiguration
    { lc_id = resourceAttr rid "id"
    , lc_name = resourceAttr rid "name"
    , lc_resource = rid
    }

data AwsLaunchConfiguration = AwsLaunchConfiguration
  { lc_id :: TFRef (AwsId AwsLaunchConfiguration)
  , lc_name :: TFRef T.Text
  , lc_resource :: ResourceId
  }

instance IsResource AwsLaunchConfiguration where
  resourceId = lc_resource

data AwsLaunchConfigurationParams = AwsLaunchConfigurationParams
  { _lc_image_id :: Ami
  , _lc_instance_type :: InstanceType
  , _lc_name' :: T.Text
  , _lc_name_prefix :: T.Text
  , _lc_iam_instance_profile :: Maybe (TFRef (AwsId AwsIamInstanceProfile))
  , _lc_key_name :: Maybe (KeyName)
  , _lc_security_groups :: [TFRef (AwsId AwsSecurityGroup)]
  , _lc_associate_public_ip_address :: Maybe (Bool)
  , _lc_user_data :: T.Text
  , _lc_enable_monitoring :: Bool
  , _lc_ebs_optimized :: Maybe (Bool)
  , _lc_root_block_device :: Maybe (RootBlockDeviceParams)
  }

-- lc_name' :: Lens' AwsLaunchConfigurationParams T.Text
lc_name' :: Functor f => (T.Text -> f (T.Text)) -> AwsLaunchConfigurationParams -> f AwsLaunchConfigurationParams
lc_name' k atom = fmap (\newlc_name' -> atom { _lc_name' = newlc_name' }) (k (_lc_name' atom))
-- lc_name_prefix :: Lens' AwsLaunchConfigurationParams T.Text
lc_name_prefix :: Functor f => (T.Text -> f (T.Text)) -> AwsLaunchConfigurationParams -> f AwsLaunchConfigurationParams
lc_name_prefix k atom = fmap (\newlc_name_prefix -> atom { _lc_name_prefix = newlc_name_prefix }) (k (_lc_name_prefix atom))
-- lc_image_id :: Lens' AwsLaunchConfigurationParams Ami
lc_image_id :: Functor f => (Ami -> f (Ami)) -> AwsLaunchConfigurationParams -> f AwsLaunchConfigurationParams
lc_image_id k atom = fmap (\newlc_image_id -> atom { _lc_image_id = newlc_image_id }) (k (_lc_image_id atom))
-- lc_instance_type :: Lens' AwsLaunchConfigurationParams InstanceType
lc_instance_type :: Functor f => (InstanceType -> f (InstanceType)) -> AwsLaunchConfigurationParams -> f AwsLaunchConfigurationParams
lc_instance_type k atom = fmap (\newlc_instance_type -> atom { _lc_instance_type = newlc_instance_type }) (k (_lc_instance_type atom))
-- lc_iam_instance_profile :: Lens' AwsLaunchConfigurationParams Maybe (TFRef (AwsId AwsIamInstanceProfile))
lc_iam_instance_profile :: Functor f => (Maybe (TFRef (AwsId AwsIamInstanceProfile)) -> f (Maybe (TFRef (AwsId AwsIamInstanceProfile)))) -> AwsLaunchConfigurationParams -> f AwsLaunchConfigurationParams
lc_iam_instance_profile k atom = fmap (\newlc_iam_instance_profile -> atom { _lc_iam_instance_profile = newlc_iam_instance_profile }) (k (_lc_iam_instance_profile atom))
-- lc_key_name :: Lens' AwsLaunchConfigurationParams Maybe (KeyName)
lc_key_name :: Functor f => (Maybe (KeyName) -> f (Maybe (KeyName))) -> AwsLaunchConfigurationParams -> f AwsLaunchConfigurationParams
lc_key_name k atom = fmap (\newlc_key_name -> atom { _lc_key_name = newlc_key_name }) (k (_lc_key_name atom))
-- lc_security_groups :: Lens' AwsLaunchConfigurationParams [TFRef (AwsId AwsSecurityGroup)]
lc_security_groups :: Functor f => ([TFRef (AwsId AwsSecurityGroup)] -> f ([TFRef (AwsId AwsSecurityGroup)])) -> AwsLaunchConfigurationParams -> f AwsLaunchConfigurationParams
lc_security_groups k atom = fmap (\newlc_security_groups -> atom { _lc_security_groups = newlc_security_groups }) (k (_lc_security_groups atom))
-- lc_associate_public_ip_address :: Lens' AwsLaunchConfigurationParams Maybe (Bool)
lc_associate_public_ip_address :: Functor f => (Maybe (Bool) -> f (Maybe (Bool))) -> AwsLaunchConfigurationParams -> f AwsLaunchConfigurationParams
lc_associate_public_ip_address k atom = fmap (\newlc_associate_public_ip_address -> atom { _lc_associate_public_ip_address = newlc_associate_public_ip_address }) (k (_lc_associate_public_ip_address atom))
-- lc_user_data :: Lens' AwsLaunchConfigurationParams T.Text
lc_user_data :: Functor f => (T.Text -> f (T.Text)) -> AwsLaunchConfigurationParams -> f AwsLaunchConfigurationParams
lc_user_data k atom = fmap (\newlc_user_data -> atom { _lc_user_data = newlc_user_data }) (k (_lc_user_data atom))
-- lc_enable_monitoring :: Lens' AwsLaunchConfigurationParams Bool
lc_enable_monitoring :: Functor f => (Bool -> f (Bool)) -> AwsLaunchConfigurationParams -> f AwsLaunchConfigurationParams
lc_enable_monitoring k atom = fmap (\newlc_enable_monitoring -> atom { _lc_enable_monitoring = newlc_enable_monitoring }) (k (_lc_enable_monitoring atom))
-- lc_ebs_optimized :: Lens' AwsLaunchConfigurationParams Maybe (Bool)
lc_ebs_optimized :: Functor f => (Maybe (Bool) -> f (Maybe (Bool))) -> AwsLaunchConfigurationParams -> f AwsLaunchConfigurationParams
lc_ebs_optimized k atom = fmap (\newlc_ebs_optimized -> atom { _lc_ebs_optimized = newlc_ebs_optimized }) (k (_lc_ebs_optimized atom))
-- lc_root_block_device :: Lens' AwsLaunchConfigurationParams Maybe (RootBlockDeviceParams)
lc_root_block_device :: Functor f => (Maybe (RootBlockDeviceParams) -> f (Maybe (RootBlockDeviceParams))) -> AwsLaunchConfigurationParams -> f AwsLaunchConfigurationParams
lc_root_block_device k atom = fmap (\newlc_root_block_device -> atom { _lc_root_block_device = newlc_root_block_device }) (k (_lc_root_block_device atom))

makeAwsLaunchConfigurationParams :: Ami -> InstanceType -> AwsLaunchConfigurationParams
makeAwsLaunchConfigurationParams imageId instanceType = AwsLaunchConfigurationParams
  { _lc_image_id = imageId
  , _lc_instance_type = instanceType
  , _lc_name' = ""
  , _lc_name_prefix = ""
  , _lc_iam_instance_profile = Nothing
  , _lc_key_name = Nothing
  , _lc_security_groups = []
  , _lc_associate_public_ip_address = Nothing
  , _lc_user_data = ""
  , _lc_enable_monitoring = True
  , _lc_ebs_optimized = Nothing
  , _lc_root_block_device = Nothing
  }

instance ToResourceFieldMap AwsLaunchConfigurationParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "name" "" (_lc_name' params)
    <> rfmOptionalDefField "name_prefix" "" (_lc_name_prefix params)
    <> rfmField "image_id" (_lc_image_id params)
    <> rfmField "instance_type" (_lc_instance_type params)
    <> rfmOptionalField "iam_instance_profile" (_lc_iam_instance_profile params)
    <> rfmOptionalField "key_name" (_lc_key_name params)
    <> rfmOptionalDefField "security_groups" [] (_lc_security_groups params)
    <> rfmOptionalField "associate_public_ip_address" (_lc_associate_public_ip_address params)
    <> rfmOptionalDefField "user_data" "" (_lc_user_data params)
    <> rfmOptionalDefField "enable_monitoring" True (_lc_enable_monitoring params)
    <> rfmOptionalField "ebs_optimized" (_lc_ebs_optimized params)
    <> rfmOptionalField "root_block_device" (_lc_root_block_device params)
    

instance ToResourceField AwsLaunchConfigurationParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsAutoscalingGroup to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/autoscaling_group.html aws_autoscaling_group> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'ag_')

awsAutoscalingGroup :: NameElement -> Int -> Int -> TFRef T.Text ->(AwsAutoscalingGroupParams -> AwsAutoscalingGroupParams) -> TF AwsAutoscalingGroup
awsAutoscalingGroup name0 minSize maxSize launchConfiguration modf = newAwsAutoscalingGroup name0 (modf (makeAwsAutoscalingGroupParams minSize maxSize launchConfiguration))

awsAutoscalingGroup' :: NameElement -> Int -> Int -> TFRef T.Text -> TF AwsAutoscalingGroup
awsAutoscalingGroup' name0 minSize maxSize launchConfiguration = newAwsAutoscalingGroup name0 (makeAwsAutoscalingGroupParams minSize maxSize launchConfiguration)

newAwsAutoscalingGroup :: NameElement -> AwsAutoscalingGroupParams -> TF AwsAutoscalingGroup
newAwsAutoscalingGroup name0 params = do
  rid <- mkResource "aws_autoscaling_group" name0 (toResourceFieldMap params)
  return AwsAutoscalingGroup
    { ag_id = resourceAttr rid "id"
    , ag_arn = resourceAttr rid "arn"
    , ag_name = resourceAttr rid "name"
    , ag_resource = rid
    }

data AwsAutoscalingGroup = AwsAutoscalingGroup
  { ag_id :: TFRef (AwsId AwsAutoscalingGroup)
  , ag_arn :: TFRef Arn
  , ag_name :: TFRef T.Text
  , ag_resource :: ResourceId
  }

instance IsResource AwsAutoscalingGroup where
  resourceId = ag_resource

data AwsAutoscalingGroupParams = AwsAutoscalingGroupParams
  { _ag_min_size :: Int
  , _ag_max_size :: Int
  , _ag_launch_configuration :: TFRef T.Text
  , _ag_name' :: T.Text
  , _ag_name_prefix :: T.Text
  , _ag_vpc_zone_identifier :: [TFRef (AwsId AwsSubnet)]
  , _ag_load_balancers :: [TFRef T.Text]
  , _ag_tag :: [AsgTagParams]
  }

-- ag_name' :: Lens' AwsAutoscalingGroupParams T.Text
ag_name' :: Functor f => (T.Text -> f (T.Text)) -> AwsAutoscalingGroupParams -> f AwsAutoscalingGroupParams
ag_name' k atom = fmap (\newag_name' -> atom { _ag_name' = newag_name' }) (k (_ag_name' atom))
-- ag_name_prefix :: Lens' AwsAutoscalingGroupParams T.Text
ag_name_prefix :: Functor f => (T.Text -> f (T.Text)) -> AwsAutoscalingGroupParams -> f AwsAutoscalingGroupParams
ag_name_prefix k atom = fmap (\newag_name_prefix -> atom { _ag_name_prefix = newag_name_prefix }) (k (_ag_name_prefix atom))
-- ag_min_size :: Lens' AwsAutoscalingGroupParams Int
ag_min_size :: Functor f => (Int -> f (Int)) -> AwsAutoscalingGroupParams -> f AwsAutoscalingGroupParams
ag_min_size k atom = fmap (\newag_min_size -> atom { _ag_min_size = newag_min_size }) (k (_ag_min_size atom))
-- ag_max_size :: Lens' AwsAutoscalingGroupParams Int
ag_max_size :: Functor f => (Int -> f (Int)) -> AwsAutoscalingGroupParams -> f AwsAutoscalingGroupParams
ag_max_size k atom = fmap (\newag_max_size -> atom { _ag_max_size = newag_max_size }) (k (_ag_max_size atom))
-- ag_vpc_zone_identifier :: Lens' AwsAutoscalingGroupParams [TFRef (AwsId AwsSubnet)]
ag_vpc_zone_identifier :: Functor f => ([TFRef (AwsId AwsSubnet)] -> f ([TFRef (AwsId AwsSubnet)])) -> AwsAutoscalingGroupParams -> f AwsAutoscalingGroupParams
ag_vpc_zone_identifier k atom = fmap (\newag_vpc_zone_identifier -> atom { _ag_vpc_zone_identifier = newag_vpc_zone_identifier }) (k (_ag_vpc_zone_identifier atom))
-- ag_launch_configuration :: Lens' AwsAutoscalingGroupParams TFRef T.Text
ag_launch_configuration :: Functor f => (TFRef T.Text -> f (TFRef T.Text)) -> AwsAutoscalingGroupParams -> f AwsAutoscalingGroupParams
ag_launch_configuration k atom = fmap (\newag_launch_configuration -> atom { _ag_launch_configuration = newag_launch_configuration }) (k (_ag_launch_configuration atom))
-- ag_load_balancers :: Lens' AwsAutoscalingGroupParams [TFRef T.Text]
ag_load_balancers :: Functor f => ([TFRef T.Text] -> f ([TFRef T.Text])) -> AwsAutoscalingGroupParams -> f AwsAutoscalingGroupParams
ag_load_balancers k atom = fmap (\newag_load_balancers -> atom { _ag_load_balancers = newag_load_balancers }) (k (_ag_load_balancers atom))
-- ag_tag :: Lens' AwsAutoscalingGroupParams [AsgTagParams]
ag_tag :: Functor f => ([AsgTagParams] -> f ([AsgTagParams])) -> AwsAutoscalingGroupParams -> f AwsAutoscalingGroupParams
ag_tag k atom = fmap (\newag_tag -> atom { _ag_tag = newag_tag }) (k (_ag_tag atom))

makeAwsAutoscalingGroupParams :: Int -> Int -> TFRef T.Text -> AwsAutoscalingGroupParams
makeAwsAutoscalingGroupParams minSize maxSize launchConfiguration = AwsAutoscalingGroupParams
  { _ag_min_size = minSize
  , _ag_max_size = maxSize
  , _ag_launch_configuration = launchConfiguration
  , _ag_name' = ""
  , _ag_name_prefix = ""
  , _ag_vpc_zone_identifier = []
  , _ag_load_balancers = []
  , _ag_tag = []
  }

instance ToResourceFieldMap AwsAutoscalingGroupParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "name" "" (_ag_name' params)
    <> rfmOptionalDefField "name_prefix" "" (_ag_name_prefix params)
    <> rfmField "min_size" (_ag_min_size params)
    <> rfmField "max_size" (_ag_max_size params)
    <> rfmOptionalDefField "vpc_zone_identifier" [] (_ag_vpc_zone_identifier params)
    <> rfmField "launch_configuration" (_ag_launch_configuration params)
    <> rfmOptionalDefField "load_balancers" [] (_ag_load_balancers params)
    <> rfmExpandedList "tag" (_ag_tag params)
    

instance ToResourceField AwsAutoscalingGroupParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data AsgTagParams = AsgTagParams
  { _asg_key :: T.Text
  , _asg_value :: T.Text
  , _asg_propagate_at_launch :: Bool
  }
  deriving (Eq)

-- asg_key :: Lens' AsgTagParams T.Text
asg_key :: Functor f => (T.Text -> f (T.Text)) -> AsgTagParams -> f AsgTagParams
asg_key k atom = fmap (\newasg_key -> atom { _asg_key = newasg_key }) (k (_asg_key atom))
-- asg_value :: Lens' AsgTagParams T.Text
asg_value :: Functor f => (T.Text -> f (T.Text)) -> AsgTagParams -> f AsgTagParams
asg_value k atom = fmap (\newasg_value -> atom { _asg_value = newasg_value }) (k (_asg_value atom))
-- asg_propagate_at_launch :: Lens' AsgTagParams Bool
asg_propagate_at_launch :: Functor f => (Bool -> f (Bool)) -> AsgTagParams -> f AsgTagParams
asg_propagate_at_launch k atom = fmap (\newasg_propagate_at_launch -> atom { _asg_propagate_at_launch = newasg_propagate_at_launch }) (k (_asg_propagate_at_launch atom))

makeAsgTagParams :: T.Text -> T.Text -> Bool -> AsgTagParams
makeAsgTagParams key value propagateAtLaunch = AsgTagParams
  { _asg_key = key
  , _asg_value = value
  , _asg_propagate_at_launch = propagateAtLaunch
  }

instance ToResourceFieldMap AsgTagParams where
  toResourceFieldMap params
    =  rfmField "key" (_asg_key params)
    <> rfmField "value" (_asg_value params)
    <> rfmField "propagate_at_launch" (_asg_propagate_at_launch params)
    

instance ToResourceField AsgTagParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsEip to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/eip.html aws_eip> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'eip_')

awsEip :: NameElement -> (AwsEipParams -> AwsEipParams) -> TF AwsEip
awsEip name0  modf = newAwsEip name0 (modf (makeAwsEipParams ))

awsEip' :: NameElement ->  TF AwsEip
awsEip' name0  = newAwsEip name0 (makeAwsEipParams )

newAwsEip :: NameElement -> AwsEipParams -> TF AwsEip
newAwsEip name0 params = do
  rid <- mkResource "aws_eip" name0 (toResourceFieldMap params)
  return AwsEip
    { eip_id = resourceAttr rid "id"
    , eip_private_ip = resourceAttr rid "private_ip"
    , eip_public_ip = resourceAttr rid "public_ip"
    , eip_resource = rid
    }

data AwsEip = AwsEip
  { eip_id :: TFRef (AwsId AwsEip)
  , eip_private_ip :: TFRef IpAddress
  , eip_public_ip :: TFRef IpAddress
  , eip_resource :: ResourceId
  }

instance IsResource AwsEip where
  resourceId = eip_resource

data AwsEipParams = AwsEipParams
  { _eip_vpc :: Bool
  , _eip_instance :: Maybe (TFRef (AwsId AwsInstance))
  }

-- eip_vpc :: Lens' AwsEipParams Bool
eip_vpc :: Functor f => (Bool -> f (Bool)) -> AwsEipParams -> f AwsEipParams
eip_vpc k atom = fmap (\neweip_vpc -> atom { _eip_vpc = neweip_vpc }) (k (_eip_vpc atom))
-- eip_instance :: Lens' AwsEipParams Maybe (TFRef (AwsId AwsInstance))
eip_instance :: Functor f => (Maybe (TFRef (AwsId AwsInstance)) -> f (Maybe (TFRef (AwsId AwsInstance)))) -> AwsEipParams -> f AwsEipParams
eip_instance k atom = fmap (\neweip_instance -> atom { _eip_instance = neweip_instance }) (k (_eip_instance atom))

makeAwsEipParams ::  AwsEipParams
makeAwsEipParams  = AwsEipParams
  { _eip_vpc = False
  , _eip_instance = Nothing
  }

instance ToResourceFieldMap AwsEipParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "vpc" False (_eip_vpc params)
    <> rfmOptionalField "instance" (_eip_instance params)
    

instance ToResourceField AwsEipParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data AccessLogsParams = AccessLogsParams
  { _al_bucket :: S3BucketName
  , _al_bucket_prefix :: S3Key
  , _al_interval :: Int
  , _al_enabled :: Bool
  }
  deriving (Eq)

-- al_bucket :: Lens' AccessLogsParams S3BucketName
al_bucket :: Functor f => (S3BucketName -> f (S3BucketName)) -> AccessLogsParams -> f AccessLogsParams
al_bucket k atom = fmap (\newal_bucket -> atom { _al_bucket = newal_bucket }) (k (_al_bucket atom))
-- al_bucket_prefix :: Lens' AccessLogsParams S3Key
al_bucket_prefix :: Functor f => (S3Key -> f (S3Key)) -> AccessLogsParams -> f AccessLogsParams
al_bucket_prefix k atom = fmap (\newal_bucket_prefix -> atom { _al_bucket_prefix = newal_bucket_prefix }) (k (_al_bucket_prefix atom))
-- al_interval :: Lens' AccessLogsParams Int
al_interval :: Functor f => (Int -> f (Int)) -> AccessLogsParams -> f AccessLogsParams
al_interval k atom = fmap (\newal_interval -> atom { _al_interval = newal_interval }) (k (_al_interval atom))
-- al_enabled :: Lens' AccessLogsParams Bool
al_enabled :: Functor f => (Bool -> f (Bool)) -> AccessLogsParams -> f AccessLogsParams
al_enabled k atom = fmap (\newal_enabled -> atom { _al_enabled = newal_enabled }) (k (_al_enabled atom))

makeAccessLogsParams :: S3BucketName -> AccessLogsParams
makeAccessLogsParams bucket = AccessLogsParams
  { _al_bucket = bucket
  , _al_bucket_prefix = ""
  , _al_interval = 60
  , _al_enabled = True
  }

instance ToResourceFieldMap AccessLogsParams where
  toResourceFieldMap params
    =  rfmField "bucket" (_al_bucket params)
    <> rfmOptionalDefField "bucket_prefix" "" (_al_bucket_prefix params)
    <> rfmOptionalDefField "interval" 60 (_al_interval params)
    <> rfmOptionalDefField "enabled" True (_al_enabled params)
    

instance ToResourceField AccessLogsParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data ListenerParams = ListenerParams
  { _l_instance_port :: Int
  , _l_instance_protocol :: T.Text
  , _l_lb_port :: Int
  , _l_lb_protocol :: T.Text
  , _l_ssl_certificate_id :: Maybe (Arn)
  }
  deriving (Eq)

-- l_instance_port :: Lens' ListenerParams Int
l_instance_port :: Functor f => (Int -> f (Int)) -> ListenerParams -> f ListenerParams
l_instance_port k atom = fmap (\newl_instance_port -> atom { _l_instance_port = newl_instance_port }) (k (_l_instance_port atom))
-- l_instance_protocol :: Lens' ListenerParams T.Text
l_instance_protocol :: Functor f => (T.Text -> f (T.Text)) -> ListenerParams -> f ListenerParams
l_instance_protocol k atom = fmap (\newl_instance_protocol -> atom { _l_instance_protocol = newl_instance_protocol }) (k (_l_instance_protocol atom))
-- l_lb_port :: Lens' ListenerParams Int
l_lb_port :: Functor f => (Int -> f (Int)) -> ListenerParams -> f ListenerParams
l_lb_port k atom = fmap (\newl_lb_port -> atom { _l_lb_port = newl_lb_port }) (k (_l_lb_port atom))
-- l_lb_protocol :: Lens' ListenerParams T.Text
l_lb_protocol :: Functor f => (T.Text -> f (T.Text)) -> ListenerParams -> f ListenerParams
l_lb_protocol k atom = fmap (\newl_lb_protocol -> atom { _l_lb_protocol = newl_lb_protocol }) (k (_l_lb_protocol atom))
-- l_ssl_certificate_id :: Lens' ListenerParams Maybe (Arn)
l_ssl_certificate_id :: Functor f => (Maybe (Arn) -> f (Maybe (Arn))) -> ListenerParams -> f ListenerParams
l_ssl_certificate_id k atom = fmap (\newl_ssl_certificate_id -> atom { _l_ssl_certificate_id = newl_ssl_certificate_id }) (k (_l_ssl_certificate_id atom))

makeListenerParams :: Int -> T.Text -> Int -> T.Text -> ListenerParams
makeListenerParams instancePort instanceProtocol lbPort lbProtocol = ListenerParams
  { _l_instance_port = instancePort
  , _l_instance_protocol = instanceProtocol
  , _l_lb_port = lbPort
  , _l_lb_protocol = lbProtocol
  , _l_ssl_certificate_id = Nothing
  }

instance ToResourceFieldMap ListenerParams where
  toResourceFieldMap params
    =  rfmField "instance_port" (_l_instance_port params)
    <> rfmField "instance_protocol" (_l_instance_protocol params)
    <> rfmField "lb_port" (_l_lb_port params)
    <> rfmField "lb_protocol" (_l_lb_protocol params)
    <> rfmOptionalField "ssl_certificate_id" (_l_ssl_certificate_id params)
    

instance ToResourceField ListenerParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data HealthCheckParams = HealthCheckParams
  { _hc_healthy_threshold :: Int
  , _hc_unhealthy_threshold :: Int
  , _hc_target :: T.Text
  , _hc_interval :: Int
  , _hc_timeout :: Int
  }
  deriving (Eq)

-- hc_healthy_threshold :: Lens' HealthCheckParams Int
hc_healthy_threshold :: Functor f => (Int -> f (Int)) -> HealthCheckParams -> f HealthCheckParams
hc_healthy_threshold k atom = fmap (\newhc_healthy_threshold -> atom { _hc_healthy_threshold = newhc_healthy_threshold }) (k (_hc_healthy_threshold atom))
-- hc_unhealthy_threshold :: Lens' HealthCheckParams Int
hc_unhealthy_threshold :: Functor f => (Int -> f (Int)) -> HealthCheckParams -> f HealthCheckParams
hc_unhealthy_threshold k atom = fmap (\newhc_unhealthy_threshold -> atom { _hc_unhealthy_threshold = newhc_unhealthy_threshold }) (k (_hc_unhealthy_threshold atom))
-- hc_target :: Lens' HealthCheckParams T.Text
hc_target :: Functor f => (T.Text -> f (T.Text)) -> HealthCheckParams -> f HealthCheckParams
hc_target k atom = fmap (\newhc_target -> atom { _hc_target = newhc_target }) (k (_hc_target atom))
-- hc_interval :: Lens' HealthCheckParams Int
hc_interval :: Functor f => (Int -> f (Int)) -> HealthCheckParams -> f HealthCheckParams
hc_interval k atom = fmap (\newhc_interval -> atom { _hc_interval = newhc_interval }) (k (_hc_interval atom))
-- hc_timeout :: Lens' HealthCheckParams Int
hc_timeout :: Functor f => (Int -> f (Int)) -> HealthCheckParams -> f HealthCheckParams
hc_timeout k atom = fmap (\newhc_timeout -> atom { _hc_timeout = newhc_timeout }) (k (_hc_timeout atom))

makeHealthCheckParams :: Int -> Int -> T.Text -> Int -> Int -> HealthCheckParams
makeHealthCheckParams healthyThreshold unhealthyThreshold target interval timeout = HealthCheckParams
  { _hc_healthy_threshold = healthyThreshold
  , _hc_unhealthy_threshold = unhealthyThreshold
  , _hc_target = target
  , _hc_interval = interval
  , _hc_timeout = timeout
  }

instance ToResourceFieldMap HealthCheckParams where
  toResourceFieldMap params
    =  rfmField "healthy_threshold" (_hc_healthy_threshold params)
    <> rfmField "unhealthy_threshold" (_hc_unhealthy_threshold params)
    <> rfmField "target" (_hc_target params)
    <> rfmField "interval" (_hc_interval params)
    <> rfmField "timeout" (_hc_timeout params)
    

instance ToResourceField HealthCheckParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsElb to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/elb.html aws_elb> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'elb_')

awsElb :: NameElement -> [ListenerParams] ->(AwsElbParams -> AwsElbParams) -> TF AwsElb
awsElb name0 listener modf = newAwsElb name0 (modf (makeAwsElbParams listener))

awsElb' :: NameElement -> [ListenerParams] -> TF AwsElb
awsElb' name0 listener = newAwsElb name0 (makeAwsElbParams listener)

newAwsElb :: NameElement -> AwsElbParams -> TF AwsElb
newAwsElb name0 params = do
  rid <- mkResource "aws_elb" name0 (toResourceFieldMap params)
  return AwsElb
    { elb_id = resourceAttr rid "id"
    , elb_name = resourceAttr rid "name"
    , elb_dns_name = resourceAttr rid "dns_name"
    , elb_zone_id = resourceAttr rid "zone_id"
    , elb_resource = rid
    }

data AwsElb = AwsElb
  { elb_id :: TFRef T.Text
  , elb_name :: TFRef T.Text
  , elb_dns_name :: TFRef T.Text
  , elb_zone_id :: TFRef T.Text
  , elb_resource :: ResourceId
  }

instance IsResource AwsElb where
  resourceId = elb_resource

data AwsElbParams = AwsElbParams
  { _elb_listener :: [ListenerParams]
  , _elb_name' :: Maybe (T.Text)
  , _elb_access_logs :: Maybe (AccessLogsParams)
  , _elb_security_groups :: [TFRef (AwsId AwsSecurityGroup)]
  , _elb_subnets :: [TFRef (AwsId AwsSubnet)]
  , _elb_instances :: [TFRef (AwsId AwsInstance)]
  , _elb_health_check :: Maybe (HealthCheckParams)
  , _elb_tags :: M.Map T.Text T.Text
  }

-- elb_name' :: Lens' AwsElbParams Maybe (T.Text)
elb_name' :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsElbParams -> f AwsElbParams
elb_name' k atom = fmap (\newelb_name' -> atom { _elb_name' = newelb_name' }) (k (_elb_name' atom))
-- elb_access_logs :: Lens' AwsElbParams Maybe (AccessLogsParams)
elb_access_logs :: Functor f => (Maybe (AccessLogsParams) -> f (Maybe (AccessLogsParams))) -> AwsElbParams -> f AwsElbParams
elb_access_logs k atom = fmap (\newelb_access_logs -> atom { _elb_access_logs = newelb_access_logs }) (k (_elb_access_logs atom))
-- elb_security_groups :: Lens' AwsElbParams [TFRef (AwsId AwsSecurityGroup)]
elb_security_groups :: Functor f => ([TFRef (AwsId AwsSecurityGroup)] -> f ([TFRef (AwsId AwsSecurityGroup)])) -> AwsElbParams -> f AwsElbParams
elb_security_groups k atom = fmap (\newelb_security_groups -> atom { _elb_security_groups = newelb_security_groups }) (k (_elb_security_groups atom))
-- elb_subnets :: Lens' AwsElbParams [TFRef (AwsId AwsSubnet)]
elb_subnets :: Functor f => ([TFRef (AwsId AwsSubnet)] -> f ([TFRef (AwsId AwsSubnet)])) -> AwsElbParams -> f AwsElbParams
elb_subnets k atom = fmap (\newelb_subnets -> atom { _elb_subnets = newelb_subnets }) (k (_elb_subnets atom))
-- elb_instances :: Lens' AwsElbParams [TFRef (AwsId AwsInstance)]
elb_instances :: Functor f => ([TFRef (AwsId AwsInstance)] -> f ([TFRef (AwsId AwsInstance)])) -> AwsElbParams -> f AwsElbParams
elb_instances k atom = fmap (\newelb_instances -> atom { _elb_instances = newelb_instances }) (k (_elb_instances atom))
-- elb_listener :: Lens' AwsElbParams [ListenerParams]
elb_listener :: Functor f => ([ListenerParams] -> f ([ListenerParams])) -> AwsElbParams -> f AwsElbParams
elb_listener k atom = fmap (\newelb_listener -> atom { _elb_listener = newelb_listener }) (k (_elb_listener atom))
-- elb_health_check :: Lens' AwsElbParams Maybe (HealthCheckParams)
elb_health_check :: Functor f => (Maybe (HealthCheckParams) -> f (Maybe (HealthCheckParams))) -> AwsElbParams -> f AwsElbParams
elb_health_check k atom = fmap (\newelb_health_check -> atom { _elb_health_check = newelb_health_check }) (k (_elb_health_check atom))
-- elb_tags :: Lens' AwsElbParams M.Map T.Text T.Text
elb_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsElbParams -> f AwsElbParams
elb_tags k atom = fmap (\newelb_tags -> atom { _elb_tags = newelb_tags }) (k (_elb_tags atom))

makeAwsElbParams :: [ListenerParams] -> AwsElbParams
makeAwsElbParams listener = AwsElbParams
  { _elb_listener = listener
  , _elb_name' = Nothing
  , _elb_access_logs = Nothing
  , _elb_security_groups = []
  , _elb_subnets = []
  , _elb_instances = []
  , _elb_health_check = Nothing
  , _elb_tags = M.empty
  }

instance ToResourceFieldMap AwsElbParams where
  toResourceFieldMap params
    =  rfmOptionalField "name" (_elb_name' params)
    <> rfmOptionalField "access_logs" (_elb_access_logs params)
    <> rfmOptionalDefField "security_groups" [] (_elb_security_groups params)
    <> rfmOptionalDefField "subnets" [] (_elb_subnets params)
    <> rfmOptionalDefField "instances" [] (_elb_instances params)
    <> rfmField "listener" (_elb_listener params)
    <> rfmOptionalField "health_check" (_elb_health_check params)
    <> rfmOptionalDefField "tags" M.empty (_elb_tags params)
    

instance ToResourceField AwsElbParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data BucketVersioningParams = BucketVersioningParams
  { _bv_enabled :: Bool
  , _bv_mfa_delete :: Bool
  }
  deriving (Eq)

-- bv_enabled :: Lens' BucketVersioningParams Bool
bv_enabled :: Functor f => (Bool -> f (Bool)) -> BucketVersioningParams -> f BucketVersioningParams
bv_enabled k atom = fmap (\newbv_enabled -> atom { _bv_enabled = newbv_enabled }) (k (_bv_enabled atom))
-- bv_mfa_delete :: Lens' BucketVersioningParams Bool
bv_mfa_delete :: Functor f => (Bool -> f (Bool)) -> BucketVersioningParams -> f BucketVersioningParams
bv_mfa_delete k atom = fmap (\newbv_mfa_delete -> atom { _bv_mfa_delete = newbv_mfa_delete }) (k (_bv_mfa_delete atom))

makeBucketVersioningParams ::  BucketVersioningParams
makeBucketVersioningParams  = BucketVersioningParams
  { _bv_enabled = False
  , _bv_mfa_delete = False
  }

instance ToResourceFieldMap BucketVersioningParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "enabled" False (_bv_enabled params)
    <> rfmOptionalDefField "mfa_delete" False (_bv_mfa_delete params)
    

instance ToResourceField BucketVersioningParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data ExpirationParams = ExpirationParams
  { _e_days :: Maybe (Int)
  , _e_date :: Maybe (T.Text)
  , _e_expired_object_delete_marker :: Bool
  }
  deriving (Eq)

-- e_days :: Lens' ExpirationParams Maybe (Int)
e_days :: Functor f => (Maybe (Int) -> f (Maybe (Int))) -> ExpirationParams -> f ExpirationParams
e_days k atom = fmap (\newe_days -> atom { _e_days = newe_days }) (k (_e_days atom))
-- e_date :: Lens' ExpirationParams Maybe (T.Text)
e_date :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> ExpirationParams -> f ExpirationParams
e_date k atom = fmap (\newe_date -> atom { _e_date = newe_date }) (k (_e_date atom))
-- e_expired_object_delete_marker :: Lens' ExpirationParams Bool
e_expired_object_delete_marker :: Functor f => (Bool -> f (Bool)) -> ExpirationParams -> f ExpirationParams
e_expired_object_delete_marker k atom = fmap (\newe_expired_object_delete_marker -> atom { _e_expired_object_delete_marker = newe_expired_object_delete_marker }) (k (_e_expired_object_delete_marker atom))

makeExpirationParams ::  ExpirationParams
makeExpirationParams  = ExpirationParams
  { _e_days = Nothing
  , _e_date = Nothing
  , _e_expired_object_delete_marker = False
  }

instance ToResourceFieldMap ExpirationParams where
  toResourceFieldMap params
    =  rfmOptionalField "days" (_e_days params)
    <> rfmOptionalField "date" (_e_date params)
    <> rfmOptionalDefField "expired_object_delete_marker" False (_e_expired_object_delete_marker params)
    

instance ToResourceField ExpirationParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data LifecycleRuleParams = LifecycleRuleParams
  { _lr_prefix :: T.Text
  , _lr_enabled :: Bool
  , _lr_id :: Maybe (T.Text)
  , _lr_expiration :: Maybe (ExpirationParams)
  }
  deriving (Eq)

-- lr_id :: Lens' LifecycleRuleParams Maybe (T.Text)
lr_id :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> LifecycleRuleParams -> f LifecycleRuleParams
lr_id k atom = fmap (\newlr_id -> atom { _lr_id = newlr_id }) (k (_lr_id atom))
-- lr_prefix :: Lens' LifecycleRuleParams T.Text
lr_prefix :: Functor f => (T.Text -> f (T.Text)) -> LifecycleRuleParams -> f LifecycleRuleParams
lr_prefix k atom = fmap (\newlr_prefix -> atom { _lr_prefix = newlr_prefix }) (k (_lr_prefix atom))
-- lr_enabled :: Lens' LifecycleRuleParams Bool
lr_enabled :: Functor f => (Bool -> f (Bool)) -> LifecycleRuleParams -> f LifecycleRuleParams
lr_enabled k atom = fmap (\newlr_enabled -> atom { _lr_enabled = newlr_enabled }) (k (_lr_enabled atom))
-- lr_expiration :: Lens' LifecycleRuleParams Maybe (ExpirationParams)
lr_expiration :: Functor f => (Maybe (ExpirationParams) -> f (Maybe (ExpirationParams))) -> LifecycleRuleParams -> f LifecycleRuleParams
lr_expiration k atom = fmap (\newlr_expiration -> atom { _lr_expiration = newlr_expiration }) (k (_lr_expiration atom))

makeLifecycleRuleParams :: T.Text -> Bool -> LifecycleRuleParams
makeLifecycleRuleParams prefix enabled = LifecycleRuleParams
  { _lr_prefix = prefix
  , _lr_enabled = enabled
  , _lr_id = Nothing
  , _lr_expiration = Nothing
  }

instance ToResourceFieldMap LifecycleRuleParams where
  toResourceFieldMap params
    =  rfmOptionalField "id" (_lr_id params)
    <> rfmField "prefix" (_lr_prefix params)
    <> rfmField "enabled" (_lr_enabled params)
    <> rfmOptionalField "expiration" (_lr_expiration params)
    

instance ToResourceField LifecycleRuleParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsElbAttachment to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/elb_attachment.html aws_elb_attachment> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'elba_')

awsElbAttachment :: NameElement -> T.Text -> T.Text ->(AwsElbAttachmentParams -> AwsElbAttachmentParams) -> TF AwsElbAttachment
awsElbAttachment name0 elb instance_ modf = newAwsElbAttachment name0 (modf (makeAwsElbAttachmentParams elb instance_))

awsElbAttachment' :: NameElement -> T.Text -> T.Text -> TF AwsElbAttachment
awsElbAttachment' name0 elb instance_ = newAwsElbAttachment name0 (makeAwsElbAttachmentParams elb instance_)

newAwsElbAttachment :: NameElement -> AwsElbAttachmentParams -> TF AwsElbAttachment
newAwsElbAttachment name0 params = do
  rid <- mkResource "aws_elb_attachment" name0 (toResourceFieldMap params)
  return AwsElbAttachment
    { elba_resource = rid
    }

data AwsElbAttachment = AwsElbAttachment
  { elba_resource :: ResourceId
  }

instance IsResource AwsElbAttachment where
  resourceId = elba_resource

data AwsElbAttachmentParams = AwsElbAttachmentParams
  { _elba_elb :: T.Text
  , _elba_instance :: T.Text
  }

-- elba_elb :: Lens' AwsElbAttachmentParams T.Text
elba_elb :: Functor f => (T.Text -> f (T.Text)) -> AwsElbAttachmentParams -> f AwsElbAttachmentParams
elba_elb k atom = fmap (\newelba_elb -> atom { _elba_elb = newelba_elb }) (k (_elba_elb atom))
-- elba_instance :: Lens' AwsElbAttachmentParams T.Text
elba_instance :: Functor f => (T.Text -> f (T.Text)) -> AwsElbAttachmentParams -> f AwsElbAttachmentParams
elba_instance k atom = fmap (\newelba_instance -> atom { _elba_instance = newelba_instance }) (k (_elba_instance atom))

makeAwsElbAttachmentParams :: T.Text -> T.Text -> AwsElbAttachmentParams
makeAwsElbAttachmentParams elb instance_ = AwsElbAttachmentParams
  { _elba_elb = elb
  , _elba_instance = instance_
  }

instance ToResourceFieldMap AwsElbAttachmentParams where
  toResourceFieldMap params
    =  rfmField "elb" (_elba_elb params)
    <> rfmField "instance" (_elba_instance params)
    

instance ToResourceField AwsElbAttachmentParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsAutoscalingAttachment to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/autoscaling_attachment.html aws_autoscaling_attachment> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'asa_')

awsAutoscalingAttachment :: NameElement -> T.Text ->(AwsAutoscalingAttachmentParams -> AwsAutoscalingAttachmentParams) -> TF AwsAutoscalingAttachment
awsAutoscalingAttachment name0 autoscalingGroupName modf = newAwsAutoscalingAttachment name0 (modf (makeAwsAutoscalingAttachmentParams autoscalingGroupName))

awsAutoscalingAttachment' :: NameElement -> T.Text -> TF AwsAutoscalingAttachment
awsAutoscalingAttachment' name0 autoscalingGroupName = newAwsAutoscalingAttachment name0 (makeAwsAutoscalingAttachmentParams autoscalingGroupName)

newAwsAutoscalingAttachment :: NameElement -> AwsAutoscalingAttachmentParams -> TF AwsAutoscalingAttachment
newAwsAutoscalingAttachment name0 params = do
  rid <- mkResource "aws_autoscaling_attachment" name0 (toResourceFieldMap params)
  return AwsAutoscalingAttachment
    { asa_resource = rid
    }

data AwsAutoscalingAttachment = AwsAutoscalingAttachment
  { asa_resource :: ResourceId
  }

instance IsResource AwsAutoscalingAttachment where
  resourceId = asa_resource

data AwsAutoscalingAttachmentParams = AwsAutoscalingAttachmentParams
  { _asa_autoscaling_group_name :: T.Text
  , _asa_elb :: Maybe (T.Text)
  , _asa_alb_target_group_arn :: Maybe (AwsArn AwsLbTargetGroup)
  }

-- asa_autoscaling_group_name :: Lens' AwsAutoscalingAttachmentParams T.Text
asa_autoscaling_group_name :: Functor f => (T.Text -> f (T.Text)) -> AwsAutoscalingAttachmentParams -> f AwsAutoscalingAttachmentParams
asa_autoscaling_group_name k atom = fmap (\newasa_autoscaling_group_name -> atom { _asa_autoscaling_group_name = newasa_autoscaling_group_name }) (k (_asa_autoscaling_group_name atom))
-- asa_elb :: Lens' AwsAutoscalingAttachmentParams Maybe (T.Text)
asa_elb :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsAutoscalingAttachmentParams -> f AwsAutoscalingAttachmentParams
asa_elb k atom = fmap (\newasa_elb -> atom { _asa_elb = newasa_elb }) (k (_asa_elb atom))
-- asa_alb_target_group_arn :: Lens' AwsAutoscalingAttachmentParams Maybe (AwsArn AwsLbTargetGroup)
asa_alb_target_group_arn :: Functor f => (Maybe (AwsArn AwsLbTargetGroup) -> f (Maybe (AwsArn AwsLbTargetGroup))) -> AwsAutoscalingAttachmentParams -> f AwsAutoscalingAttachmentParams
asa_alb_target_group_arn k atom = fmap (\newasa_alb_target_group_arn -> atom { _asa_alb_target_group_arn = newasa_alb_target_group_arn }) (k (_asa_alb_target_group_arn atom))

makeAwsAutoscalingAttachmentParams :: T.Text -> AwsAutoscalingAttachmentParams
makeAwsAutoscalingAttachmentParams autoscalingGroupName = AwsAutoscalingAttachmentParams
  { _asa_autoscaling_group_name = autoscalingGroupName
  , _asa_elb = Nothing
  , _asa_alb_target_group_arn = Nothing
  }

instance ToResourceFieldMap AwsAutoscalingAttachmentParams where
  toResourceFieldMap params
    =  rfmField "autoscaling_group_name" (_asa_autoscaling_group_name params)
    <> rfmOptionalField "elb" (_asa_elb params)
    <> rfmOptionalField "alb_target_group_arn" (_asa_alb_target_group_arn params)
    

instance ToResourceField AwsAutoscalingAttachmentParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data CorsRuleParams = CorsRuleParams
  { _cors_allowed_methods :: [T.Text]
  , _cors_allowed_origins :: [T.Text]
  , _cors_allowed_headers :: [T.Text]
  , _cors_expose_headers :: [T.Text]
  , _cors_max_age_seconds :: Maybe (Int)
  }
  deriving (Eq)

-- cors_allowed_headers :: Lens' CorsRuleParams [T.Text]
cors_allowed_headers :: Functor f => ([T.Text] -> f ([T.Text])) -> CorsRuleParams -> f CorsRuleParams
cors_allowed_headers k atom = fmap (\newcors_allowed_headers -> atom { _cors_allowed_headers = newcors_allowed_headers }) (k (_cors_allowed_headers atom))
-- cors_allowed_methods :: Lens' CorsRuleParams [T.Text]
cors_allowed_methods :: Functor f => ([T.Text] -> f ([T.Text])) -> CorsRuleParams -> f CorsRuleParams
cors_allowed_methods k atom = fmap (\newcors_allowed_methods -> atom { _cors_allowed_methods = newcors_allowed_methods }) (k (_cors_allowed_methods atom))
-- cors_allowed_origins :: Lens' CorsRuleParams [T.Text]
cors_allowed_origins :: Functor f => ([T.Text] -> f ([T.Text])) -> CorsRuleParams -> f CorsRuleParams
cors_allowed_origins k atom = fmap (\newcors_allowed_origins -> atom { _cors_allowed_origins = newcors_allowed_origins }) (k (_cors_allowed_origins atom))
-- cors_expose_headers :: Lens' CorsRuleParams [T.Text]
cors_expose_headers :: Functor f => ([T.Text] -> f ([T.Text])) -> CorsRuleParams -> f CorsRuleParams
cors_expose_headers k atom = fmap (\newcors_expose_headers -> atom { _cors_expose_headers = newcors_expose_headers }) (k (_cors_expose_headers atom))
-- cors_max_age_seconds :: Lens' CorsRuleParams Maybe (Int)
cors_max_age_seconds :: Functor f => (Maybe (Int) -> f (Maybe (Int))) -> CorsRuleParams -> f CorsRuleParams
cors_max_age_seconds k atom = fmap (\newcors_max_age_seconds -> atom { _cors_max_age_seconds = newcors_max_age_seconds }) (k (_cors_max_age_seconds atom))

makeCorsRuleParams :: [T.Text] -> [T.Text] -> CorsRuleParams
makeCorsRuleParams allowedMethods allowedOrigins = CorsRuleParams
  { _cors_allowed_methods = allowedMethods
  , _cors_allowed_origins = allowedOrigins
  , _cors_allowed_headers = []
  , _cors_expose_headers = []
  , _cors_max_age_seconds = Nothing
  }

instance ToResourceFieldMap CorsRuleParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "allowed_headers" [] (_cors_allowed_headers params)
    <> rfmField "allowed_methods" (_cors_allowed_methods params)
    <> rfmField "allowed_origins" (_cors_allowed_origins params)
    <> rfmOptionalDefField "expose_headers" [] (_cors_expose_headers params)
    <> rfmOptionalField "max_age_seconds" (_cors_max_age_seconds params)
    

instance ToResourceField CorsRuleParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsS3Bucket to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/s3_bucket.html aws_s3_bucket> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 's3_')

awsS3Bucket :: NameElement -> T.Text ->(AwsS3BucketParams -> AwsS3BucketParams) -> TF AwsS3Bucket
awsS3Bucket name0 bucket modf = newAwsS3Bucket name0 (modf (makeAwsS3BucketParams bucket))

awsS3Bucket' :: NameElement -> T.Text -> TF AwsS3Bucket
awsS3Bucket' name0 bucket = newAwsS3Bucket name0 (makeAwsS3BucketParams bucket)

newAwsS3Bucket :: NameElement -> AwsS3BucketParams -> TF AwsS3Bucket
newAwsS3Bucket name0 params = do
  rid <- mkResource "aws_s3_bucket" name0 (toResourceFieldMap params)
  return AwsS3Bucket
    { s3_id = resourceAttr rid "id"
    , s3_resource = rid
    }

data AwsS3Bucket = AwsS3Bucket
  { s3_id :: TFRef S3BucketName
  , s3_resource :: ResourceId
  }

instance IsResource AwsS3Bucket where
  resourceId = s3_resource

data AwsS3BucketParams = AwsS3BucketParams
  { _s3_bucket :: T.Text
  , _s3_acl :: CannedAcl
  , _s3_tags :: M.Map T.Text T.Text
  , _s3_versioning :: Maybe (BucketVersioningParams)
  , _s3_lifecycle_rule :: Maybe (LifecycleRuleParams)
  , _s3_cors_rule :: Maybe (CorsRuleParams)
  }

-- s3_bucket :: Lens' AwsS3BucketParams T.Text
s3_bucket :: Functor f => (T.Text -> f (T.Text)) -> AwsS3BucketParams -> f AwsS3BucketParams
s3_bucket k atom = fmap (\news3_bucket -> atom { _s3_bucket = news3_bucket }) (k (_s3_bucket atom))
-- s3_acl :: Lens' AwsS3BucketParams CannedAcl
s3_acl :: Functor f => (CannedAcl -> f (CannedAcl)) -> AwsS3BucketParams -> f AwsS3BucketParams
s3_acl k atom = fmap (\news3_acl -> atom { _s3_acl = news3_acl }) (k (_s3_acl atom))
-- s3_tags :: Lens' AwsS3BucketParams M.Map T.Text T.Text
s3_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsS3BucketParams -> f AwsS3BucketParams
s3_tags k atom = fmap (\news3_tags -> atom { _s3_tags = news3_tags }) (k (_s3_tags atom))
-- s3_versioning :: Lens' AwsS3BucketParams Maybe (BucketVersioningParams)
s3_versioning :: Functor f => (Maybe (BucketVersioningParams) -> f (Maybe (BucketVersioningParams))) -> AwsS3BucketParams -> f AwsS3BucketParams
s3_versioning k atom = fmap (\news3_versioning -> atom { _s3_versioning = news3_versioning }) (k (_s3_versioning atom))
-- s3_lifecycle_rule :: Lens' AwsS3BucketParams Maybe (LifecycleRuleParams)
s3_lifecycle_rule :: Functor f => (Maybe (LifecycleRuleParams) -> f (Maybe (LifecycleRuleParams))) -> AwsS3BucketParams -> f AwsS3BucketParams
s3_lifecycle_rule k atom = fmap (\news3_lifecycle_rule -> atom { _s3_lifecycle_rule = news3_lifecycle_rule }) (k (_s3_lifecycle_rule atom))
-- s3_cors_rule :: Lens' AwsS3BucketParams Maybe (CorsRuleParams)
s3_cors_rule :: Functor f => (Maybe (CorsRuleParams) -> f (Maybe (CorsRuleParams))) -> AwsS3BucketParams -> f AwsS3BucketParams
s3_cors_rule k atom = fmap (\news3_cors_rule -> atom { _s3_cors_rule = news3_cors_rule }) (k (_s3_cors_rule atom))

makeAwsS3BucketParams :: T.Text -> AwsS3BucketParams
makeAwsS3BucketParams bucket = AwsS3BucketParams
  { _s3_bucket = bucket
  , _s3_acl = "private"
  , _s3_tags = M.empty
  , _s3_versioning = Nothing
  , _s3_lifecycle_rule = Nothing
  , _s3_cors_rule = Nothing
  }

instance ToResourceFieldMap AwsS3BucketParams where
  toResourceFieldMap params
    =  rfmField "bucket" (_s3_bucket params)
    <> rfmOptionalDefField "acl" "private" (_s3_acl params)
    <> rfmOptionalDefField "tags" M.empty (_s3_tags params)
    <> rfmOptionalField "versioning" (_s3_versioning params)
    <> rfmOptionalField "lifecycle_rule" (_s3_lifecycle_rule params)
    <> rfmOptionalField "cors_rule" (_s3_cors_rule params)
    

instance ToResourceField AwsS3BucketParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsS3BucketObject to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/d/s3_bucket_object.html aws_s3_bucket_object> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 's3o_')

awsS3BucketObject :: NameElement -> TFRef S3BucketName -> S3Key ->(AwsS3BucketObjectParams -> AwsS3BucketObjectParams) -> TF AwsS3BucketObject
awsS3BucketObject name0 bucket key modf = newAwsS3BucketObject name0 (modf (makeAwsS3BucketObjectParams bucket key))

awsS3BucketObject' :: NameElement -> TFRef S3BucketName -> S3Key -> TF AwsS3BucketObject
awsS3BucketObject' name0 bucket key = newAwsS3BucketObject name0 (makeAwsS3BucketObjectParams bucket key)

newAwsS3BucketObject :: NameElement -> AwsS3BucketObjectParams -> TF AwsS3BucketObject
newAwsS3BucketObject name0 params = do
  rid <- mkResource "aws_s3_bucket_object" name0 (toResourceFieldMap params)
  return AwsS3BucketObject
    { s3o_id = resourceAttr rid "id"
    , s3o_etag = resourceAttr rid "etag"
    , s3o_version_id = resourceAttr rid "version_id"
    , s3o_resource = rid
    }

data AwsS3BucketObject = AwsS3BucketObject
  { s3o_id :: TFRef T.Text
  , s3o_etag :: TFRef T.Text
  , s3o_version_id :: TFRef T.Text
  , s3o_resource :: ResourceId
  }

instance IsResource AwsS3BucketObject where
  resourceId = s3o_resource

data AwsS3BucketObjectParams = AwsS3BucketObjectParams
  { _s3o_bucket :: TFRef S3BucketName
  , _s3o_key :: S3Key
  , _s3o_source :: Maybe ( FilePath)
  , _s3o_content :: Maybe ( T.Text)
  }

-- s3o_bucket :: Lens' AwsS3BucketObjectParams TFRef S3BucketName
s3o_bucket :: Functor f => (TFRef S3BucketName -> f (TFRef S3BucketName)) -> AwsS3BucketObjectParams -> f AwsS3BucketObjectParams
s3o_bucket k atom = fmap (\news3o_bucket -> atom { _s3o_bucket = news3o_bucket }) (k (_s3o_bucket atom))
-- s3o_key :: Lens' AwsS3BucketObjectParams S3Key
s3o_key :: Functor f => (S3Key -> f (S3Key)) -> AwsS3BucketObjectParams -> f AwsS3BucketObjectParams
s3o_key k atom = fmap (\news3o_key -> atom { _s3o_key = news3o_key }) (k (_s3o_key atom))
-- s3o_source :: Lens' AwsS3BucketObjectParams Maybe ( FilePath)
s3o_source :: Functor f => (Maybe ( FilePath) -> f (Maybe ( FilePath))) -> AwsS3BucketObjectParams -> f AwsS3BucketObjectParams
s3o_source k atom = fmap (\news3o_source -> atom { _s3o_source = news3o_source }) (k (_s3o_source atom))
-- s3o_content :: Lens' AwsS3BucketObjectParams Maybe ( T.Text)
s3o_content :: Functor f => (Maybe ( T.Text) -> f (Maybe ( T.Text))) -> AwsS3BucketObjectParams -> f AwsS3BucketObjectParams
s3o_content k atom = fmap (\news3o_content -> atom { _s3o_content = news3o_content }) (k (_s3o_content atom))

makeAwsS3BucketObjectParams :: TFRef S3BucketName -> S3Key -> AwsS3BucketObjectParams
makeAwsS3BucketObjectParams bucket key = AwsS3BucketObjectParams
  { _s3o_bucket = bucket
  , _s3o_key = key
  , _s3o_source = Nothing
  , _s3o_content = Nothing
  }

instance ToResourceFieldMap AwsS3BucketObjectParams where
  toResourceFieldMap params
    =  rfmField "bucket" (_s3o_bucket params)
    <> rfmField "key" (_s3o_key params)
    <> rfmOptionalField "source" (_s3o_source params)
    <> rfmOptionalField "content" (_s3o_content params)
    

instance ToResourceField AwsS3BucketObjectParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsIamUser to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/iam_user.html aws_iam_user> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'iamu_')

awsIamUser :: NameElement -> T.Text ->(AwsIamUserParams -> AwsIamUserParams) -> TF AwsIamUser
awsIamUser name0 name' modf = newAwsIamUser name0 (modf (makeAwsIamUserParams name'))

awsIamUser' :: NameElement -> T.Text -> TF AwsIamUser
awsIamUser' name0 name' = newAwsIamUser name0 (makeAwsIamUserParams name')

newAwsIamUser :: NameElement -> AwsIamUserParams -> TF AwsIamUser
newAwsIamUser name0 params = do
  rid <- mkResource "aws_iam_user" name0 (toResourceFieldMap params)
  return AwsIamUser
    { iamu_arn = resourceAttr rid "arn"
    , iamu_name = resourceAttr rid "name"
    , iamu_unique_id = resourceAttr rid "unique_id"
    , iamu_resource = rid
    }

data AwsIamUser = AwsIamUser
  { iamu_arn :: TFRef Arn
  , iamu_name :: TFRef T.Text
  , iamu_unique_id :: TFRef T.Text
  , iamu_resource :: ResourceId
  }

instance IsResource AwsIamUser where
  resourceId = iamu_resource

data AwsIamUserParams = AwsIamUserParams
  { _iamu_name' :: T.Text
  , _iamu_path :: T.Text
  , _iamu_force_destroy :: Bool
  }

-- iamu_name' :: Lens' AwsIamUserParams T.Text
iamu_name' :: Functor f => (T.Text -> f (T.Text)) -> AwsIamUserParams -> f AwsIamUserParams
iamu_name' k atom = fmap (\newiamu_name' -> atom { _iamu_name' = newiamu_name' }) (k (_iamu_name' atom))
-- iamu_path :: Lens' AwsIamUserParams T.Text
iamu_path :: Functor f => (T.Text -> f (T.Text)) -> AwsIamUserParams -> f AwsIamUserParams
iamu_path k atom = fmap (\newiamu_path -> atom { _iamu_path = newiamu_path }) (k (_iamu_path atom))
-- iamu_force_destroy :: Lens' AwsIamUserParams Bool
iamu_force_destroy :: Functor f => (Bool -> f (Bool)) -> AwsIamUserParams -> f AwsIamUserParams
iamu_force_destroy k atom = fmap (\newiamu_force_destroy -> atom { _iamu_force_destroy = newiamu_force_destroy }) (k (_iamu_force_destroy atom))

makeAwsIamUserParams :: T.Text -> AwsIamUserParams
makeAwsIamUserParams name' = AwsIamUserParams
  { _iamu_name' = name'
  , _iamu_path = "/"
  , _iamu_force_destroy = False
  }

instance ToResourceFieldMap AwsIamUserParams where
  toResourceFieldMap params
    =  rfmField "name" (_iamu_name' params)
    <> rfmOptionalDefField "path" "/" (_iamu_path params)
    <> rfmOptionalDefField "force_destroy" False (_iamu_force_destroy params)
    

instance ToResourceField AwsIamUserParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsIamUserPolicy to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/iam_user_policy.html aws_iam_user_policy> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'iamup_')

awsIamUserPolicy :: NameElement -> T.Text -> T.Text -> TFRef T.Text ->(AwsIamUserPolicyParams -> AwsIamUserPolicyParams) -> TF AwsIamUserPolicy
awsIamUserPolicy name0 name policy user modf = newAwsIamUserPolicy name0 (modf (makeAwsIamUserPolicyParams name policy user))

awsIamUserPolicy' :: NameElement -> T.Text -> T.Text -> TFRef T.Text -> TF AwsIamUserPolicy
awsIamUserPolicy' name0 name policy user = newAwsIamUserPolicy name0 (makeAwsIamUserPolicyParams name policy user)

newAwsIamUserPolicy :: NameElement -> AwsIamUserPolicyParams -> TF AwsIamUserPolicy
newAwsIamUserPolicy name0 params = do
  rid <- mkResource "aws_iam_user_policy" name0 (toResourceFieldMap params)
  return AwsIamUserPolicy
    { iamup_resource = rid
    }

data AwsIamUserPolicy = AwsIamUserPolicy
  { iamup_resource :: ResourceId
  }

instance IsResource AwsIamUserPolicy where
  resourceId = iamup_resource

data AwsIamUserPolicyParams = AwsIamUserPolicyParams
  { _iamup_name :: T.Text
  , _iamup_policy :: T.Text
  , _iamup_user :: TFRef T.Text
  }

-- iamup_name :: Lens' AwsIamUserPolicyParams T.Text
iamup_name :: Functor f => (T.Text -> f (T.Text)) -> AwsIamUserPolicyParams -> f AwsIamUserPolicyParams
iamup_name k atom = fmap (\newiamup_name -> atom { _iamup_name = newiamup_name }) (k (_iamup_name atom))
-- iamup_policy :: Lens' AwsIamUserPolicyParams T.Text
iamup_policy :: Functor f => (T.Text -> f (T.Text)) -> AwsIamUserPolicyParams -> f AwsIamUserPolicyParams
iamup_policy k atom = fmap (\newiamup_policy -> atom { _iamup_policy = newiamup_policy }) (k (_iamup_policy atom))
-- iamup_user :: Lens' AwsIamUserPolicyParams TFRef T.Text
iamup_user :: Functor f => (TFRef T.Text -> f (TFRef T.Text)) -> AwsIamUserPolicyParams -> f AwsIamUserPolicyParams
iamup_user k atom = fmap (\newiamup_user -> atom { _iamup_user = newiamup_user }) (k (_iamup_user atom))

makeAwsIamUserPolicyParams :: T.Text -> T.Text -> TFRef T.Text -> AwsIamUserPolicyParams
makeAwsIamUserPolicyParams name policy user = AwsIamUserPolicyParams
  { _iamup_name = name
  , _iamup_policy = policy
  , _iamup_user = user
  }

instance ToResourceFieldMap AwsIamUserPolicyParams where
  toResourceFieldMap params
    =  rfmField "name" (_iamup_name params)
    <> rfmField "policy" (_iamup_policy params)
    <> rfmField "user" (_iamup_user params)
    

instance ToResourceField AwsIamUserPolicyParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsIamUserPolicyAttachment to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/iam_user_policy_attachment.html aws_iam_user_policy_attachment> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'iamupa_')

awsIamUserPolicyAttachment :: NameElement -> TFRef T.Text -> T.Text ->(AwsIamUserPolicyAttachmentParams -> AwsIamUserPolicyAttachmentParams) -> TF AwsIamUserPolicyAttachment
awsIamUserPolicyAttachment name0 user policyArn modf = newAwsIamUserPolicyAttachment name0 (modf (makeAwsIamUserPolicyAttachmentParams user policyArn))

awsIamUserPolicyAttachment' :: NameElement -> TFRef T.Text -> T.Text -> TF AwsIamUserPolicyAttachment
awsIamUserPolicyAttachment' name0 user policyArn = newAwsIamUserPolicyAttachment name0 (makeAwsIamUserPolicyAttachmentParams user policyArn)

newAwsIamUserPolicyAttachment :: NameElement -> AwsIamUserPolicyAttachmentParams -> TF AwsIamUserPolicyAttachment
newAwsIamUserPolicyAttachment name0 params = do
  rid <- mkResource "aws_iam_user_policy_attachment" name0 (toResourceFieldMap params)
  return AwsIamUserPolicyAttachment
    { iamupa_resource = rid
    }

data AwsIamUserPolicyAttachment = AwsIamUserPolicyAttachment
  { iamupa_resource :: ResourceId
  }

instance IsResource AwsIamUserPolicyAttachment where
  resourceId = iamupa_resource

data AwsIamUserPolicyAttachmentParams = AwsIamUserPolicyAttachmentParams
  { _iamupa_user :: TFRef T.Text
  , _iamupa_policy_arn :: T.Text
  }

-- iamupa_user :: Lens' AwsIamUserPolicyAttachmentParams TFRef T.Text
iamupa_user :: Functor f => (TFRef T.Text -> f (TFRef T.Text)) -> AwsIamUserPolicyAttachmentParams -> f AwsIamUserPolicyAttachmentParams
iamupa_user k atom = fmap (\newiamupa_user -> atom { _iamupa_user = newiamupa_user }) (k (_iamupa_user atom))
-- iamupa_policy_arn :: Lens' AwsIamUserPolicyAttachmentParams T.Text
iamupa_policy_arn :: Functor f => (T.Text -> f (T.Text)) -> AwsIamUserPolicyAttachmentParams -> f AwsIamUserPolicyAttachmentParams
iamupa_policy_arn k atom = fmap (\newiamupa_policy_arn -> atom { _iamupa_policy_arn = newiamupa_policy_arn }) (k (_iamupa_policy_arn atom))

makeAwsIamUserPolicyAttachmentParams :: TFRef T.Text -> T.Text -> AwsIamUserPolicyAttachmentParams
makeAwsIamUserPolicyAttachmentParams user policyArn = AwsIamUserPolicyAttachmentParams
  { _iamupa_user = user
  , _iamupa_policy_arn = policyArn
  }

instance ToResourceFieldMap AwsIamUserPolicyAttachmentParams where
  toResourceFieldMap params
    =  rfmField "user" (_iamupa_user params)
    <> rfmField "policy_arn" (_iamupa_policy_arn params)
    

instance ToResourceField AwsIamUserPolicyAttachmentParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsIamRole to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/iam_role.html aws_iam_role> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'iamr_')

awsIamRole :: NameElement -> T.Text ->(AwsIamRoleParams -> AwsIamRoleParams) -> TF AwsIamRole
awsIamRole name0 assumeRolePolicy modf = newAwsIamRole name0 (modf (makeAwsIamRoleParams assumeRolePolicy))

awsIamRole' :: NameElement -> T.Text -> TF AwsIamRole
awsIamRole' name0 assumeRolePolicy = newAwsIamRole name0 (makeAwsIamRoleParams assumeRolePolicy)

newAwsIamRole :: NameElement -> AwsIamRoleParams -> TF AwsIamRole
newAwsIamRole name0 params = do
  rid <- mkResource "aws_iam_role" name0 (toResourceFieldMap params)
  return AwsIamRole
    { iamr_id = resourceAttr rid "id"
    , iamr_arn = resourceAttr rid "arn"
    , iamr_name = resourceAttr rid "name"
    , iamr_create_date = resourceAttr rid "create_date"
    , iamr_unique_id = resourceAttr rid "unique_id"
    , iamr_resource = rid
    }

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

data AwsIamRoleParams = AwsIamRoleParams
  { _iamr_assume_role_policy :: T.Text
  , _iamr_name' :: T.Text
  , _iamr_name_prefix :: T.Text
  , _iamr_path :: T.Text
  }

-- iamr_name' :: Lens' AwsIamRoleParams T.Text
iamr_name' :: Functor f => (T.Text -> f (T.Text)) -> AwsIamRoleParams -> f AwsIamRoleParams
iamr_name' k atom = fmap (\newiamr_name' -> atom { _iamr_name' = newiamr_name' }) (k (_iamr_name' atom))
-- iamr_name_prefix :: Lens' AwsIamRoleParams T.Text
iamr_name_prefix :: Functor f => (T.Text -> f (T.Text)) -> AwsIamRoleParams -> f AwsIamRoleParams
iamr_name_prefix k atom = fmap (\newiamr_name_prefix -> atom { _iamr_name_prefix = newiamr_name_prefix }) (k (_iamr_name_prefix atom))
-- iamr_assume_role_policy :: Lens' AwsIamRoleParams T.Text
iamr_assume_role_policy :: Functor f => (T.Text -> f (T.Text)) -> AwsIamRoleParams -> f AwsIamRoleParams
iamr_assume_role_policy k atom = fmap (\newiamr_assume_role_policy -> atom { _iamr_assume_role_policy = newiamr_assume_role_policy }) (k (_iamr_assume_role_policy atom))
-- iamr_path :: Lens' AwsIamRoleParams T.Text
iamr_path :: Functor f => (T.Text -> f (T.Text)) -> AwsIamRoleParams -> f AwsIamRoleParams
iamr_path k atom = fmap (\newiamr_path -> atom { _iamr_path = newiamr_path }) (k (_iamr_path atom))

makeAwsIamRoleParams :: T.Text -> AwsIamRoleParams
makeAwsIamRoleParams assumeRolePolicy = AwsIamRoleParams
  { _iamr_assume_role_policy = assumeRolePolicy
  , _iamr_name' = ""
  , _iamr_name_prefix = ""
  , _iamr_path = ""
  }

instance ToResourceFieldMap AwsIamRoleParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "name" "" (_iamr_name' params)
    <> rfmOptionalDefField "name_prefix" "" (_iamr_name_prefix params)
    <> rfmField "assume_role_policy" (_iamr_assume_role_policy params)
    <> rfmOptionalDefField "path" "" (_iamr_path params)
    

instance ToResourceField AwsIamRoleParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsIamInstanceProfile to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/iam_instance_profile.html aws_iam_instance_profile> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'iamip_')

awsIamInstanceProfile :: NameElement -> (AwsIamInstanceProfileParams -> AwsIamInstanceProfileParams) -> TF AwsIamInstanceProfile
awsIamInstanceProfile name0  modf = newAwsIamInstanceProfile name0 (modf (makeAwsIamInstanceProfileParams ))

awsIamInstanceProfile' :: NameElement ->  TF AwsIamInstanceProfile
awsIamInstanceProfile' name0  = newAwsIamInstanceProfile name0 (makeAwsIamInstanceProfileParams )

newAwsIamInstanceProfile :: NameElement -> AwsIamInstanceProfileParams -> TF AwsIamInstanceProfile
newAwsIamInstanceProfile name0 params = do
  rid <- mkResource "aws_iam_instance_profile" name0 (toResourceFieldMap params)
  return AwsIamInstanceProfile
    { iamip_id = resourceAttr rid "id"
    , iamip_arn = resourceAttr rid "arn"
    , iamip_create_date = resourceAttr rid "create_date"
    , iamip_unique_id = resourceAttr rid "unique_id"
    , iamip_resource = rid
    }

data AwsIamInstanceProfile = AwsIamInstanceProfile
  { iamip_id :: TFRef (AwsId AwsIamInstanceProfile)
  , iamip_arn :: TFRef Arn
  , iamip_create_date :: TFRef T.Text
  , iamip_unique_id :: TFRef T.Text
  , iamip_resource :: ResourceId
  }

instance IsResource AwsIamInstanceProfile where
  resourceId = iamip_resource

data AwsIamInstanceProfileParams = AwsIamInstanceProfileParams
  { _iamip_name :: T.Text
  , _iamip_name_prefix :: T.Text
  , _iamip_path :: T.Text
  , _iamip_roles :: [TFRef T.Text]
  , _iamip_role :: Maybe (TFRef T.Text)
  }

-- iamip_name :: Lens' AwsIamInstanceProfileParams T.Text
iamip_name :: Functor f => (T.Text -> f (T.Text)) -> AwsIamInstanceProfileParams -> f AwsIamInstanceProfileParams
iamip_name k atom = fmap (\newiamip_name -> atom { _iamip_name = newiamip_name }) (k (_iamip_name atom))
-- iamip_name_prefix :: Lens' AwsIamInstanceProfileParams T.Text
iamip_name_prefix :: Functor f => (T.Text -> f (T.Text)) -> AwsIamInstanceProfileParams -> f AwsIamInstanceProfileParams
iamip_name_prefix k atom = fmap (\newiamip_name_prefix -> atom { _iamip_name_prefix = newiamip_name_prefix }) (k (_iamip_name_prefix atom))
-- iamip_path :: Lens' AwsIamInstanceProfileParams T.Text
iamip_path :: Functor f => (T.Text -> f (T.Text)) -> AwsIamInstanceProfileParams -> f AwsIamInstanceProfileParams
iamip_path k atom = fmap (\newiamip_path -> atom { _iamip_path = newiamip_path }) (k (_iamip_path atom))
-- iamip_roles :: Lens' AwsIamInstanceProfileParams [TFRef T.Text]
iamip_roles :: Functor f => ([TFRef T.Text] -> f ([TFRef T.Text])) -> AwsIamInstanceProfileParams -> f AwsIamInstanceProfileParams
iamip_roles k atom = fmap (\newiamip_roles -> atom { _iamip_roles = newiamip_roles }) (k (_iamip_roles atom))
-- iamip_role :: Lens' AwsIamInstanceProfileParams Maybe (TFRef T.Text)
iamip_role :: Functor f => (Maybe (TFRef T.Text) -> f (Maybe (TFRef T.Text))) -> AwsIamInstanceProfileParams -> f AwsIamInstanceProfileParams
iamip_role k atom = fmap (\newiamip_role -> atom { _iamip_role = newiamip_role }) (k (_iamip_role atom))

makeAwsIamInstanceProfileParams ::  AwsIamInstanceProfileParams
makeAwsIamInstanceProfileParams  = AwsIamInstanceProfileParams
  { _iamip_name = ""
  , _iamip_name_prefix = ""
  , _iamip_path = "/"
  , _iamip_roles = []
  , _iamip_role = Nothing
  }

instance ToResourceFieldMap AwsIamInstanceProfileParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "name" "" (_iamip_name params)
    <> rfmOptionalDefField "name_prefix" "" (_iamip_name_prefix params)
    <> rfmOptionalDefField "path" "/" (_iamip_path params)
    <> rfmOptionalDefField "roles" [] (_iamip_roles params)
    <> rfmOptionalField "role" (_iamip_role params)
    

instance ToResourceField AwsIamInstanceProfileParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsIamRolePolicy to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/iam_role_policy.html aws_iam_role_policy> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'iamrp_')

awsIamRolePolicy :: NameElement -> T.Text -> T.Text -> TFRef (AwsId AwsIamRole) ->(AwsIamRolePolicyParams -> AwsIamRolePolicyParams) -> TF AwsIamRolePolicy
awsIamRolePolicy name0 name policy role modf = newAwsIamRolePolicy name0 (modf (makeAwsIamRolePolicyParams name policy role))

awsIamRolePolicy' :: NameElement -> T.Text -> T.Text -> TFRef (AwsId AwsIamRole) -> TF AwsIamRolePolicy
awsIamRolePolicy' name0 name policy role = newAwsIamRolePolicy name0 (makeAwsIamRolePolicyParams name policy role)

newAwsIamRolePolicy :: NameElement -> AwsIamRolePolicyParams -> TF AwsIamRolePolicy
newAwsIamRolePolicy name0 params = do
  rid <- mkResource "aws_iam_role_policy" name0 (toResourceFieldMap params)
  return AwsIamRolePolicy
    { iamrp_id = resourceAttr rid "id"
    , iamrp_resource = rid
    }

data AwsIamRolePolicy = AwsIamRolePolicy
  { iamrp_id :: TFRef (AwsId AwsIamInstanceProfile)
  , iamrp_resource :: ResourceId
  }

instance IsResource AwsIamRolePolicy where
  resourceId = iamrp_resource

data AwsIamRolePolicyParams = AwsIamRolePolicyParams
  { _iamrp_name :: T.Text
  , _iamrp_policy :: T.Text
  , _iamrp_role :: TFRef (AwsId AwsIamRole)
  }

-- iamrp_name :: Lens' AwsIamRolePolicyParams T.Text
iamrp_name :: Functor f => (T.Text -> f (T.Text)) -> AwsIamRolePolicyParams -> f AwsIamRolePolicyParams
iamrp_name k atom = fmap (\newiamrp_name -> atom { _iamrp_name = newiamrp_name }) (k (_iamrp_name atom))
-- iamrp_policy :: Lens' AwsIamRolePolicyParams T.Text
iamrp_policy :: Functor f => (T.Text -> f (T.Text)) -> AwsIamRolePolicyParams -> f AwsIamRolePolicyParams
iamrp_policy k atom = fmap (\newiamrp_policy -> atom { _iamrp_policy = newiamrp_policy }) (k (_iamrp_policy atom))
-- iamrp_role :: Lens' AwsIamRolePolicyParams TFRef (AwsId AwsIamRole)
iamrp_role :: Functor f => (TFRef (AwsId AwsIamRole) -> f (TFRef (AwsId AwsIamRole))) -> AwsIamRolePolicyParams -> f AwsIamRolePolicyParams
iamrp_role k atom = fmap (\newiamrp_role -> atom { _iamrp_role = newiamrp_role }) (k (_iamrp_role atom))

makeAwsIamRolePolicyParams :: T.Text -> T.Text -> TFRef (AwsId AwsIamRole) -> AwsIamRolePolicyParams
makeAwsIamRolePolicyParams name policy role = AwsIamRolePolicyParams
  { _iamrp_name = name
  , _iamrp_policy = policy
  , _iamrp_role = role
  }

instance ToResourceFieldMap AwsIamRolePolicyParams where
  toResourceFieldMap params
    =  rfmField "name" (_iamrp_name params)
    <> rfmField "policy" (_iamrp_policy params)
    <> rfmField "role" (_iamrp_role params)
    

instance ToResourceField AwsIamRolePolicyParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsSnsTopic to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/sns_topic.html aws_sns_topic> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'sns_')

awsSnsTopic :: NameElement -> T.Text ->(AwsSnsTopicParams -> AwsSnsTopicParams) -> TF AwsSnsTopic
awsSnsTopic name0 name modf = newAwsSnsTopic name0 (modf (makeAwsSnsTopicParams name))

awsSnsTopic' :: NameElement -> T.Text -> TF AwsSnsTopic
awsSnsTopic' name0 name = newAwsSnsTopic name0 (makeAwsSnsTopicParams name)

newAwsSnsTopic :: NameElement -> AwsSnsTopicParams -> TF AwsSnsTopic
newAwsSnsTopic name0 params = do
  rid <- mkResource "aws_sns_topic" name0 (toResourceFieldMap params)
  return AwsSnsTopic
    { sns_id = resourceAttr rid "id"
    , sns_arn = resourceAttr rid "arn"
    , sns_resource = rid
    }

data AwsSnsTopic = AwsSnsTopic
  { sns_id :: TFRef (AwsId AwsSnsTopic)
  , sns_arn :: TFRef Arn
  , sns_resource :: ResourceId
  }

instance IsResource AwsSnsTopic where
  resourceId = sns_resource

data AwsSnsTopicParams = AwsSnsTopicParams
  { _sns_name :: T.Text
  , _sns_display_name :: T.Text
  }

-- sns_name :: Lens' AwsSnsTopicParams T.Text
sns_name :: Functor f => (T.Text -> f (T.Text)) -> AwsSnsTopicParams -> f AwsSnsTopicParams
sns_name k atom = fmap (\newsns_name -> atom { _sns_name = newsns_name }) (k (_sns_name atom))
-- sns_display_name :: Lens' AwsSnsTopicParams T.Text
sns_display_name :: Functor f => (T.Text -> f (T.Text)) -> AwsSnsTopicParams -> f AwsSnsTopicParams
sns_display_name k atom = fmap (\newsns_display_name -> atom { _sns_display_name = newsns_display_name }) (k (_sns_display_name atom))

makeAwsSnsTopicParams :: T.Text -> AwsSnsTopicParams
makeAwsSnsTopicParams name = AwsSnsTopicParams
  { _sns_name = name
  , _sns_display_name = ""
  }

instance ToResourceFieldMap AwsSnsTopicParams where
  toResourceFieldMap params
    =  rfmField "name" (_sns_name params)
    <> rfmOptionalDefField "display_name" "" (_sns_display_name params)
    

instance ToResourceField AwsSnsTopicParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsCloudwatchMetricAlarm to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/cloudwatch_metric_alarm.html aws_cloudwatch_metric_alarm> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'cma_')

awsCloudwatchMetricAlarm :: NameElement -> T.Text -> MetricComparisonOperator -> Int -> MetricName -> MetricNamespace -> Int -> MetricStatistic -> Int ->(AwsCloudwatchMetricAlarmParams -> AwsCloudwatchMetricAlarmParams) -> TF AwsCloudwatchMetricAlarm
awsCloudwatchMetricAlarm name0 alarmName comparisonOperator evaluationPeriods metricName namespace period statistic threshold modf = newAwsCloudwatchMetricAlarm name0 (modf (makeAwsCloudwatchMetricAlarmParams alarmName comparisonOperator evaluationPeriods metricName namespace period statistic threshold))

awsCloudwatchMetricAlarm' :: NameElement -> T.Text -> MetricComparisonOperator -> Int -> MetricName -> MetricNamespace -> Int -> MetricStatistic -> Int -> TF AwsCloudwatchMetricAlarm
awsCloudwatchMetricAlarm' name0 alarmName comparisonOperator evaluationPeriods metricName namespace period statistic threshold = newAwsCloudwatchMetricAlarm name0 (makeAwsCloudwatchMetricAlarmParams alarmName comparisonOperator evaluationPeriods metricName namespace period statistic threshold)

newAwsCloudwatchMetricAlarm :: NameElement -> AwsCloudwatchMetricAlarmParams -> TF AwsCloudwatchMetricAlarm
newAwsCloudwatchMetricAlarm name0 params = do
  rid <- mkResource "aws_cloudwatch_metric_alarm" name0 (toResourceFieldMap params)
  return AwsCloudwatchMetricAlarm
    { cma_id = resourceAttr rid "id"
    , cma_resource = rid
    }

data AwsCloudwatchMetricAlarm = AwsCloudwatchMetricAlarm
  { cma_id :: TFRef (AwsId AwsCloudwatchMetricAlarm)
  , cma_resource :: ResourceId
  }

instance IsResource AwsCloudwatchMetricAlarm where
  resourceId = cma_resource

data AwsCloudwatchMetricAlarmParams = AwsCloudwatchMetricAlarmParams
  { _cma_alarm_name :: T.Text
  , _cma_comparison_operator :: MetricComparisonOperator
  , _cma_evaluation_periods :: Int
  , _cma_metric_name :: MetricName
  , _cma_namespace :: MetricNamespace
  , _cma_period :: Int
  , _cma_statistic :: MetricStatistic
  , _cma_threshold :: Int
  , _cma_actions_enabled :: Bool
  , _cma_alarm_actions :: [TFRef Arn]
  , _cma_alarm_description :: T.Text
  , _cma_dimensions :: M.Map T.Text T.Text
  , _cma_insufficient_data_actions :: [TFRef Arn]
  , _cma_ok_actions :: [TFRef Arn]
  , _cma_unit :: MetricUnit
  }

-- cma_alarm_name :: Lens' AwsCloudwatchMetricAlarmParams T.Text
cma_alarm_name :: Functor f => (T.Text -> f (T.Text)) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_alarm_name k atom = fmap (\newcma_alarm_name -> atom { _cma_alarm_name = newcma_alarm_name }) (k (_cma_alarm_name atom))
-- cma_comparison_operator :: Lens' AwsCloudwatchMetricAlarmParams MetricComparisonOperator
cma_comparison_operator :: Functor f => (MetricComparisonOperator -> f (MetricComparisonOperator)) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_comparison_operator k atom = fmap (\newcma_comparison_operator -> atom { _cma_comparison_operator = newcma_comparison_operator }) (k (_cma_comparison_operator atom))
-- cma_evaluation_periods :: Lens' AwsCloudwatchMetricAlarmParams Int
cma_evaluation_periods :: Functor f => (Int -> f (Int)) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_evaluation_periods k atom = fmap (\newcma_evaluation_periods -> atom { _cma_evaluation_periods = newcma_evaluation_periods }) (k (_cma_evaluation_periods atom))
-- cma_metric_name :: Lens' AwsCloudwatchMetricAlarmParams MetricName
cma_metric_name :: Functor f => (MetricName -> f (MetricName)) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_metric_name k atom = fmap (\newcma_metric_name -> atom { _cma_metric_name = newcma_metric_name }) (k (_cma_metric_name atom))
-- cma_namespace :: Lens' AwsCloudwatchMetricAlarmParams MetricNamespace
cma_namespace :: Functor f => (MetricNamespace -> f (MetricNamespace)) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_namespace k atom = fmap (\newcma_namespace -> atom { _cma_namespace = newcma_namespace }) (k (_cma_namespace atom))
-- cma_period :: Lens' AwsCloudwatchMetricAlarmParams Int
cma_period :: Functor f => (Int -> f (Int)) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_period k atom = fmap (\newcma_period -> atom { _cma_period = newcma_period }) (k (_cma_period atom))
-- cma_statistic :: Lens' AwsCloudwatchMetricAlarmParams MetricStatistic
cma_statistic :: Functor f => (MetricStatistic -> f (MetricStatistic)) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_statistic k atom = fmap (\newcma_statistic -> atom { _cma_statistic = newcma_statistic }) (k (_cma_statistic atom))
-- cma_threshold :: Lens' AwsCloudwatchMetricAlarmParams Int
cma_threshold :: Functor f => (Int -> f (Int)) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_threshold k atom = fmap (\newcma_threshold -> atom { _cma_threshold = newcma_threshold }) (k (_cma_threshold atom))
-- cma_actions_enabled :: Lens' AwsCloudwatchMetricAlarmParams Bool
cma_actions_enabled :: Functor f => (Bool -> f (Bool)) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_actions_enabled k atom = fmap (\newcma_actions_enabled -> atom { _cma_actions_enabled = newcma_actions_enabled }) (k (_cma_actions_enabled atom))
-- cma_alarm_actions :: Lens' AwsCloudwatchMetricAlarmParams [TFRef Arn]
cma_alarm_actions :: Functor f => ([TFRef Arn] -> f ([TFRef Arn])) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_alarm_actions k atom = fmap (\newcma_alarm_actions -> atom { _cma_alarm_actions = newcma_alarm_actions }) (k (_cma_alarm_actions atom))
-- cma_alarm_description :: Lens' AwsCloudwatchMetricAlarmParams T.Text
cma_alarm_description :: Functor f => (T.Text -> f (T.Text)) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_alarm_description k atom = fmap (\newcma_alarm_description -> atom { _cma_alarm_description = newcma_alarm_description }) (k (_cma_alarm_description atom))
-- cma_dimensions :: Lens' AwsCloudwatchMetricAlarmParams M.Map T.Text T.Text
cma_dimensions :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_dimensions k atom = fmap (\newcma_dimensions -> atom { _cma_dimensions = newcma_dimensions }) (k (_cma_dimensions atom))
-- cma_insufficient_data_actions :: Lens' AwsCloudwatchMetricAlarmParams [TFRef Arn]
cma_insufficient_data_actions :: Functor f => ([TFRef Arn] -> f ([TFRef Arn])) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_insufficient_data_actions k atom = fmap (\newcma_insufficient_data_actions -> atom { _cma_insufficient_data_actions = newcma_insufficient_data_actions }) (k (_cma_insufficient_data_actions atom))
-- cma_ok_actions :: Lens' AwsCloudwatchMetricAlarmParams [TFRef Arn]
cma_ok_actions :: Functor f => ([TFRef Arn] -> f ([TFRef Arn])) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_ok_actions k atom = fmap (\newcma_ok_actions -> atom { _cma_ok_actions = newcma_ok_actions }) (k (_cma_ok_actions atom))
-- cma_unit :: Lens' AwsCloudwatchMetricAlarmParams MetricUnit
cma_unit :: Functor f => (MetricUnit -> f (MetricUnit)) -> AwsCloudwatchMetricAlarmParams -> f AwsCloudwatchMetricAlarmParams
cma_unit k atom = fmap (\newcma_unit -> atom { _cma_unit = newcma_unit }) (k (_cma_unit atom))

makeAwsCloudwatchMetricAlarmParams :: T.Text -> MetricComparisonOperator -> Int -> MetricName -> MetricNamespace -> Int -> MetricStatistic -> Int -> AwsCloudwatchMetricAlarmParams
makeAwsCloudwatchMetricAlarmParams alarmName comparisonOperator evaluationPeriods metricName namespace period statistic threshold = AwsCloudwatchMetricAlarmParams
  { _cma_alarm_name = alarmName
  , _cma_comparison_operator = comparisonOperator
  , _cma_evaluation_periods = evaluationPeriods
  , _cma_metric_name = metricName
  , _cma_namespace = namespace
  , _cma_period = period
  , _cma_statistic = statistic
  , _cma_threshold = threshold
  , _cma_actions_enabled = True
  , _cma_alarm_actions = []
  , _cma_alarm_description = ""
  , _cma_dimensions = M.empty
  , _cma_insufficient_data_actions = []
  , _cma_ok_actions = []
  , _cma_unit = ""
  }

instance ToResourceFieldMap AwsCloudwatchMetricAlarmParams where
  toResourceFieldMap params
    =  rfmField "alarm_name" (_cma_alarm_name params)
    <> rfmField "comparison_operator" (_cma_comparison_operator params)
    <> rfmField "evaluation_periods" (_cma_evaluation_periods params)
    <> rfmField "metric_name" (_cma_metric_name params)
    <> rfmField "namespace" (_cma_namespace params)
    <> rfmField "period" (_cma_period params)
    <> rfmField "statistic" (_cma_statistic params)
    <> rfmField "threshold" (_cma_threshold params)
    <> rfmOptionalDefField "actions_enabled" True (_cma_actions_enabled params)
    <> rfmOptionalDefField "alarm_actions" [] (_cma_alarm_actions params)
    <> rfmOptionalDefField "alarm_description" "" (_cma_alarm_description params)
    <> rfmOptionalDefField "dimensions" M.empty (_cma_dimensions params)
    <> rfmOptionalDefField "insufficient_data_actions" [] (_cma_insufficient_data_actions params)
    <> rfmOptionalDefField "ok_actions" [] (_cma_ok_actions params)
    <> rfmOptionalDefField "unit" "" (_cma_unit params)
    

instance ToResourceField AwsCloudwatchMetricAlarmParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsRdsCluster to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/rds_cluster.html aws_rds_cluster> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'rc_')

awsRdsCluster :: NameElement -> DBEngine -> T.Text -> T.Text ->(AwsRdsClusterParams -> AwsRdsClusterParams) -> TF AwsRdsCluster
awsRdsCluster name0 engine masterUsername' masterPassword modf = newAwsRdsCluster name0 (modf (makeAwsRdsClusterParams engine masterUsername' masterPassword))

awsRdsCluster' :: NameElement -> DBEngine -> T.Text -> T.Text -> TF AwsRdsCluster
awsRdsCluster' name0 engine masterUsername' masterPassword = newAwsRdsCluster name0 (makeAwsRdsClusterParams engine masterUsername' masterPassword)

newAwsRdsCluster :: NameElement -> AwsRdsClusterParams -> TF AwsRdsCluster
newAwsRdsCluster name0 params = do
  rid <- mkResource "aws_rds_cluster" name0 (toResourceFieldMap params)
  return AwsRdsCluster
    { rc_id = resourceAttr rid "id"
    , rc_reader_endpoint = resourceAttr rid "reader_endpoint"
    , rc_endpoint = resourceAttr rid "endpoint"
    , rc_database_name = resourceAttr rid "database_name"
    , rc_port = resourceAttr rid "port"
    , rc_master_username = resourceAttr rid "master_username"
    , rc_resource = rid
    }

data AwsRdsCluster = AwsRdsCluster
  { rc_id :: TFRef (AwsId AwsRdsCluster)
  , rc_reader_endpoint :: TFRef T.Text
  , rc_endpoint :: TFRef T.Text
  , rc_database_name :: TFRef T.Text
  , rc_port :: TFRef T.Text
  , rc_master_username :: TFRef T.Text
  , rc_resource :: ResourceId
  }

instance IsResource AwsRdsCluster where
  resourceId = rc_resource

data AwsRdsClusterParams = AwsRdsClusterParams
  { _rc_engine :: DBEngine
  , _rc_master_username' :: T.Text
  , _rc_master_password :: T.Text
  , _rc_cluster_identifier :: T.Text
  , _rc_engine_version :: Maybe (T.Text)
  , _rc_database_name' :: T.Text
  , _rc_port' :: Maybe (Int)
  , _rc_apply_immediately :: Bool
  , _rc_skip_final_snapshot :: Bool
  , _rc_final_snapshot_identifier :: Maybe (T.Text)
  , _rc_vpc_security_group_ids :: [TFRef (AwsId AwsSecurityGroup)]
  , _rc_db_cluster_parameter_group_name :: Maybe (TFRef T.Text)
  , _rc_db_subnet_group_name :: Maybe (TFRef T.Text)
  , _rc_backup_retention_period :: Int
  , _rc_tags :: M.Map T.Text T.Text
  }

-- rc_cluster_identifier :: Lens' AwsRdsClusterParams T.Text
rc_cluster_identifier :: Functor f => (T.Text -> f (T.Text)) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_cluster_identifier k atom = fmap (\newrc_cluster_identifier -> atom { _rc_cluster_identifier = newrc_cluster_identifier }) (k (_rc_cluster_identifier atom))
-- rc_engine :: Lens' AwsRdsClusterParams DBEngine
rc_engine :: Functor f => (DBEngine -> f (DBEngine)) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_engine k atom = fmap (\newrc_engine -> atom { _rc_engine = newrc_engine }) (k (_rc_engine atom))
-- rc_engine_version :: Lens' AwsRdsClusterParams Maybe (T.Text)
rc_engine_version :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_engine_version k atom = fmap (\newrc_engine_version -> atom { _rc_engine_version = newrc_engine_version }) (k (_rc_engine_version atom))
-- rc_database_name' :: Lens' AwsRdsClusterParams T.Text
rc_database_name' :: Functor f => (T.Text -> f (T.Text)) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_database_name' k atom = fmap (\newrc_database_name' -> atom { _rc_database_name' = newrc_database_name' }) (k (_rc_database_name' atom))
-- rc_port' :: Lens' AwsRdsClusterParams Maybe (Int)
rc_port' :: Functor f => (Maybe (Int) -> f (Maybe (Int))) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_port' k atom = fmap (\newrc_port' -> atom { _rc_port' = newrc_port' }) (k (_rc_port' atom))
-- rc_master_username' :: Lens' AwsRdsClusterParams T.Text
rc_master_username' :: Functor f => (T.Text -> f (T.Text)) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_master_username' k atom = fmap (\newrc_master_username' -> atom { _rc_master_username' = newrc_master_username' }) (k (_rc_master_username' atom))
-- rc_master_password :: Lens' AwsRdsClusterParams T.Text
rc_master_password :: Functor f => (T.Text -> f (T.Text)) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_master_password k atom = fmap (\newrc_master_password -> atom { _rc_master_password = newrc_master_password }) (k (_rc_master_password atom))
-- rc_apply_immediately :: Lens' AwsRdsClusterParams Bool
rc_apply_immediately :: Functor f => (Bool -> f (Bool)) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_apply_immediately k atom = fmap (\newrc_apply_immediately -> atom { _rc_apply_immediately = newrc_apply_immediately }) (k (_rc_apply_immediately atom))
-- rc_skip_final_snapshot :: Lens' AwsRdsClusterParams Bool
rc_skip_final_snapshot :: Functor f => (Bool -> f (Bool)) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_skip_final_snapshot k atom = fmap (\newrc_skip_final_snapshot -> atom { _rc_skip_final_snapshot = newrc_skip_final_snapshot }) (k (_rc_skip_final_snapshot atom))
-- rc_final_snapshot_identifier :: Lens' AwsRdsClusterParams Maybe (T.Text)
rc_final_snapshot_identifier :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_final_snapshot_identifier k atom = fmap (\newrc_final_snapshot_identifier -> atom { _rc_final_snapshot_identifier = newrc_final_snapshot_identifier }) (k (_rc_final_snapshot_identifier atom))
-- rc_vpc_security_group_ids :: Lens' AwsRdsClusterParams [TFRef (AwsId AwsSecurityGroup)]
rc_vpc_security_group_ids :: Functor f => ([TFRef (AwsId AwsSecurityGroup)] -> f ([TFRef (AwsId AwsSecurityGroup)])) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_vpc_security_group_ids k atom = fmap (\newrc_vpc_security_group_ids -> atom { _rc_vpc_security_group_ids = newrc_vpc_security_group_ids }) (k (_rc_vpc_security_group_ids atom))
-- rc_db_cluster_parameter_group_name :: Lens' AwsRdsClusterParams Maybe (TFRef T.Text)
rc_db_cluster_parameter_group_name :: Functor f => (Maybe (TFRef T.Text) -> f (Maybe (TFRef T.Text))) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_db_cluster_parameter_group_name k atom = fmap (\newrc_db_cluster_parameter_group_name -> atom { _rc_db_cluster_parameter_group_name = newrc_db_cluster_parameter_group_name }) (k (_rc_db_cluster_parameter_group_name atom))
-- rc_db_subnet_group_name :: Lens' AwsRdsClusterParams Maybe (TFRef T.Text)
rc_db_subnet_group_name :: Functor f => (Maybe (TFRef T.Text) -> f (Maybe (TFRef T.Text))) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_db_subnet_group_name k atom = fmap (\newrc_db_subnet_group_name -> atom { _rc_db_subnet_group_name = newrc_db_subnet_group_name }) (k (_rc_db_subnet_group_name atom))
-- rc_backup_retention_period :: Lens' AwsRdsClusterParams Int
rc_backup_retention_period :: Functor f => (Int -> f (Int)) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_backup_retention_period k atom = fmap (\newrc_backup_retention_period -> atom { _rc_backup_retention_period = newrc_backup_retention_period }) (k (_rc_backup_retention_period atom))
-- rc_tags :: Lens' AwsRdsClusterParams M.Map T.Text T.Text
rc_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsRdsClusterParams -> f AwsRdsClusterParams
rc_tags k atom = fmap (\newrc_tags -> atom { _rc_tags = newrc_tags }) (k (_rc_tags atom))

makeAwsRdsClusterParams :: DBEngine -> T.Text -> T.Text -> AwsRdsClusterParams
makeAwsRdsClusterParams engine masterUsername' masterPassword = AwsRdsClusterParams
  { _rc_engine = engine
  , _rc_master_username' = masterUsername'
  , _rc_master_password = masterPassword
  , _rc_cluster_identifier = ""
  , _rc_engine_version = Nothing
  , _rc_database_name' = ""
  , _rc_port' = Nothing
  , _rc_apply_immediately = False
  , _rc_skip_final_snapshot = False
  , _rc_final_snapshot_identifier = Nothing
  , _rc_vpc_security_group_ids = []
  , _rc_db_cluster_parameter_group_name = Nothing
  , _rc_db_subnet_group_name = Nothing
  , _rc_backup_retention_period = 0
  , _rc_tags = M.empty
  }

instance ToResourceFieldMap AwsRdsClusterParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "cluster_identifier" "" (_rc_cluster_identifier params)
    <> rfmField "engine" (_rc_engine params)
    <> rfmOptionalField "engine_version" (_rc_engine_version params)
    <> rfmOptionalDefField "database_name" "" (_rc_database_name' params)
    <> rfmOptionalField "port" (_rc_port' params)
    <> rfmField "master_username" (_rc_master_username' params)
    <> rfmField "master_password" (_rc_master_password params)
    <> rfmOptionalDefField "apply_immediately" False (_rc_apply_immediately params)
    <> rfmOptionalDefField "skip_final_snapshot" False (_rc_skip_final_snapshot params)
    <> rfmOptionalField "final_snapshot_identifier" (_rc_final_snapshot_identifier params)
    <> rfmOptionalDefField "vpc_security_group_ids" [] (_rc_vpc_security_group_ids params)
    <> rfmOptionalField "db_cluster_parameter_group_name" (_rc_db_cluster_parameter_group_name params)
    <> rfmOptionalField "db_subnet_group_name" (_rc_db_subnet_group_name params)
    <> rfmOptionalDefField "backup_retention_period" 0 (_rc_backup_retention_period params)
    <> rfmOptionalDefField "tags" M.empty (_rc_tags params)
    

instance ToResourceField AwsRdsClusterParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsRdsClusterInstance to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/rds_cluster_instance.html aws_rds_cluster_instance> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'rci_')

awsRdsClusterInstance :: NameElement -> T.Text -> DBInstanceClass ->(AwsRdsClusterInstanceParams -> AwsRdsClusterInstanceParams) -> TF AwsRdsClusterInstance
awsRdsClusterInstance name0 clusterIdentifier instanceClass modf = newAwsRdsClusterInstance name0 (modf (makeAwsRdsClusterInstanceParams clusterIdentifier instanceClass))

awsRdsClusterInstance' :: NameElement -> T.Text -> DBInstanceClass -> TF AwsRdsClusterInstance
awsRdsClusterInstance' name0 clusterIdentifier instanceClass = newAwsRdsClusterInstance name0 (makeAwsRdsClusterInstanceParams clusterIdentifier instanceClass)

newAwsRdsClusterInstance :: NameElement -> AwsRdsClusterInstanceParams -> TF AwsRdsClusterInstance
newAwsRdsClusterInstance name0 params = do
  rid <- mkResource "aws_rds_cluster_instance" name0 (toResourceFieldMap params)
  return AwsRdsClusterInstance
    { rci_id = resourceAttr rid "id"
    , rci_resource = rid
    }

data AwsRdsClusterInstance = AwsRdsClusterInstance
  { rci_id :: TFRef (AwsId AwsRdsClusterInstance)
  , rci_resource :: ResourceId
  }

instance IsResource AwsRdsClusterInstance where
  resourceId = rci_resource

data AwsRdsClusterInstanceParams = AwsRdsClusterInstanceParams
  { _rci_cluster_identifier :: T.Text
  , _rci_instance_class :: DBInstanceClass
  , _rci_count :: Maybe (Int)
  , _rci_identifier :: Maybe (T.Text)
  , _rci_db_parameter_group_name :: Maybe (TFRef T.Text)
  }

-- rci_cluster_identifier :: Lens' AwsRdsClusterInstanceParams T.Text
rci_cluster_identifier :: Functor f => (T.Text -> f (T.Text)) -> AwsRdsClusterInstanceParams -> f AwsRdsClusterInstanceParams
rci_cluster_identifier k atom = fmap (\newrci_cluster_identifier -> atom { _rci_cluster_identifier = newrci_cluster_identifier }) (k (_rci_cluster_identifier atom))
-- rci_count :: Lens' AwsRdsClusterInstanceParams Maybe (Int)
rci_count :: Functor f => (Maybe (Int) -> f (Maybe (Int))) -> AwsRdsClusterInstanceParams -> f AwsRdsClusterInstanceParams
rci_count k atom = fmap (\newrci_count -> atom { _rci_count = newrci_count }) (k (_rci_count atom))
-- rci_identifier :: Lens' AwsRdsClusterInstanceParams Maybe (T.Text)
rci_identifier :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsRdsClusterInstanceParams -> f AwsRdsClusterInstanceParams
rci_identifier k atom = fmap (\newrci_identifier -> atom { _rci_identifier = newrci_identifier }) (k (_rci_identifier atom))
-- rci_instance_class :: Lens' AwsRdsClusterInstanceParams DBInstanceClass
rci_instance_class :: Functor f => (DBInstanceClass -> f (DBInstanceClass)) -> AwsRdsClusterInstanceParams -> f AwsRdsClusterInstanceParams
rci_instance_class k atom = fmap (\newrci_instance_class -> atom { _rci_instance_class = newrci_instance_class }) (k (_rci_instance_class atom))
-- rci_db_parameter_group_name :: Lens' AwsRdsClusterInstanceParams Maybe (TFRef T.Text)
rci_db_parameter_group_name :: Functor f => (Maybe (TFRef T.Text) -> f (Maybe (TFRef T.Text))) -> AwsRdsClusterInstanceParams -> f AwsRdsClusterInstanceParams
rci_db_parameter_group_name k atom = fmap (\newrci_db_parameter_group_name -> atom { _rci_db_parameter_group_name = newrci_db_parameter_group_name }) (k (_rci_db_parameter_group_name atom))

makeAwsRdsClusterInstanceParams :: T.Text -> DBInstanceClass -> AwsRdsClusterInstanceParams
makeAwsRdsClusterInstanceParams clusterIdentifier instanceClass = AwsRdsClusterInstanceParams
  { _rci_cluster_identifier = clusterIdentifier
  , _rci_instance_class = instanceClass
  , _rci_count = Nothing
  , _rci_identifier = Nothing
  , _rci_db_parameter_group_name = Nothing
  }

instance ToResourceFieldMap AwsRdsClusterInstanceParams where
  toResourceFieldMap params
    =  rfmField "cluster_identifier" (_rci_cluster_identifier params)
    <> rfmOptionalField "count" (_rci_count params)
    <> rfmOptionalField "identifier" (_rci_identifier params)
    <> rfmField "instance_class" (_rci_instance_class params)
    <> rfmOptionalField "db_parameter_group_name" (_rci_db_parameter_group_name params)
    

instance ToResourceField AwsRdsClusterInstanceParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsRdsClusterParameterGroup to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/rds_cluster_parameter_group.html aws_rds_cluster_parameter_group> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'rcpg_')

awsRdsClusterParameterGroup :: NameElement -> T.Text -> T.Text ->(AwsRdsClusterParameterGroupParams -> AwsRdsClusterParameterGroupParams) -> TF AwsRdsClusterParameterGroup
awsRdsClusterParameterGroup name0 name' family modf = newAwsRdsClusterParameterGroup name0 (modf (makeAwsRdsClusterParameterGroupParams name' family))

awsRdsClusterParameterGroup' :: NameElement -> T.Text -> T.Text -> TF AwsRdsClusterParameterGroup
awsRdsClusterParameterGroup' name0 name' family = newAwsRdsClusterParameterGroup name0 (makeAwsRdsClusterParameterGroupParams name' family)

newAwsRdsClusterParameterGroup :: NameElement -> AwsRdsClusterParameterGroupParams -> TF AwsRdsClusterParameterGroup
newAwsRdsClusterParameterGroup name0 params = do
  rid <- mkResource "aws_rds_cluster_parameter_group" name0 (toResourceFieldMap params)
  return AwsRdsClusterParameterGroup
    { rcpg_id = resourceAttr rid "id"
    , rcpg_name = resourceAttr rid "name"
    , rcpg_resource = rid
    }

data AwsRdsClusterParameterGroup = AwsRdsClusterParameterGroup
  { rcpg_id :: TFRef (AwsId AwsDbParameterGroup)
  , rcpg_name :: TFRef T.Text
  , rcpg_resource :: ResourceId
  }

instance IsResource AwsRdsClusterParameterGroup where
  resourceId = rcpg_resource

data AwsRdsClusterParameterGroupParams = AwsRdsClusterParameterGroupParams
  { _rcpg_name' :: T.Text
  , _rcpg_family :: T.Text
  , _rcpg_parameter :: [RcpgParameterParams]
  }

-- rcpg_name' :: Lens' AwsRdsClusterParameterGroupParams T.Text
rcpg_name' :: Functor f => (T.Text -> f (T.Text)) -> AwsRdsClusterParameterGroupParams -> f AwsRdsClusterParameterGroupParams
rcpg_name' k atom = fmap (\newrcpg_name' -> atom { _rcpg_name' = newrcpg_name' }) (k (_rcpg_name' atom))
-- rcpg_family :: Lens' AwsRdsClusterParameterGroupParams T.Text
rcpg_family :: Functor f => (T.Text -> f (T.Text)) -> AwsRdsClusterParameterGroupParams -> f AwsRdsClusterParameterGroupParams
rcpg_family k atom = fmap (\newrcpg_family -> atom { _rcpg_family = newrcpg_family }) (k (_rcpg_family atom))
-- rcpg_parameter :: Lens' AwsRdsClusterParameterGroupParams [RcpgParameterParams]
rcpg_parameter :: Functor f => ([RcpgParameterParams] -> f ([RcpgParameterParams])) -> AwsRdsClusterParameterGroupParams -> f AwsRdsClusterParameterGroupParams
rcpg_parameter k atom = fmap (\newrcpg_parameter -> atom { _rcpg_parameter = newrcpg_parameter }) (k (_rcpg_parameter atom))

makeAwsRdsClusterParameterGroupParams :: T.Text -> T.Text -> AwsRdsClusterParameterGroupParams
makeAwsRdsClusterParameterGroupParams name' family = AwsRdsClusterParameterGroupParams
  { _rcpg_name' = name'
  , _rcpg_family = family
  , _rcpg_parameter = []
  }

instance ToResourceFieldMap AwsRdsClusterParameterGroupParams where
  toResourceFieldMap params
    =  rfmField "name" (_rcpg_name' params)
    <> rfmField "family" (_rcpg_family params)
    <> rfmExpandedList "parameter" (_rcpg_parameter params)
    

instance ToResourceField AwsRdsClusterParameterGroupParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data RcpgParameterParams = RcpgParameterParams
  { _rcpgp_name :: T.Text
  , _rcpgp_value :: T.Text
  }
  deriving (Eq)

-- rcpgp_name :: Lens' RcpgParameterParams T.Text
rcpgp_name :: Functor f => (T.Text -> f (T.Text)) -> RcpgParameterParams -> f RcpgParameterParams
rcpgp_name k atom = fmap (\newrcpgp_name -> atom { _rcpgp_name = newrcpgp_name }) (k (_rcpgp_name atom))
-- rcpgp_value :: Lens' RcpgParameterParams T.Text
rcpgp_value :: Functor f => (T.Text -> f (T.Text)) -> RcpgParameterParams -> f RcpgParameterParams
rcpgp_value k atom = fmap (\newrcpgp_value -> atom { _rcpgp_value = newrcpgp_value }) (k (_rcpgp_value atom))

makeRcpgParameterParams :: T.Text -> T.Text -> RcpgParameterParams
makeRcpgParameterParams name value = RcpgParameterParams
  { _rcpgp_name = name
  , _rcpgp_value = value
  }

instance ToResourceFieldMap RcpgParameterParams where
  toResourceFieldMap params
    =  rfmField "name" (_rcpgp_name params)
    <> rfmField "value" (_rcpgp_value params)
    

instance ToResourceField RcpgParameterParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsDbInstance to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/db_instance.html aws_db_instance> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'db_')

awsDbInstance :: NameElement -> Int -> DBEngine -> DBInstanceClass -> T.Text -> T.Text ->(AwsDbInstanceParams -> AwsDbInstanceParams) -> TF AwsDbInstance
awsDbInstance name0 allocatedStorage engine instanceClass username' password modf = newAwsDbInstance name0 (modf (makeAwsDbInstanceParams allocatedStorage engine instanceClass username' password))

awsDbInstance' :: NameElement -> Int -> DBEngine -> DBInstanceClass -> T.Text -> T.Text -> TF AwsDbInstance
awsDbInstance' name0 allocatedStorage engine instanceClass username' password = newAwsDbInstance name0 (makeAwsDbInstanceParams allocatedStorage engine instanceClass username' password)

newAwsDbInstance :: NameElement -> AwsDbInstanceParams -> TF AwsDbInstance
newAwsDbInstance name0 params = do
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

data AwsDbInstanceParams = AwsDbInstanceParams
  { _db_allocated_storage :: Int
  , _db_engine :: DBEngine
  , _db_instance_class :: DBInstanceClass
  , _db_username' :: T.Text
  , _db_password :: T.Text
  , _db_engine_version :: T.Text
  , _db_identifier :: T.Text
  , _db_name' :: T.Text
  , _db_port' :: Maybe (Int)
  , _db_publicly_accessible :: Bool
  , _db_backup_retention_period :: Int
  , _db_vpc_security_group_ids :: [TFRef (AwsId AwsSecurityGroup)]
  , _db_parameter_group_name :: Maybe (TFRef T.Text)
  , _db_db_subnet_group_name :: Maybe (TFRef T.Text)
  , _db_tags :: M.Map T.Text T.Text
  , _db_skip_final_snapshot :: Bool
  , _db_final_snapshot_identifier :: Maybe (T.Text)
  }

-- db_allocated_storage :: Lens' AwsDbInstanceParams Int
db_allocated_storage :: Functor f => (Int -> f (Int)) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_allocated_storage k atom = fmap (\newdb_allocated_storage -> atom { _db_allocated_storage = newdb_allocated_storage }) (k (_db_allocated_storage atom))
-- db_engine :: Lens' AwsDbInstanceParams DBEngine
db_engine :: Functor f => (DBEngine -> f (DBEngine)) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_engine k atom = fmap (\newdb_engine -> atom { _db_engine = newdb_engine }) (k (_db_engine atom))
-- db_engine_version :: Lens' AwsDbInstanceParams T.Text
db_engine_version :: Functor f => (T.Text -> f (T.Text)) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_engine_version k atom = fmap (\newdb_engine_version -> atom { _db_engine_version = newdb_engine_version }) (k (_db_engine_version atom))
-- db_identifier :: Lens' AwsDbInstanceParams T.Text
db_identifier :: Functor f => (T.Text -> f (T.Text)) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_identifier k atom = fmap (\newdb_identifier -> atom { _db_identifier = newdb_identifier }) (k (_db_identifier atom))
-- db_instance_class :: Lens' AwsDbInstanceParams DBInstanceClass
db_instance_class :: Functor f => (DBInstanceClass -> f (DBInstanceClass)) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_instance_class k atom = fmap (\newdb_instance_class -> atom { _db_instance_class = newdb_instance_class }) (k (_db_instance_class atom))
-- db_name' :: Lens' AwsDbInstanceParams T.Text
db_name' :: Functor f => (T.Text -> f (T.Text)) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_name' k atom = fmap (\newdb_name' -> atom { _db_name' = newdb_name' }) (k (_db_name' atom))
-- db_port' :: Lens' AwsDbInstanceParams Maybe (Int)
db_port' :: Functor f => (Maybe (Int) -> f (Maybe (Int))) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_port' k atom = fmap (\newdb_port' -> atom { _db_port' = newdb_port' }) (k (_db_port' atom))
-- db_username' :: Lens' AwsDbInstanceParams T.Text
db_username' :: Functor f => (T.Text -> f (T.Text)) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_username' k atom = fmap (\newdb_username' -> atom { _db_username' = newdb_username' }) (k (_db_username' atom))
-- db_password :: Lens' AwsDbInstanceParams T.Text
db_password :: Functor f => (T.Text -> f (T.Text)) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_password k atom = fmap (\newdb_password -> atom { _db_password = newdb_password }) (k (_db_password atom))
-- db_publicly_accessible :: Lens' AwsDbInstanceParams Bool
db_publicly_accessible :: Functor f => (Bool -> f (Bool)) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_publicly_accessible k atom = fmap (\newdb_publicly_accessible -> atom { _db_publicly_accessible = newdb_publicly_accessible }) (k (_db_publicly_accessible atom))
-- db_backup_retention_period :: Lens' AwsDbInstanceParams Int
db_backup_retention_period :: Functor f => (Int -> f (Int)) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_backup_retention_period k atom = fmap (\newdb_backup_retention_period -> atom { _db_backup_retention_period = newdb_backup_retention_period }) (k (_db_backup_retention_period atom))
-- db_vpc_security_group_ids :: Lens' AwsDbInstanceParams [TFRef (AwsId AwsSecurityGroup)]
db_vpc_security_group_ids :: Functor f => ([TFRef (AwsId AwsSecurityGroup)] -> f ([TFRef (AwsId AwsSecurityGroup)])) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_vpc_security_group_ids k atom = fmap (\newdb_vpc_security_group_ids -> atom { _db_vpc_security_group_ids = newdb_vpc_security_group_ids }) (k (_db_vpc_security_group_ids atom))
-- db_parameter_group_name :: Lens' AwsDbInstanceParams Maybe (TFRef T.Text)
db_parameter_group_name :: Functor f => (Maybe (TFRef T.Text) -> f (Maybe (TFRef T.Text))) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_parameter_group_name k atom = fmap (\newdb_parameter_group_name -> atom { _db_parameter_group_name = newdb_parameter_group_name }) (k (_db_parameter_group_name atom))
-- db_db_subnet_group_name :: Lens' AwsDbInstanceParams Maybe (TFRef T.Text)
db_db_subnet_group_name :: Functor f => (Maybe (TFRef T.Text) -> f (Maybe (TFRef T.Text))) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_db_subnet_group_name k atom = fmap (\newdb_db_subnet_group_name -> atom { _db_db_subnet_group_name = newdb_db_subnet_group_name }) (k (_db_db_subnet_group_name atom))
-- db_tags :: Lens' AwsDbInstanceParams M.Map T.Text T.Text
db_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_tags k atom = fmap (\newdb_tags -> atom { _db_tags = newdb_tags }) (k (_db_tags atom))
-- db_skip_final_snapshot :: Lens' AwsDbInstanceParams Bool
db_skip_final_snapshot :: Functor f => (Bool -> f (Bool)) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_skip_final_snapshot k atom = fmap (\newdb_skip_final_snapshot -> atom { _db_skip_final_snapshot = newdb_skip_final_snapshot }) (k (_db_skip_final_snapshot atom))
-- db_final_snapshot_identifier :: Lens' AwsDbInstanceParams Maybe (T.Text)
db_final_snapshot_identifier :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsDbInstanceParams -> f AwsDbInstanceParams
db_final_snapshot_identifier k atom = fmap (\newdb_final_snapshot_identifier -> atom { _db_final_snapshot_identifier = newdb_final_snapshot_identifier }) (k (_db_final_snapshot_identifier atom))

makeAwsDbInstanceParams :: Int -> DBEngine -> DBInstanceClass -> T.Text -> T.Text -> AwsDbInstanceParams
makeAwsDbInstanceParams allocatedStorage engine instanceClass username' password = AwsDbInstanceParams
  { _db_allocated_storage = allocatedStorage
  , _db_engine = engine
  , _db_instance_class = instanceClass
  , _db_username' = username'
  , _db_password = password
  , _db_engine_version = ""
  , _db_identifier = ""
  , _db_name' = ""
  , _db_port' = Nothing
  , _db_publicly_accessible = False
  , _db_backup_retention_period = 0
  , _db_vpc_security_group_ids = []
  , _db_parameter_group_name = Nothing
  , _db_db_subnet_group_name = Nothing
  , _db_tags = M.empty
  , _db_skip_final_snapshot = False
  , _db_final_snapshot_identifier = Nothing
  }

instance ToResourceFieldMap AwsDbInstanceParams where
  toResourceFieldMap params
    =  rfmField "allocated_storage" (_db_allocated_storage params)
    <> rfmField "engine" (_db_engine params)
    <> rfmOptionalDefField "engine_version" "" (_db_engine_version params)
    <> rfmOptionalDefField "identifier" "" (_db_identifier params)
    <> rfmField "instance_class" (_db_instance_class params)
    <> rfmOptionalDefField "name" "" (_db_name' params)
    <> rfmOptionalField "port" (_db_port' params)
    <> rfmField "username" (_db_username' params)
    <> rfmField "password" (_db_password params)
    <> rfmOptionalDefField "publicly_accessible" False (_db_publicly_accessible params)
    <> rfmOptionalDefField "backup_retention_period" 0 (_db_backup_retention_period params)
    <> rfmOptionalDefField "vpc_security_group_ids" [] (_db_vpc_security_group_ids params)
    <> rfmOptionalField "parameter_group_name" (_db_parameter_group_name params)
    <> rfmOptionalField "db_subnet_group_name" (_db_db_subnet_group_name params)
    <> rfmOptionalDefField "tags" M.empty (_db_tags params)
    <> rfmOptionalDefField "skip_final_snapshot" False (_db_skip_final_snapshot params)
    <> rfmOptionalField "final_snapshot_identifier" (_db_final_snapshot_identifier params)
    

instance ToResourceField AwsDbInstanceParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsDbParameterGroup to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/db_parameter_group.html aws_db_parameter_group> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'dbpg_')

awsDbParameterGroup :: NameElement -> T.Text -> T.Text ->(AwsDbParameterGroupParams -> AwsDbParameterGroupParams) -> TF AwsDbParameterGroup
awsDbParameterGroup name0 name' family modf = newAwsDbParameterGroup name0 (modf (makeAwsDbParameterGroupParams name' family))

awsDbParameterGroup' :: NameElement -> T.Text -> T.Text -> TF AwsDbParameterGroup
awsDbParameterGroup' name0 name' family = newAwsDbParameterGroup name0 (makeAwsDbParameterGroupParams name' family)

newAwsDbParameterGroup :: NameElement -> AwsDbParameterGroupParams -> TF AwsDbParameterGroup
newAwsDbParameterGroup name0 params = do
  rid <- mkResource "aws_db_parameter_group" name0 (toResourceFieldMap params)
  return AwsDbParameterGroup
    { dbpg_id = resourceAttr rid "id"
    , dbpg_name = resourceAttr rid "name"
    , dbpg_resource = rid
    }

data AwsDbParameterGroup = AwsDbParameterGroup
  { dbpg_id :: TFRef (AwsId AwsDbParameterGroup)
  , dbpg_name :: TFRef T.Text
  , dbpg_resource :: ResourceId
  }

instance IsResource AwsDbParameterGroup where
  resourceId = dbpg_resource

data AwsDbParameterGroupParams = AwsDbParameterGroupParams
  { _dbpg_name' :: T.Text
  , _dbpg_family :: T.Text
  , _dbpg_parameter :: [DbpgParameterParams]
  }

-- dbpg_name' :: Lens' AwsDbParameterGroupParams T.Text
dbpg_name' :: Functor f => (T.Text -> f (T.Text)) -> AwsDbParameterGroupParams -> f AwsDbParameterGroupParams
dbpg_name' k atom = fmap (\newdbpg_name' -> atom { _dbpg_name' = newdbpg_name' }) (k (_dbpg_name' atom))
-- dbpg_family :: Lens' AwsDbParameterGroupParams T.Text
dbpg_family :: Functor f => (T.Text -> f (T.Text)) -> AwsDbParameterGroupParams -> f AwsDbParameterGroupParams
dbpg_family k atom = fmap (\newdbpg_family -> atom { _dbpg_family = newdbpg_family }) (k (_dbpg_family atom))
-- dbpg_parameter :: Lens' AwsDbParameterGroupParams [DbpgParameterParams]
dbpg_parameter :: Functor f => ([DbpgParameterParams] -> f ([DbpgParameterParams])) -> AwsDbParameterGroupParams -> f AwsDbParameterGroupParams
dbpg_parameter k atom = fmap (\newdbpg_parameter -> atom { _dbpg_parameter = newdbpg_parameter }) (k (_dbpg_parameter atom))

makeAwsDbParameterGroupParams :: T.Text -> T.Text -> AwsDbParameterGroupParams
makeAwsDbParameterGroupParams name' family = AwsDbParameterGroupParams
  { _dbpg_name' = name'
  , _dbpg_family = family
  , _dbpg_parameter = []
  }

instance ToResourceFieldMap AwsDbParameterGroupParams where
  toResourceFieldMap params
    =  rfmField "name" (_dbpg_name' params)
    <> rfmField "family" (_dbpg_family params)
    <> rfmExpandedList "parameter" (_dbpg_parameter params)
    

instance ToResourceField AwsDbParameterGroupParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data DbpgParameterParams = DbpgParameterParams
  { _dbpgp_name :: T.Text
  , _dbpgp_value :: T.Text
  , _dbpgp_apply_method :: Maybe (T.Text)
  }
  deriving (Eq)

-- dbpgp_name :: Lens' DbpgParameterParams T.Text
dbpgp_name :: Functor f => (T.Text -> f (T.Text)) -> DbpgParameterParams -> f DbpgParameterParams
dbpgp_name k atom = fmap (\newdbpgp_name -> atom { _dbpgp_name = newdbpgp_name }) (k (_dbpgp_name atom))
-- dbpgp_value :: Lens' DbpgParameterParams T.Text
dbpgp_value :: Functor f => (T.Text -> f (T.Text)) -> DbpgParameterParams -> f DbpgParameterParams
dbpgp_value k atom = fmap (\newdbpgp_value -> atom { _dbpgp_value = newdbpgp_value }) (k (_dbpgp_value atom))
-- dbpgp_apply_method :: Lens' DbpgParameterParams Maybe (T.Text)
dbpgp_apply_method :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> DbpgParameterParams -> f DbpgParameterParams
dbpgp_apply_method k atom = fmap (\newdbpgp_apply_method -> atom { _dbpgp_apply_method = newdbpgp_apply_method }) (k (_dbpgp_apply_method atom))

makeDbpgParameterParams :: T.Text -> T.Text -> DbpgParameterParams
makeDbpgParameterParams name value = DbpgParameterParams
  { _dbpgp_name = name
  , _dbpgp_value = value
  , _dbpgp_apply_method = Nothing
  }

instance ToResourceFieldMap DbpgParameterParams where
  toResourceFieldMap params
    =  rfmField "name" (_dbpgp_name params)
    <> rfmField "value" (_dbpgp_value params)
    <> rfmOptionalField "apply_method" (_dbpgp_apply_method params)
    

instance ToResourceField DbpgParameterParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsDbSubnetGroup to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/db_subnet_group.html aws_db_subnet_group> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'dsg_')

awsDbSubnetGroup :: NameElement -> T.Text -> [TFRef (AwsId AwsSubnet)] ->(AwsDbSubnetGroupParams -> AwsDbSubnetGroupParams) -> TF AwsDbSubnetGroup
awsDbSubnetGroup name0 name' subnetIds modf = newAwsDbSubnetGroup name0 (modf (makeAwsDbSubnetGroupParams name' subnetIds))

awsDbSubnetGroup' :: NameElement -> T.Text -> [TFRef (AwsId AwsSubnet)] -> TF AwsDbSubnetGroup
awsDbSubnetGroup' name0 name' subnetIds = newAwsDbSubnetGroup name0 (makeAwsDbSubnetGroupParams name' subnetIds)

newAwsDbSubnetGroup :: NameElement -> AwsDbSubnetGroupParams -> TF AwsDbSubnetGroup
newAwsDbSubnetGroup name0 params = do
  rid <- mkResource "aws_db_subnet_group" name0 (toResourceFieldMap params)
  return AwsDbSubnetGroup
    { dsg_id = resourceAttr rid "id"
    , dsg_name = resourceAttr rid "name"
    , dsg_arn = resourceAttr rid "arn"
    , dsg_resource = rid
    }

data AwsDbSubnetGroup = AwsDbSubnetGroup
  { dsg_id :: TFRef (AwsId AwsDbSubnetGroup)
  , dsg_name :: TFRef T.Text
  , dsg_arn :: TFRef Arn
  , dsg_resource :: ResourceId
  }

instance IsResource AwsDbSubnetGroup where
  resourceId = dsg_resource

data AwsDbSubnetGroupParams = AwsDbSubnetGroupParams
  { _dsg_name' :: T.Text
  , _dsg_subnet_ids :: [TFRef (AwsId AwsSubnet)]
  , _dsg_description :: T.Text
  , _dsg_tags :: M.Map T.Text T.Text
  }

-- dsg_name' :: Lens' AwsDbSubnetGroupParams T.Text
dsg_name' :: Functor f => (T.Text -> f (T.Text)) -> AwsDbSubnetGroupParams -> f AwsDbSubnetGroupParams
dsg_name' k atom = fmap (\newdsg_name' -> atom { _dsg_name' = newdsg_name' }) (k (_dsg_name' atom))
-- dsg_description :: Lens' AwsDbSubnetGroupParams T.Text
dsg_description :: Functor f => (T.Text -> f (T.Text)) -> AwsDbSubnetGroupParams -> f AwsDbSubnetGroupParams
dsg_description k atom = fmap (\newdsg_description -> atom { _dsg_description = newdsg_description }) (k (_dsg_description atom))
-- dsg_subnet_ids :: Lens' AwsDbSubnetGroupParams [TFRef (AwsId AwsSubnet)]
dsg_subnet_ids :: Functor f => ([TFRef (AwsId AwsSubnet)] -> f ([TFRef (AwsId AwsSubnet)])) -> AwsDbSubnetGroupParams -> f AwsDbSubnetGroupParams
dsg_subnet_ids k atom = fmap (\newdsg_subnet_ids -> atom { _dsg_subnet_ids = newdsg_subnet_ids }) (k (_dsg_subnet_ids atom))
-- dsg_tags :: Lens' AwsDbSubnetGroupParams M.Map T.Text T.Text
dsg_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsDbSubnetGroupParams -> f AwsDbSubnetGroupParams
dsg_tags k atom = fmap (\newdsg_tags -> atom { _dsg_tags = newdsg_tags }) (k (_dsg_tags atom))

makeAwsDbSubnetGroupParams :: T.Text -> [TFRef (AwsId AwsSubnet)] -> AwsDbSubnetGroupParams
makeAwsDbSubnetGroupParams name' subnetIds = AwsDbSubnetGroupParams
  { _dsg_name' = name'
  , _dsg_subnet_ids = subnetIds
  , _dsg_description = ""
  , _dsg_tags = M.empty
  }

instance ToResourceFieldMap AwsDbSubnetGroupParams where
  toResourceFieldMap params
    =  rfmField "name" (_dsg_name' params)
    <> rfmOptionalDefField "description" "" (_dsg_description params)
    <> rfmField "subnet_ids" (_dsg_subnet_ids params)
    <> rfmOptionalDefField "tags" M.empty (_dsg_tags params)
    

instance ToResourceField AwsDbSubnetGroupParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsRoute53Zone to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/route53_zone.html aws_route53_zone> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'r53z_')

awsRoute53Zone :: NameElement -> T.Text ->(AwsRoute53ZoneParams -> AwsRoute53ZoneParams) -> TF AwsRoute53Zone
awsRoute53Zone name0 name modf = newAwsRoute53Zone name0 (modf (makeAwsRoute53ZoneParams name))

awsRoute53Zone' :: NameElement -> T.Text -> TF AwsRoute53Zone
awsRoute53Zone' name0 name = newAwsRoute53Zone name0 (makeAwsRoute53ZoneParams name)

newAwsRoute53Zone :: NameElement -> AwsRoute53ZoneParams -> TF AwsRoute53Zone
newAwsRoute53Zone name0 params = do
  rid <- mkResource "aws_route53_zone" name0 (toResourceFieldMap params)
  return AwsRoute53Zone
    { r53z_zone_id = resourceAttr rid "zone_id"
    , r53z_resource = rid
    }

data AwsRoute53Zone = AwsRoute53Zone
  { r53z_zone_id :: TFRef HostedZoneId
  , r53z_resource :: ResourceId
  }

instance IsResource AwsRoute53Zone where
  resourceId = r53z_resource

data AwsRoute53ZoneParams = AwsRoute53ZoneParams
  { _r53z_name :: T.Text
  , _r53z_comment :: T.Text
  , _r53z_vpc_id :: Maybe (TFRef (AwsId AwsVpc))
  , _r53z_vpc_region :: Maybe (AwsRegion)
  , _r53z_force_destroy :: Bool
  , _r53z_tags :: M.Map T.Text T.Text
  }

-- r53z_name :: Lens' AwsRoute53ZoneParams T.Text
r53z_name :: Functor f => (T.Text -> f (T.Text)) -> AwsRoute53ZoneParams -> f AwsRoute53ZoneParams
r53z_name k atom = fmap (\newr53z_name -> atom { _r53z_name = newr53z_name }) (k (_r53z_name atom))
-- r53z_comment :: Lens' AwsRoute53ZoneParams T.Text
r53z_comment :: Functor f => (T.Text -> f (T.Text)) -> AwsRoute53ZoneParams -> f AwsRoute53ZoneParams
r53z_comment k atom = fmap (\newr53z_comment -> atom { _r53z_comment = newr53z_comment }) (k (_r53z_comment atom))
-- r53z_vpc_id :: Lens' AwsRoute53ZoneParams Maybe (TFRef (AwsId AwsVpc))
r53z_vpc_id :: Functor f => (Maybe (TFRef (AwsId AwsVpc)) -> f (Maybe (TFRef (AwsId AwsVpc)))) -> AwsRoute53ZoneParams -> f AwsRoute53ZoneParams
r53z_vpc_id k atom = fmap (\newr53z_vpc_id -> atom { _r53z_vpc_id = newr53z_vpc_id }) (k (_r53z_vpc_id atom))
-- r53z_vpc_region :: Lens' AwsRoute53ZoneParams Maybe (AwsRegion)
r53z_vpc_region :: Functor f => (Maybe (AwsRegion) -> f (Maybe (AwsRegion))) -> AwsRoute53ZoneParams -> f AwsRoute53ZoneParams
r53z_vpc_region k atom = fmap (\newr53z_vpc_region -> atom { _r53z_vpc_region = newr53z_vpc_region }) (k (_r53z_vpc_region atom))
-- r53z_force_destroy :: Lens' AwsRoute53ZoneParams Bool
r53z_force_destroy :: Functor f => (Bool -> f (Bool)) -> AwsRoute53ZoneParams -> f AwsRoute53ZoneParams
r53z_force_destroy k atom = fmap (\newr53z_force_destroy -> atom { _r53z_force_destroy = newr53z_force_destroy }) (k (_r53z_force_destroy atom))
-- r53z_tags :: Lens' AwsRoute53ZoneParams M.Map T.Text T.Text
r53z_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsRoute53ZoneParams -> f AwsRoute53ZoneParams
r53z_tags k atom = fmap (\newr53z_tags -> atom { _r53z_tags = newr53z_tags }) (k (_r53z_tags atom))

makeAwsRoute53ZoneParams :: T.Text -> AwsRoute53ZoneParams
makeAwsRoute53ZoneParams name = AwsRoute53ZoneParams
  { _r53z_name = name
  , _r53z_comment = "Managed by Terraform"
  , _r53z_vpc_id = Nothing
  , _r53z_vpc_region = Nothing
  , _r53z_force_destroy = False
  , _r53z_tags = M.empty
  }

instance ToResourceFieldMap AwsRoute53ZoneParams where
  toResourceFieldMap params
    =  rfmField "name" (_r53z_name params)
    <> rfmOptionalDefField "comment" "Managed by Terraform" (_r53z_comment params)
    <> rfmOptionalField "vpc_id" (_r53z_vpc_id params)
    <> rfmOptionalField "vpc_region" (_r53z_vpc_region params)
    <> rfmOptionalDefField "force_destroy" False (_r53z_force_destroy params)
    <> rfmOptionalDefField "tags" M.empty (_r53z_tags params)
    

instance ToResourceField AwsRoute53ZoneParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data Route53AliasParams = Route53AliasParams
  { _r53a_zone_id :: TFRef HostedZoneId
  , _r53a_name :: TFRef T.Text
  , _r53a_evaluate_target_health :: Bool
  }
  deriving (Eq)

-- r53a_zone_id :: Lens' Route53AliasParams TFRef HostedZoneId
r53a_zone_id :: Functor f => (TFRef HostedZoneId -> f (TFRef HostedZoneId)) -> Route53AliasParams -> f Route53AliasParams
r53a_zone_id k atom = fmap (\newr53a_zone_id -> atom { _r53a_zone_id = newr53a_zone_id }) (k (_r53a_zone_id atom))
-- r53a_name :: Lens' Route53AliasParams TFRef T.Text
r53a_name :: Functor f => (TFRef T.Text -> f (TFRef T.Text)) -> Route53AliasParams -> f Route53AliasParams
r53a_name k atom = fmap (\newr53a_name -> atom { _r53a_name = newr53a_name }) (k (_r53a_name atom))
-- r53a_evaluate_target_health :: Lens' Route53AliasParams Bool
r53a_evaluate_target_health :: Functor f => (Bool -> f (Bool)) -> Route53AliasParams -> f Route53AliasParams
r53a_evaluate_target_health k atom = fmap (\newr53a_evaluate_target_health -> atom { _r53a_evaluate_target_health = newr53a_evaluate_target_health }) (k (_r53a_evaluate_target_health atom))

makeRoute53AliasParams :: TFRef HostedZoneId -> TFRef T.Text -> Bool -> Route53AliasParams
makeRoute53AliasParams zoneId name evaluateTargetHealth = Route53AliasParams
  { _r53a_zone_id = zoneId
  , _r53a_name = name
  , _r53a_evaluate_target_health = evaluateTargetHealth
  }

instance ToResourceFieldMap Route53AliasParams where
  toResourceFieldMap params
    =  rfmField "zone_id" (_r53a_zone_id params)
    <> rfmField "name" (_r53a_name params)
    <> rfmField "evaluate_target_health" (_r53a_evaluate_target_health params)
    

instance ToResourceField Route53AliasParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsRoute53Record to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/route53_record.html aws_route53_record> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'r53r_')

awsRoute53Record :: NameElement -> TFRef HostedZoneId -> T.Text -> Route53RecordType ->(AwsRoute53RecordParams -> AwsRoute53RecordParams) -> TF AwsRoute53Record
awsRoute53Record name0 zoneId name type_ modf = newAwsRoute53Record name0 (modf (makeAwsRoute53RecordParams zoneId name type_))

awsRoute53Record' :: NameElement -> TFRef HostedZoneId -> T.Text -> Route53RecordType -> TF AwsRoute53Record
awsRoute53Record' name0 zoneId name type_ = newAwsRoute53Record name0 (makeAwsRoute53RecordParams zoneId name type_)

newAwsRoute53Record :: NameElement -> AwsRoute53RecordParams -> TF AwsRoute53Record
newAwsRoute53Record name0 params = do
  rid <- mkResource "aws_route53_record" name0 (toResourceFieldMap params)
  return AwsRoute53Record
    { r53r_fqdn = resourceAttr rid "fqdn"
    , r53r_resource = rid
    }

data AwsRoute53Record = AwsRoute53Record
  { r53r_fqdn :: TFRef T.Text
  , r53r_resource :: ResourceId
  }

instance IsResource AwsRoute53Record where
  resourceId = r53r_resource

data AwsRoute53RecordParams = AwsRoute53RecordParams
  { _r53r_zone_id :: TFRef HostedZoneId
  , _r53r_name :: T.Text
  , _r53r_type :: Route53RecordType
  , _r53r_ttl :: Maybe (Int)
  , _r53r_records :: [TFRef IpAddress]
  , _r53r_alias :: Maybe (Route53AliasParams)
  }

-- r53r_zone_id :: Lens' AwsRoute53RecordParams TFRef HostedZoneId
r53r_zone_id :: Functor f => (TFRef HostedZoneId -> f (TFRef HostedZoneId)) -> AwsRoute53RecordParams -> f AwsRoute53RecordParams
r53r_zone_id k atom = fmap (\newr53r_zone_id -> atom { _r53r_zone_id = newr53r_zone_id }) (k (_r53r_zone_id atom))
-- r53r_name :: Lens' AwsRoute53RecordParams T.Text
r53r_name :: Functor f => (T.Text -> f (T.Text)) -> AwsRoute53RecordParams -> f AwsRoute53RecordParams
r53r_name k atom = fmap (\newr53r_name -> atom { _r53r_name = newr53r_name }) (k (_r53r_name atom))
-- r53r_type :: Lens' AwsRoute53RecordParams Route53RecordType
r53r_type :: Functor f => (Route53RecordType -> f (Route53RecordType)) -> AwsRoute53RecordParams -> f AwsRoute53RecordParams
r53r_type k atom = fmap (\newr53r_type -> atom { _r53r_type = newr53r_type }) (k (_r53r_type atom))
-- r53r_ttl :: Lens' AwsRoute53RecordParams Maybe (Int)
r53r_ttl :: Functor f => (Maybe (Int) -> f (Maybe (Int))) -> AwsRoute53RecordParams -> f AwsRoute53RecordParams
r53r_ttl k atom = fmap (\newr53r_ttl -> atom { _r53r_ttl = newr53r_ttl }) (k (_r53r_ttl atom))
-- r53r_records :: Lens' AwsRoute53RecordParams [TFRef IpAddress]
r53r_records :: Functor f => ([TFRef IpAddress] -> f ([TFRef IpAddress])) -> AwsRoute53RecordParams -> f AwsRoute53RecordParams
r53r_records k atom = fmap (\newr53r_records -> atom { _r53r_records = newr53r_records }) (k (_r53r_records atom))
-- r53r_alias :: Lens' AwsRoute53RecordParams Maybe (Route53AliasParams)
r53r_alias :: Functor f => (Maybe (Route53AliasParams) -> f (Maybe (Route53AliasParams))) -> AwsRoute53RecordParams -> f AwsRoute53RecordParams
r53r_alias k atom = fmap (\newr53r_alias -> atom { _r53r_alias = newr53r_alias }) (k (_r53r_alias atom))

makeAwsRoute53RecordParams :: TFRef HostedZoneId -> T.Text -> Route53RecordType -> AwsRoute53RecordParams
makeAwsRoute53RecordParams zoneId name type_ = AwsRoute53RecordParams
  { _r53r_zone_id = zoneId
  , _r53r_name = name
  , _r53r_type = type_
  , _r53r_ttl = Nothing
  , _r53r_records = []
  , _r53r_alias = Nothing
  }

instance ToResourceFieldMap AwsRoute53RecordParams where
  toResourceFieldMap params
    =  rfmField "zone_id" (_r53r_zone_id params)
    <> rfmField "name" (_r53r_name params)
    <> rfmField "type" (_r53r_type params)
    <> rfmOptionalField "ttl" (_r53r_ttl params)
    <> rfmOptionalDefField "records" [] (_r53r_records params)
    <> rfmOptionalField "alias" (_r53r_alias params)
    

instance ToResourceField AwsRoute53RecordParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsSqsQueue to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/sqs_queue.html aws_sqs_queue> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'sqs_')

awsSqsQueue :: NameElement -> T.Text ->(AwsSqsQueueParams -> AwsSqsQueueParams) -> TF AwsSqsQueue
awsSqsQueue name0 name modf = newAwsSqsQueue name0 (modf (makeAwsSqsQueueParams name))

awsSqsQueue' :: NameElement -> T.Text -> TF AwsSqsQueue
awsSqsQueue' name0 name = newAwsSqsQueue name0 (makeAwsSqsQueueParams name)

newAwsSqsQueue :: NameElement -> AwsSqsQueueParams -> TF AwsSqsQueue
newAwsSqsQueue name0 params = do
  rid <- mkResource "aws_sqs_queue" name0 (toResourceFieldMap params)
  return AwsSqsQueue
    { sqs_id = resourceAttr rid "id"
    , sqs_arn = resourceAttr rid "arn"
    , sqs_resource = rid
    }

data AwsSqsQueue = AwsSqsQueue
  { sqs_id :: TFRef (AwsId AwsSqsQueue)
  , sqs_arn :: TFRef Arn
  , sqs_resource :: ResourceId
  }

instance IsResource AwsSqsQueue where
  resourceId = sqs_resource

data AwsSqsQueueParams = AwsSqsQueueParams
  { _sqs_name :: T.Text
  , _sqs_visibility_timeout_seconds :: Int
  , _sqs_message_retention_seconds :: Int
  , _sqs_max_message_size :: Int
  , _sqs_delay_seconds :: Int
  , _sqs_receive_wait_time_seconds :: Int
  , _sqs_policy :: Maybe (T.Text)
  , _sqs_redrive_policy :: Maybe (T.Text)
  , _sqs_fifo_queue :: Bool
  , _sqs_content_based_deduplication :: Bool
  }

-- sqs_name :: Lens' AwsSqsQueueParams T.Text
sqs_name :: Functor f => (T.Text -> f (T.Text)) -> AwsSqsQueueParams -> f AwsSqsQueueParams
sqs_name k atom = fmap (\newsqs_name -> atom { _sqs_name = newsqs_name }) (k (_sqs_name atom))
-- sqs_visibility_timeout_seconds :: Lens' AwsSqsQueueParams Int
sqs_visibility_timeout_seconds :: Functor f => (Int -> f (Int)) -> AwsSqsQueueParams -> f AwsSqsQueueParams
sqs_visibility_timeout_seconds k atom = fmap (\newsqs_visibility_timeout_seconds -> atom { _sqs_visibility_timeout_seconds = newsqs_visibility_timeout_seconds }) (k (_sqs_visibility_timeout_seconds atom))
-- sqs_message_retention_seconds :: Lens' AwsSqsQueueParams Int
sqs_message_retention_seconds :: Functor f => (Int -> f (Int)) -> AwsSqsQueueParams -> f AwsSqsQueueParams
sqs_message_retention_seconds k atom = fmap (\newsqs_message_retention_seconds -> atom { _sqs_message_retention_seconds = newsqs_message_retention_seconds }) (k (_sqs_message_retention_seconds atom))
-- sqs_max_message_size :: Lens' AwsSqsQueueParams Int
sqs_max_message_size :: Functor f => (Int -> f (Int)) -> AwsSqsQueueParams -> f AwsSqsQueueParams
sqs_max_message_size k atom = fmap (\newsqs_max_message_size -> atom { _sqs_max_message_size = newsqs_max_message_size }) (k (_sqs_max_message_size atom))
-- sqs_delay_seconds :: Lens' AwsSqsQueueParams Int
sqs_delay_seconds :: Functor f => (Int -> f (Int)) -> AwsSqsQueueParams -> f AwsSqsQueueParams
sqs_delay_seconds k atom = fmap (\newsqs_delay_seconds -> atom { _sqs_delay_seconds = newsqs_delay_seconds }) (k (_sqs_delay_seconds atom))
-- sqs_receive_wait_time_seconds :: Lens' AwsSqsQueueParams Int
sqs_receive_wait_time_seconds :: Functor f => (Int -> f (Int)) -> AwsSqsQueueParams -> f AwsSqsQueueParams
sqs_receive_wait_time_seconds k atom = fmap (\newsqs_receive_wait_time_seconds -> atom { _sqs_receive_wait_time_seconds = newsqs_receive_wait_time_seconds }) (k (_sqs_receive_wait_time_seconds atom))
-- sqs_policy :: Lens' AwsSqsQueueParams Maybe (T.Text)
sqs_policy :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsSqsQueueParams -> f AwsSqsQueueParams
sqs_policy k atom = fmap (\newsqs_policy -> atom { _sqs_policy = newsqs_policy }) (k (_sqs_policy atom))
-- sqs_redrive_policy :: Lens' AwsSqsQueueParams Maybe (T.Text)
sqs_redrive_policy :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsSqsQueueParams -> f AwsSqsQueueParams
sqs_redrive_policy k atom = fmap (\newsqs_redrive_policy -> atom { _sqs_redrive_policy = newsqs_redrive_policy }) (k (_sqs_redrive_policy atom))
-- sqs_fifo_queue :: Lens' AwsSqsQueueParams Bool
sqs_fifo_queue :: Functor f => (Bool -> f (Bool)) -> AwsSqsQueueParams -> f AwsSqsQueueParams
sqs_fifo_queue k atom = fmap (\newsqs_fifo_queue -> atom { _sqs_fifo_queue = newsqs_fifo_queue }) (k (_sqs_fifo_queue atom))
-- sqs_content_based_deduplication :: Lens' AwsSqsQueueParams Bool
sqs_content_based_deduplication :: Functor f => (Bool -> f (Bool)) -> AwsSqsQueueParams -> f AwsSqsQueueParams
sqs_content_based_deduplication k atom = fmap (\newsqs_content_based_deduplication -> atom { _sqs_content_based_deduplication = newsqs_content_based_deduplication }) (k (_sqs_content_based_deduplication atom))

makeAwsSqsQueueParams :: T.Text -> AwsSqsQueueParams
makeAwsSqsQueueParams name = AwsSqsQueueParams
  { _sqs_name = name
  , _sqs_visibility_timeout_seconds = 30
  , _sqs_message_retention_seconds = 345600
  , _sqs_max_message_size = 262144
  , _sqs_delay_seconds = 0
  , _sqs_receive_wait_time_seconds = 0
  , _sqs_policy = Nothing
  , _sqs_redrive_policy = Nothing
  , _sqs_fifo_queue = False
  , _sqs_content_based_deduplication = False
  }

instance ToResourceFieldMap AwsSqsQueueParams where
  toResourceFieldMap params
    =  rfmField "name" (_sqs_name params)
    <> rfmOptionalDefField "visibility_timeout_seconds" 30 (_sqs_visibility_timeout_seconds params)
    <> rfmOptionalDefField "message_retention_seconds" 345600 (_sqs_message_retention_seconds params)
    <> rfmOptionalDefField "max_message_size" 262144 (_sqs_max_message_size params)
    <> rfmOptionalDefField "delay_seconds" 0 (_sqs_delay_seconds params)
    <> rfmOptionalDefField "receive_wait_time_seconds" 0 (_sqs_receive_wait_time_seconds params)
    <> rfmOptionalField "policy" (_sqs_policy params)
    <> rfmOptionalField "redrive_policy" (_sqs_redrive_policy params)
    <> rfmOptionalDefField "fifo_queue" False (_sqs_fifo_queue params)
    <> rfmOptionalDefField "content_based_deduplication" False (_sqs_content_based_deduplication params)
    

instance ToResourceField AwsSqsQueueParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsSqsQueuePolicy to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/sqs_queue_policy.html aws_sqs_queue_policy> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'sqsp_')

awsSqsQueuePolicy :: NameElement -> T.Text -> T.Text ->(AwsSqsQueuePolicyParams -> AwsSqsQueuePolicyParams) -> TF AwsSqsQueuePolicy
awsSqsQueuePolicy name0 queueUrl policy modf = newAwsSqsQueuePolicy name0 (modf (makeAwsSqsQueuePolicyParams queueUrl policy))

awsSqsQueuePolicy' :: NameElement -> T.Text -> T.Text -> TF AwsSqsQueuePolicy
awsSqsQueuePolicy' name0 queueUrl policy = newAwsSqsQueuePolicy name0 (makeAwsSqsQueuePolicyParams queueUrl policy)

newAwsSqsQueuePolicy :: NameElement -> AwsSqsQueuePolicyParams -> TF AwsSqsQueuePolicy
newAwsSqsQueuePolicy name0 params = do
  rid <- mkResource "aws_sqs_queue_policy" name0 (toResourceFieldMap params)
  return AwsSqsQueuePolicy
    { sqsp_resource = rid
    }

data AwsSqsQueuePolicy = AwsSqsQueuePolicy
  { sqsp_resource :: ResourceId
  }

instance IsResource AwsSqsQueuePolicy where
  resourceId = sqsp_resource

data AwsSqsQueuePolicyParams = AwsSqsQueuePolicyParams
  { _sqsp_queue_url :: T.Text
  , _sqsp_policy :: T.Text
  }

-- sqsp_queue_url :: Lens' AwsSqsQueuePolicyParams T.Text
sqsp_queue_url :: Functor f => (T.Text -> f (T.Text)) -> AwsSqsQueuePolicyParams -> f AwsSqsQueuePolicyParams
sqsp_queue_url k atom = fmap (\newsqsp_queue_url -> atom { _sqsp_queue_url = newsqsp_queue_url }) (k (_sqsp_queue_url atom))
-- sqsp_policy :: Lens' AwsSqsQueuePolicyParams T.Text
sqsp_policy :: Functor f => (T.Text -> f (T.Text)) -> AwsSqsQueuePolicyParams -> f AwsSqsQueuePolicyParams
sqsp_policy k atom = fmap (\newsqsp_policy -> atom { _sqsp_policy = newsqsp_policy }) (k (_sqsp_policy atom))

makeAwsSqsQueuePolicyParams :: T.Text -> T.Text -> AwsSqsQueuePolicyParams
makeAwsSqsQueuePolicyParams queueUrl policy = AwsSqsQueuePolicyParams
  { _sqsp_queue_url = queueUrl
  , _sqsp_policy = policy
  }

instance ToResourceFieldMap AwsSqsQueuePolicyParams where
  toResourceFieldMap params
    =  rfmField "queue_url" (_sqsp_queue_url params)
    <> rfmField "policy" (_sqsp_policy params)
    

instance ToResourceField AwsSqsQueuePolicyParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsEcrRepository to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/ecr_repository.html aws_ecr_repository> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'ecr_')

awsEcrRepository :: NameElement -> T.Text ->(AwsEcrRepositoryParams -> AwsEcrRepositoryParams) -> TF AwsEcrRepository
awsEcrRepository name0 name' modf = newAwsEcrRepository name0 (modf (makeAwsEcrRepositoryParams name'))

awsEcrRepository' :: NameElement -> T.Text -> TF AwsEcrRepository
awsEcrRepository' name0 name' = newAwsEcrRepository name0 (makeAwsEcrRepositoryParams name')

newAwsEcrRepository :: NameElement -> AwsEcrRepositoryParams -> TF AwsEcrRepository
newAwsEcrRepository name0 params = do
  rid <- mkResource "aws_ecr_repository" name0 (toResourceFieldMap params)
  return AwsEcrRepository
    { ecr_arn = resourceAttr rid "arn"
    , ecr_name = resourceAttr rid "name"
    , ecr_registry_id = resourceAttr rid "registry_id"
    , ecr_repository_url = resourceAttr rid "repository_url"
    , ecr_resource = rid
    }

data AwsEcrRepository = AwsEcrRepository
  { ecr_arn :: TFRef Arn
  , ecr_name :: TFRef T.Text
  , ecr_registry_id :: TFRef T.Text
  , ecr_repository_url :: TFRef T.Text
  , ecr_resource :: ResourceId
  }

instance IsResource AwsEcrRepository where
  resourceId = ecr_resource

data AwsEcrRepositoryParams = AwsEcrRepositoryParams
  { _ecr_name' :: T.Text
  }

-- ecr_name' :: Lens' AwsEcrRepositoryParams T.Text
ecr_name' :: Functor f => (T.Text -> f (T.Text)) -> AwsEcrRepositoryParams -> f AwsEcrRepositoryParams
ecr_name' k atom = fmap (\newecr_name' -> atom { _ecr_name' = newecr_name' }) (k (_ecr_name' atom))

makeAwsEcrRepositoryParams :: T.Text -> AwsEcrRepositoryParams
makeAwsEcrRepositoryParams name' = AwsEcrRepositoryParams
  { _ecr_name' = name'
  }

instance ToResourceFieldMap AwsEcrRepositoryParams where
  toResourceFieldMap params
    =  rfmField "name" (_ecr_name' params)
    

instance ToResourceField AwsEcrRepositoryParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsCloudwatchLogGroup to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/cloudwatch_log_group.html aws_cloudwatch_log_group> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'cwlg_')

awsCloudwatchLogGroup :: NameElement -> T.Text ->(AwsCloudwatchLogGroupParams -> AwsCloudwatchLogGroupParams) -> TF AwsCloudwatchLogGroup
awsCloudwatchLogGroup name0 name' modf = newAwsCloudwatchLogGroup name0 (modf (makeAwsCloudwatchLogGroupParams name'))

awsCloudwatchLogGroup' :: NameElement -> T.Text -> TF AwsCloudwatchLogGroup
awsCloudwatchLogGroup' name0 name' = newAwsCloudwatchLogGroup name0 (makeAwsCloudwatchLogGroupParams name')

newAwsCloudwatchLogGroup :: NameElement -> AwsCloudwatchLogGroupParams -> TF AwsCloudwatchLogGroup
newAwsCloudwatchLogGroup name0 params = do
  rid <- mkResource "aws_cloudwatch_log_group" name0 (toResourceFieldMap params)
  return AwsCloudwatchLogGroup
    { cwlg_arn = resourceAttr rid "arn"
    , cwlg_resource = rid
    }

data AwsCloudwatchLogGroup = AwsCloudwatchLogGroup
  { cwlg_arn :: TFRef Arn
  , cwlg_resource :: ResourceId
  }

instance IsResource AwsCloudwatchLogGroup where
  resourceId = cwlg_resource

data AwsCloudwatchLogGroupParams = AwsCloudwatchLogGroupParams
  { _cwlg_name' :: T.Text
  , _cwlg_name_prefix' :: Maybe (T.Text)
  , _cwlg_retention_in_days' :: Maybe (T.Text)
  , _cwlg_kms_key_id :: Maybe (T.Text)
  , _cwlg_tags :: M.Map T.Text T.Text
  }

-- cwlg_name' :: Lens' AwsCloudwatchLogGroupParams T.Text
cwlg_name' :: Functor f => (T.Text -> f (T.Text)) -> AwsCloudwatchLogGroupParams -> f AwsCloudwatchLogGroupParams
cwlg_name' k atom = fmap (\newcwlg_name' -> atom { _cwlg_name' = newcwlg_name' }) (k (_cwlg_name' atom))
-- cwlg_name_prefix' :: Lens' AwsCloudwatchLogGroupParams Maybe (T.Text)
cwlg_name_prefix' :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsCloudwatchLogGroupParams -> f AwsCloudwatchLogGroupParams
cwlg_name_prefix' k atom = fmap (\newcwlg_name_prefix' -> atom { _cwlg_name_prefix' = newcwlg_name_prefix' }) (k (_cwlg_name_prefix' atom))
-- cwlg_retention_in_days' :: Lens' AwsCloudwatchLogGroupParams Maybe (T.Text)
cwlg_retention_in_days' :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsCloudwatchLogGroupParams -> f AwsCloudwatchLogGroupParams
cwlg_retention_in_days' k atom = fmap (\newcwlg_retention_in_days' -> atom { _cwlg_retention_in_days' = newcwlg_retention_in_days' }) (k (_cwlg_retention_in_days' atom))
-- cwlg_kms_key_id :: Lens' AwsCloudwatchLogGroupParams Maybe (T.Text)
cwlg_kms_key_id :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsCloudwatchLogGroupParams -> f AwsCloudwatchLogGroupParams
cwlg_kms_key_id k atom = fmap (\newcwlg_kms_key_id -> atom { _cwlg_kms_key_id = newcwlg_kms_key_id }) (k (_cwlg_kms_key_id atom))
-- cwlg_tags :: Lens' AwsCloudwatchLogGroupParams M.Map T.Text T.Text
cwlg_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsCloudwatchLogGroupParams -> f AwsCloudwatchLogGroupParams
cwlg_tags k atom = fmap (\newcwlg_tags -> atom { _cwlg_tags = newcwlg_tags }) (k (_cwlg_tags atom))

makeAwsCloudwatchLogGroupParams :: T.Text -> AwsCloudwatchLogGroupParams
makeAwsCloudwatchLogGroupParams name' = AwsCloudwatchLogGroupParams
  { _cwlg_name' = name'
  , _cwlg_name_prefix' = Nothing
  , _cwlg_retention_in_days' = Nothing
  , _cwlg_kms_key_id = Nothing
  , _cwlg_tags = M.empty
  }

instance ToResourceFieldMap AwsCloudwatchLogGroupParams where
  toResourceFieldMap params
    =  rfmField "name" (_cwlg_name' params)
    <> rfmOptionalField "name_prefix" (_cwlg_name_prefix' params)
    <> rfmOptionalField "retention_in_days" (_cwlg_retention_in_days' params)
    <> rfmOptionalField "kms_key_id" (_cwlg_kms_key_id params)
    <> rfmOptionalDefField "tags" M.empty (_cwlg_tags params)
    

instance ToResourceField AwsCloudwatchLogGroupParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data EbsOptionsParams = EbsOptionsParams
  { _edeo_ebs_enabled :: Bool
  , _edeo_volume_type :: Maybe (T.Text)
  , _edeo_volume_size :: Maybe (Int)
  , _edeo_iops :: Maybe (Int)
  }
  deriving (Eq)

-- edeo_ebs_enabled :: Lens' EbsOptionsParams Bool
edeo_ebs_enabled :: Functor f => (Bool -> f (Bool)) -> EbsOptionsParams -> f EbsOptionsParams
edeo_ebs_enabled k atom = fmap (\newedeo_ebs_enabled -> atom { _edeo_ebs_enabled = newedeo_ebs_enabled }) (k (_edeo_ebs_enabled atom))
-- edeo_volume_type :: Lens' EbsOptionsParams Maybe (T.Text)
edeo_volume_type :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> EbsOptionsParams -> f EbsOptionsParams
edeo_volume_type k atom = fmap (\newedeo_volume_type -> atom { _edeo_volume_type = newedeo_volume_type }) (k (_edeo_volume_type atom))
-- edeo_volume_size :: Lens' EbsOptionsParams Maybe (Int)
edeo_volume_size :: Functor f => (Maybe (Int) -> f (Maybe (Int))) -> EbsOptionsParams -> f EbsOptionsParams
edeo_volume_size k atom = fmap (\newedeo_volume_size -> atom { _edeo_volume_size = newedeo_volume_size }) (k (_edeo_volume_size atom))
-- edeo_iops :: Lens' EbsOptionsParams Maybe (Int)
edeo_iops :: Functor f => (Maybe (Int) -> f (Maybe (Int))) -> EbsOptionsParams -> f EbsOptionsParams
edeo_iops k atom = fmap (\newedeo_iops -> atom { _edeo_iops = newedeo_iops }) (k (_edeo_iops atom))

makeEbsOptionsParams :: Bool -> EbsOptionsParams
makeEbsOptionsParams ebsEnabled = EbsOptionsParams
  { _edeo_ebs_enabled = ebsEnabled
  , _edeo_volume_type = Nothing
  , _edeo_volume_size = Nothing
  , _edeo_iops = Nothing
  }

instance ToResourceFieldMap EbsOptionsParams where
  toResourceFieldMap params
    =  rfmField "ebs_enabled" (_edeo_ebs_enabled params)
    <> rfmOptionalField "volume_type" (_edeo_volume_type params)
    <> rfmOptionalField "volume_size" (_edeo_volume_size params)
    <> rfmOptionalField "iops" (_edeo_iops params)
    

instance ToResourceField EbsOptionsParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data ClusterConfigParams = ClusterConfigParams
  { _edcc_instance_type :: Maybe (InstanceType)
  , _edcc_instance_count :: Maybe (Int)
  , _edcc_dedicated_master_enabled :: Maybe (Bool)
  , _edcc_dedicated_master_type :: Maybe (InstanceType)
  , _edcc_dedicated_master_count :: Maybe (Int)
  , _edcc_zone_awareness_enabled :: Maybe (Bool)
  }
  deriving (Eq)

-- edcc_instance_type :: Lens' ClusterConfigParams Maybe (InstanceType)
edcc_instance_type :: Functor f => (Maybe (InstanceType) -> f (Maybe (InstanceType))) -> ClusterConfigParams -> f ClusterConfigParams
edcc_instance_type k atom = fmap (\newedcc_instance_type -> atom { _edcc_instance_type = newedcc_instance_type }) (k (_edcc_instance_type atom))
-- edcc_instance_count :: Lens' ClusterConfigParams Maybe (Int)
edcc_instance_count :: Functor f => (Maybe (Int) -> f (Maybe (Int))) -> ClusterConfigParams -> f ClusterConfigParams
edcc_instance_count k atom = fmap (\newedcc_instance_count -> atom { _edcc_instance_count = newedcc_instance_count }) (k (_edcc_instance_count atom))
-- edcc_dedicated_master_enabled :: Lens' ClusterConfigParams Maybe (Bool)
edcc_dedicated_master_enabled :: Functor f => (Maybe (Bool) -> f (Maybe (Bool))) -> ClusterConfigParams -> f ClusterConfigParams
edcc_dedicated_master_enabled k atom = fmap (\newedcc_dedicated_master_enabled -> atom { _edcc_dedicated_master_enabled = newedcc_dedicated_master_enabled }) (k (_edcc_dedicated_master_enabled atom))
-- edcc_dedicated_master_type :: Lens' ClusterConfigParams Maybe (InstanceType)
edcc_dedicated_master_type :: Functor f => (Maybe (InstanceType) -> f (Maybe (InstanceType))) -> ClusterConfigParams -> f ClusterConfigParams
edcc_dedicated_master_type k atom = fmap (\newedcc_dedicated_master_type -> atom { _edcc_dedicated_master_type = newedcc_dedicated_master_type }) (k (_edcc_dedicated_master_type atom))
-- edcc_dedicated_master_count :: Lens' ClusterConfigParams Maybe (Int)
edcc_dedicated_master_count :: Functor f => (Maybe (Int) -> f (Maybe (Int))) -> ClusterConfigParams -> f ClusterConfigParams
edcc_dedicated_master_count k atom = fmap (\newedcc_dedicated_master_count -> atom { _edcc_dedicated_master_count = newedcc_dedicated_master_count }) (k (_edcc_dedicated_master_count atom))
-- edcc_zone_awareness_enabled :: Lens' ClusterConfigParams Maybe (Bool)
edcc_zone_awareness_enabled :: Functor f => (Maybe (Bool) -> f (Maybe (Bool))) -> ClusterConfigParams -> f ClusterConfigParams
edcc_zone_awareness_enabled k atom = fmap (\newedcc_zone_awareness_enabled -> atom { _edcc_zone_awareness_enabled = newedcc_zone_awareness_enabled }) (k (_edcc_zone_awareness_enabled atom))

makeClusterConfigParams ::  ClusterConfigParams
makeClusterConfigParams  = ClusterConfigParams
  { _edcc_instance_type = Nothing
  , _edcc_instance_count = Nothing
  , _edcc_dedicated_master_enabled = Nothing
  , _edcc_dedicated_master_type = Nothing
  , _edcc_dedicated_master_count = Nothing
  , _edcc_zone_awareness_enabled = Nothing
  }

instance ToResourceFieldMap ClusterConfigParams where
  toResourceFieldMap params
    =  rfmOptionalField "instance_type" (_edcc_instance_type params)
    <> rfmOptionalField "instance_count" (_edcc_instance_count params)
    <> rfmOptionalField "dedicated_master_enabled" (_edcc_dedicated_master_enabled params)
    <> rfmOptionalField "dedicated_master_type" (_edcc_dedicated_master_type params)
    <> rfmOptionalField "dedicated_master_count" (_edcc_dedicated_master_count params)
    <> rfmOptionalField "zone_awareness_enabled" (_edcc_zone_awareness_enabled params)
    

instance ToResourceField ClusterConfigParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data SnapshotOptionsParams = SnapshotOptionsParams
  { _edso_automated_snapshot_start_hour :: Int
  }
  deriving (Eq)

-- edso_automated_snapshot_start_hour :: Lens' SnapshotOptionsParams Int
edso_automated_snapshot_start_hour :: Functor f => (Int -> f (Int)) -> SnapshotOptionsParams -> f SnapshotOptionsParams
edso_automated_snapshot_start_hour k atom = fmap (\newedso_automated_snapshot_start_hour -> atom { _edso_automated_snapshot_start_hour = newedso_automated_snapshot_start_hour }) (k (_edso_automated_snapshot_start_hour atom))

makeSnapshotOptionsParams :: Int -> SnapshotOptionsParams
makeSnapshotOptionsParams automatedSnapshotStartHour = SnapshotOptionsParams
  { _edso_automated_snapshot_start_hour = automatedSnapshotStartHour
  }

instance ToResourceFieldMap SnapshotOptionsParams where
  toResourceFieldMap params
    =  rfmField "automated_snapshot_start_hour" (_edso_automated_snapshot_start_hour params)
    

instance ToResourceField SnapshotOptionsParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsElasticsearchDomain to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/elasticsearch_domain.html aws_elasticsearch_domain> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'ed_')

awsElasticsearchDomain :: NameElement -> T.Text ->(AwsElasticsearchDomainParams -> AwsElasticsearchDomainParams) -> TF AwsElasticsearchDomain
awsElasticsearchDomain name0 domainName' modf = newAwsElasticsearchDomain name0 (modf (makeAwsElasticsearchDomainParams domainName'))

awsElasticsearchDomain' :: NameElement -> T.Text -> TF AwsElasticsearchDomain
awsElasticsearchDomain' name0 domainName' = newAwsElasticsearchDomain name0 (makeAwsElasticsearchDomainParams domainName')

newAwsElasticsearchDomain :: NameElement -> AwsElasticsearchDomainParams -> TF AwsElasticsearchDomain
newAwsElasticsearchDomain name0 params = do
  rid <- mkResource "aws_elasticsearch_domain" name0 (toResourceFieldMap params)
  return AwsElasticsearchDomain
    { ed_arn = resourceAttr rid "arn"
    , ed_domain_id = resourceAttr rid "domain_id"
    , ed_domain_name = resourceAttr rid "domain_name"
    , ed_endpoint = resourceAttr rid "endpoint"
    , ed_resource = rid
    }

data AwsElasticsearchDomain = AwsElasticsearchDomain
  { ed_arn :: TFRef Arn
  , ed_domain_id :: TFRef T.Text
  , ed_domain_name :: TFRef T.Text
  , ed_endpoint :: TFRef T.Text
  , ed_resource :: ResourceId
  }

instance IsResource AwsElasticsearchDomain where
  resourceId = ed_resource

data AwsElasticsearchDomainParams = AwsElasticsearchDomainParams
  { _ed_domain_name' :: T.Text
  , _ed_access_policies :: Maybe (T.Text)
  , _ed_advanced_options :: M.Map T.Text T.Text
  , _ed_ebs_options :: Maybe (EbsOptionsParams)
  , _ed_cluster_config :: Maybe (ClusterConfigParams)
  , _ed_snapshot_options :: Maybe (SnapshotOptionsParams)
  , _ed_elasticsearch_version :: T.Text
  , _ed_tags :: M.Map T.Text T.Text
  }

-- ed_domain_name' :: Lens' AwsElasticsearchDomainParams T.Text
ed_domain_name' :: Functor f => (T.Text -> f (T.Text)) -> AwsElasticsearchDomainParams -> f AwsElasticsearchDomainParams
ed_domain_name' k atom = fmap (\newed_domain_name' -> atom { _ed_domain_name' = newed_domain_name' }) (k (_ed_domain_name' atom))
-- ed_access_policies :: Lens' AwsElasticsearchDomainParams Maybe (T.Text)
ed_access_policies :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsElasticsearchDomainParams -> f AwsElasticsearchDomainParams
ed_access_policies k atom = fmap (\newed_access_policies -> atom { _ed_access_policies = newed_access_policies }) (k (_ed_access_policies atom))
-- ed_advanced_options :: Lens' AwsElasticsearchDomainParams M.Map T.Text T.Text
ed_advanced_options :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsElasticsearchDomainParams -> f AwsElasticsearchDomainParams
ed_advanced_options k atom = fmap (\newed_advanced_options -> atom { _ed_advanced_options = newed_advanced_options }) (k (_ed_advanced_options atom))
-- ed_ebs_options :: Lens' AwsElasticsearchDomainParams Maybe (EbsOptionsParams)
ed_ebs_options :: Functor f => (Maybe (EbsOptionsParams) -> f (Maybe (EbsOptionsParams))) -> AwsElasticsearchDomainParams -> f AwsElasticsearchDomainParams
ed_ebs_options k atom = fmap (\newed_ebs_options -> atom { _ed_ebs_options = newed_ebs_options }) (k (_ed_ebs_options atom))
-- ed_cluster_config :: Lens' AwsElasticsearchDomainParams Maybe (ClusterConfigParams)
ed_cluster_config :: Functor f => (Maybe (ClusterConfigParams) -> f (Maybe (ClusterConfigParams))) -> AwsElasticsearchDomainParams -> f AwsElasticsearchDomainParams
ed_cluster_config k atom = fmap (\newed_cluster_config -> atom { _ed_cluster_config = newed_cluster_config }) (k (_ed_cluster_config atom))
-- ed_snapshot_options :: Lens' AwsElasticsearchDomainParams Maybe (SnapshotOptionsParams)
ed_snapshot_options :: Functor f => (Maybe (SnapshotOptionsParams) -> f (Maybe (SnapshotOptionsParams))) -> AwsElasticsearchDomainParams -> f AwsElasticsearchDomainParams
ed_snapshot_options k atom = fmap (\newed_snapshot_options -> atom { _ed_snapshot_options = newed_snapshot_options }) (k (_ed_snapshot_options atom))
-- ed_elasticsearch_version :: Lens' AwsElasticsearchDomainParams T.Text
ed_elasticsearch_version :: Functor f => (T.Text -> f (T.Text)) -> AwsElasticsearchDomainParams -> f AwsElasticsearchDomainParams
ed_elasticsearch_version k atom = fmap (\newed_elasticsearch_version -> atom { _ed_elasticsearch_version = newed_elasticsearch_version }) (k (_ed_elasticsearch_version atom))
-- ed_tags :: Lens' AwsElasticsearchDomainParams M.Map T.Text T.Text
ed_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsElasticsearchDomainParams -> f AwsElasticsearchDomainParams
ed_tags k atom = fmap (\newed_tags -> atom { _ed_tags = newed_tags }) (k (_ed_tags atom))

makeAwsElasticsearchDomainParams :: T.Text -> AwsElasticsearchDomainParams
makeAwsElasticsearchDomainParams domainName' = AwsElasticsearchDomainParams
  { _ed_domain_name' = domainName'
  , _ed_access_policies = Nothing
  , _ed_advanced_options = M.empty
  , _ed_ebs_options = Nothing
  , _ed_cluster_config = Nothing
  , _ed_snapshot_options = Nothing
  , _ed_elasticsearch_version = "1.5"
  , _ed_tags = M.empty
  }

instance ToResourceFieldMap AwsElasticsearchDomainParams where
  toResourceFieldMap params
    =  rfmField "domain_name" (_ed_domain_name' params)
    <> rfmOptionalField "access_policies" (_ed_access_policies params)
    <> rfmOptionalDefField "advanced_options" M.empty (_ed_advanced_options params)
    <> rfmOptionalField "ebs_options" (_ed_ebs_options params)
    <> rfmOptionalField "cluster_config" (_ed_cluster_config params)
    <> rfmOptionalField "snapshot_options" (_ed_snapshot_options params)
    <> rfmOptionalDefField "elasticsearch_version" "1.5" (_ed_elasticsearch_version params)
    <> rfmOptionalDefField "tags" M.empty (_ed_tags params)
    

instance ToResourceField AwsElasticsearchDomainParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsElasticsearchDomainPolicy to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/elasticsearch_domain_policy.html aws_elasticsearch_domain_policy> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'edp_')

awsElasticsearchDomainPolicy :: NameElement -> T.Text -> T.Text ->(AwsElasticsearchDomainPolicyParams -> AwsElasticsearchDomainPolicyParams) -> TF AwsElasticsearchDomainPolicy
awsElasticsearchDomainPolicy name0 domainName accessPolicies modf = newAwsElasticsearchDomainPolicy name0 (modf (makeAwsElasticsearchDomainPolicyParams domainName accessPolicies))

awsElasticsearchDomainPolicy' :: NameElement -> T.Text -> T.Text -> TF AwsElasticsearchDomainPolicy
awsElasticsearchDomainPolicy' name0 domainName accessPolicies = newAwsElasticsearchDomainPolicy name0 (makeAwsElasticsearchDomainPolicyParams domainName accessPolicies)

newAwsElasticsearchDomainPolicy :: NameElement -> AwsElasticsearchDomainPolicyParams -> TF AwsElasticsearchDomainPolicy
newAwsElasticsearchDomainPolicy name0 params = do
  rid <- mkResource "aws_elasticsearch_domain_policy" name0 (toResourceFieldMap params)
  return AwsElasticsearchDomainPolicy
    { edp_arn = resourceAttr rid "arn"
    , edp_resource = rid
    }

data AwsElasticsearchDomainPolicy = AwsElasticsearchDomainPolicy
  { edp_arn :: TFRef Arn
  , edp_resource :: ResourceId
  }

instance IsResource AwsElasticsearchDomainPolicy where
  resourceId = edp_resource

data AwsElasticsearchDomainPolicyParams = AwsElasticsearchDomainPolicyParams
  { _edp_domain_name :: T.Text
  , _edp_access_policies :: T.Text
  }

-- edp_domain_name :: Lens' AwsElasticsearchDomainPolicyParams T.Text
edp_domain_name :: Functor f => (T.Text -> f (T.Text)) -> AwsElasticsearchDomainPolicyParams -> f AwsElasticsearchDomainPolicyParams
edp_domain_name k atom = fmap (\newedp_domain_name -> atom { _edp_domain_name = newedp_domain_name }) (k (_edp_domain_name atom))
-- edp_access_policies :: Lens' AwsElasticsearchDomainPolicyParams T.Text
edp_access_policies :: Functor f => (T.Text -> f (T.Text)) -> AwsElasticsearchDomainPolicyParams -> f AwsElasticsearchDomainPolicyParams
edp_access_policies k atom = fmap (\newedp_access_policies -> atom { _edp_access_policies = newedp_access_policies }) (k (_edp_access_policies atom))

makeAwsElasticsearchDomainPolicyParams :: T.Text -> T.Text -> AwsElasticsearchDomainPolicyParams
makeAwsElasticsearchDomainPolicyParams domainName accessPolicies = AwsElasticsearchDomainPolicyParams
  { _edp_domain_name = domainName
  , _edp_access_policies = accessPolicies
  }

instance ToResourceFieldMap AwsElasticsearchDomainPolicyParams where
  toResourceFieldMap params
    =  rfmField "domain_name" (_edp_domain_name params)
    <> rfmField "access_policies" (_edp_access_policies params)
    

instance ToResourceField AwsElasticsearchDomainPolicyParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsLb to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/lb.html aws_lb> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'lb_')

awsLb :: NameElement -> (AwsLbParams -> AwsLbParams) -> TF AwsLb
awsLb name0  modf = newAwsLb name0 (modf (makeAwsLbParams ))

awsLb' :: NameElement ->  TF AwsLb
awsLb' name0  = newAwsLb name0 (makeAwsLbParams )

newAwsLb :: NameElement -> AwsLbParams -> TF AwsLb
newAwsLb name0 params = do
  rid <- mkResource "aws_lb" name0 (toResourceFieldMap params)
  return AwsLb
    { lb_id = resourceAttr rid "id"
    , lb_arn = resourceAttr rid "arn"
    , lb_dns_name = resourceAttr rid "dns_name"
    , lb_canonical_hosted_zone_id = resourceAttr rid "canonical_hosted_zone_id"
    , lb_zone_id = resourceAttr rid "zone_id"
    , lb_resource = rid
    }

data AwsLb = AwsLb
  { lb_id :: TFRef (AwsId AwsLb)
  , lb_arn :: TFRef (AwsArn AwsLb)
  , lb_dns_name :: TFRef T.Text
  , lb_canonical_hosted_zone_id :: TFRef T.Text
  , lb_zone_id :: TFRef T.Text
  , lb_resource :: ResourceId
  }

instance IsResource AwsLb where
  resourceId = lb_resource

data AwsLbParams = AwsLbParams
  { _lb_name :: Maybe (T.Text)
  , _lb_name_prefix :: Maybe (T.Text)
  , _lb_internal :: Bool
  , _lb_load_balancer_type :: LoadBalancerType
  , _lb_security_groups :: [TFRef (AwsId AwsSecurityGroup)]
  , _lb_access_logs :: Maybe (AccessLogsParams)
  , _lb_subnets :: [TFRef (AwsId AwsSubnet)]
  , _lb_subnet_mapping :: Maybe (SubnetMappingParams)
  , _lb_idle_timeout :: Int
  , _lb_enable_deletion_protection :: Bool
  , _lb_enable_cross_zone_load_balancing :: Bool
  , _lb_enable_http2 :: Bool
  , _lb_ip_address_type :: Maybe (T.Text)
  , _lb_tags :: M.Map T.Text T.Text
  }

-- lb_name :: Lens' AwsLbParams Maybe (T.Text)
lb_name :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsLbParams -> f AwsLbParams
lb_name k atom = fmap (\newlb_name -> atom { _lb_name = newlb_name }) (k (_lb_name atom))
-- lb_name_prefix :: Lens' AwsLbParams Maybe (T.Text)
lb_name_prefix :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsLbParams -> f AwsLbParams
lb_name_prefix k atom = fmap (\newlb_name_prefix -> atom { _lb_name_prefix = newlb_name_prefix }) (k (_lb_name_prefix atom))
-- lb_internal :: Lens' AwsLbParams Bool
lb_internal :: Functor f => (Bool -> f (Bool)) -> AwsLbParams -> f AwsLbParams
lb_internal k atom = fmap (\newlb_internal -> atom { _lb_internal = newlb_internal }) (k (_lb_internal atom))
-- lb_load_balancer_type :: Lens' AwsLbParams LoadBalancerType
lb_load_balancer_type :: Functor f => (LoadBalancerType -> f (LoadBalancerType)) -> AwsLbParams -> f AwsLbParams
lb_load_balancer_type k atom = fmap (\newlb_load_balancer_type -> atom { _lb_load_balancer_type = newlb_load_balancer_type }) (k (_lb_load_balancer_type atom))
-- lb_security_groups :: Lens' AwsLbParams [TFRef (AwsId AwsSecurityGroup)]
lb_security_groups :: Functor f => ([TFRef (AwsId AwsSecurityGroup)] -> f ([TFRef (AwsId AwsSecurityGroup)])) -> AwsLbParams -> f AwsLbParams
lb_security_groups k atom = fmap (\newlb_security_groups -> atom { _lb_security_groups = newlb_security_groups }) (k (_lb_security_groups atom))
-- lb_access_logs :: Lens' AwsLbParams Maybe (AccessLogsParams)
lb_access_logs :: Functor f => (Maybe (AccessLogsParams) -> f (Maybe (AccessLogsParams))) -> AwsLbParams -> f AwsLbParams
lb_access_logs k atom = fmap (\newlb_access_logs -> atom { _lb_access_logs = newlb_access_logs }) (k (_lb_access_logs atom))
-- lb_subnets :: Lens' AwsLbParams [TFRef (AwsId AwsSubnet)]
lb_subnets :: Functor f => ([TFRef (AwsId AwsSubnet)] -> f ([TFRef (AwsId AwsSubnet)])) -> AwsLbParams -> f AwsLbParams
lb_subnets k atom = fmap (\newlb_subnets -> atom { _lb_subnets = newlb_subnets }) (k (_lb_subnets atom))
-- lb_subnet_mapping :: Lens' AwsLbParams Maybe (SubnetMappingParams)
lb_subnet_mapping :: Functor f => (Maybe (SubnetMappingParams) -> f (Maybe (SubnetMappingParams))) -> AwsLbParams -> f AwsLbParams
lb_subnet_mapping k atom = fmap (\newlb_subnet_mapping -> atom { _lb_subnet_mapping = newlb_subnet_mapping }) (k (_lb_subnet_mapping atom))
-- lb_idle_timeout :: Lens' AwsLbParams Int
lb_idle_timeout :: Functor f => (Int -> f (Int)) -> AwsLbParams -> f AwsLbParams
lb_idle_timeout k atom = fmap (\newlb_idle_timeout -> atom { _lb_idle_timeout = newlb_idle_timeout }) (k (_lb_idle_timeout atom))
-- lb_enable_deletion_protection :: Lens' AwsLbParams Bool
lb_enable_deletion_protection :: Functor f => (Bool -> f (Bool)) -> AwsLbParams -> f AwsLbParams
lb_enable_deletion_protection k atom = fmap (\newlb_enable_deletion_protection -> atom { _lb_enable_deletion_protection = newlb_enable_deletion_protection }) (k (_lb_enable_deletion_protection atom))
-- lb_enable_cross_zone_load_balancing :: Lens' AwsLbParams Bool
lb_enable_cross_zone_load_balancing :: Functor f => (Bool -> f (Bool)) -> AwsLbParams -> f AwsLbParams
lb_enable_cross_zone_load_balancing k atom = fmap (\newlb_enable_cross_zone_load_balancing -> atom { _lb_enable_cross_zone_load_balancing = newlb_enable_cross_zone_load_balancing }) (k (_lb_enable_cross_zone_load_balancing atom))
-- lb_enable_http2 :: Lens' AwsLbParams Bool
lb_enable_http2 :: Functor f => (Bool -> f (Bool)) -> AwsLbParams -> f AwsLbParams
lb_enable_http2 k atom = fmap (\newlb_enable_http2 -> atom { _lb_enable_http2 = newlb_enable_http2 }) (k (_lb_enable_http2 atom))
-- lb_ip_address_type :: Lens' AwsLbParams Maybe (T.Text)
lb_ip_address_type :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsLbParams -> f AwsLbParams
lb_ip_address_type k atom = fmap (\newlb_ip_address_type -> atom { _lb_ip_address_type = newlb_ip_address_type }) (k (_lb_ip_address_type atom))
-- lb_tags :: Lens' AwsLbParams M.Map T.Text T.Text
lb_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsLbParams -> f AwsLbParams
lb_tags k atom = fmap (\newlb_tags -> atom { _lb_tags = newlb_tags }) (k (_lb_tags atom))

makeAwsLbParams ::  AwsLbParams
makeAwsLbParams  = AwsLbParams
  { _lb_name = Nothing
  , _lb_name_prefix = Nothing
  , _lb_internal = False
  , _lb_load_balancer_type = LB_application
  , _lb_security_groups = []
  , _lb_access_logs = Nothing
  , _lb_subnets = []
  , _lb_subnet_mapping = Nothing
  , _lb_idle_timeout = 60
  , _lb_enable_deletion_protection = False
  , _lb_enable_cross_zone_load_balancing = False
  , _lb_enable_http2 = True
  , _lb_ip_address_type = Nothing
  , _lb_tags = M.empty
  }

instance ToResourceFieldMap AwsLbParams where
  toResourceFieldMap params
    =  rfmOptionalField "name" (_lb_name params)
    <> rfmOptionalField "name_prefix" (_lb_name_prefix params)
    <> rfmOptionalDefField "internal" False (_lb_internal params)
    <> rfmOptionalDefField "load_balancer_type" LB_application (_lb_load_balancer_type params)
    <> rfmOptionalDefField "security_groups" [] (_lb_security_groups params)
    <> rfmOptionalField "access_logs" (_lb_access_logs params)
    <> rfmOptionalDefField "subnets" [] (_lb_subnets params)
    <> rfmOptionalField "subnet_mapping" (_lb_subnet_mapping params)
    <> rfmOptionalDefField "idle_timeout" 60 (_lb_idle_timeout params)
    <> rfmOptionalDefField "enable_deletion_protection" False (_lb_enable_deletion_protection params)
    <> rfmOptionalDefField "enable_cross_zone_load_balancing" False (_lb_enable_cross_zone_load_balancing params)
    <> rfmOptionalDefField "enable_http2" True (_lb_enable_http2 params)
    <> rfmOptionalField "ip_address_type" (_lb_ip_address_type params)
    <> rfmOptionalDefField "tags" M.empty (_lb_tags params)
    

instance ToResourceField AwsLbParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data SubnetMappingParams = SubnetMappingParams
  { _sn_subnet_id :: TFRef (AwsId AwsSubnet)
  , _sn_allocation_id :: TFRef (AwsId AwsEip)
  }
  deriving (Eq)

-- sn_subnet_id :: Lens' SubnetMappingParams TFRef (AwsId AwsSubnet)
sn_subnet_id :: Functor f => (TFRef (AwsId AwsSubnet) -> f (TFRef (AwsId AwsSubnet))) -> SubnetMappingParams -> f SubnetMappingParams
sn_subnet_id k atom = fmap (\newsn_subnet_id -> atom { _sn_subnet_id = newsn_subnet_id }) (k (_sn_subnet_id atom))
-- sn_allocation_id :: Lens' SubnetMappingParams TFRef (AwsId AwsEip)
sn_allocation_id :: Functor f => (TFRef (AwsId AwsEip) -> f (TFRef (AwsId AwsEip))) -> SubnetMappingParams -> f SubnetMappingParams
sn_allocation_id k atom = fmap (\newsn_allocation_id -> atom { _sn_allocation_id = newsn_allocation_id }) (k (_sn_allocation_id atom))

makeSubnetMappingParams :: TFRef (AwsId AwsSubnet) -> TFRef (AwsId AwsEip) -> SubnetMappingParams
makeSubnetMappingParams subnetId allocationId = SubnetMappingParams
  { _sn_subnet_id = subnetId
  , _sn_allocation_id = allocationId
  }

instance ToResourceFieldMap SubnetMappingParams where
  toResourceFieldMap params
    =  rfmField "subnet_id" (_sn_subnet_id params)
    <> rfmField "allocation_id" (_sn_allocation_id params)
    

instance ToResourceField SubnetMappingParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsLbListener to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/lb_listener.html aws_lb_listener> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'lbl_')

awsLbListener :: NameElement -> AwsArn AwsLb -> Int -> ListenerActionParams ->(AwsLbListenerParams -> AwsLbListenerParams) -> TF AwsLbListener
awsLbListener name0 loadBalancerArn port' defaultAction modf = newAwsLbListener name0 (modf (makeAwsLbListenerParams loadBalancerArn port' defaultAction))

awsLbListener' :: NameElement -> AwsArn AwsLb -> Int -> ListenerActionParams -> TF AwsLbListener
awsLbListener' name0 loadBalancerArn port' defaultAction = newAwsLbListener name0 (makeAwsLbListenerParams loadBalancerArn port' defaultAction)

newAwsLbListener :: NameElement -> AwsLbListenerParams -> TF AwsLbListener
newAwsLbListener name0 params = do
  rid <- mkResource "aws_lb_listener" name0 (toResourceFieldMap params)
  return AwsLbListener
    { lbl_id = resourceAttr rid "id"
    , lbl_arn = resourceAttr rid "arn"
    , lbl_resource = rid
    }

data AwsLbListener = AwsLbListener
  { lbl_id :: TFRef (AwsId AwsLbListener)
  , lbl_arn :: TFRef (AwsArn AwsLbListener)
  , lbl_resource :: ResourceId
  }

instance IsResource AwsLbListener where
  resourceId = lbl_resource

data AwsLbListenerParams = AwsLbListenerParams
  { _lbl_load_balancer_arn :: AwsArn AwsLb
  , _lbl_port' :: Int
  , _lbl_default_action :: ListenerActionParams
  , _lbl_protocol :: LoadBalancerProtocol
  , _lbl_ssl_policy :: Maybe (T.Text)
  , _lbl_certificate_arn :: Maybe (AwsArn AwsAcmCertificate)
  }

-- lbl_load_balancer_arn :: Lens' AwsLbListenerParams AwsArn AwsLb
lbl_load_balancer_arn :: Functor f => (AwsArn AwsLb -> f (AwsArn AwsLb)) -> AwsLbListenerParams -> f AwsLbListenerParams
lbl_load_balancer_arn k atom = fmap (\newlbl_load_balancer_arn -> atom { _lbl_load_balancer_arn = newlbl_load_balancer_arn }) (k (_lbl_load_balancer_arn atom))
-- lbl_port' :: Lens' AwsLbListenerParams Int
lbl_port' :: Functor f => (Int -> f (Int)) -> AwsLbListenerParams -> f AwsLbListenerParams
lbl_port' k atom = fmap (\newlbl_port' -> atom { _lbl_port' = newlbl_port' }) (k (_lbl_port' atom))
-- lbl_protocol :: Lens' AwsLbListenerParams LoadBalancerProtocol
lbl_protocol :: Functor f => (LoadBalancerProtocol -> f (LoadBalancerProtocol)) -> AwsLbListenerParams -> f AwsLbListenerParams
lbl_protocol k atom = fmap (\newlbl_protocol -> atom { _lbl_protocol = newlbl_protocol }) (k (_lbl_protocol atom))
-- lbl_ssl_policy :: Lens' AwsLbListenerParams Maybe (T.Text)
lbl_ssl_policy :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsLbListenerParams -> f AwsLbListenerParams
lbl_ssl_policy k atom = fmap (\newlbl_ssl_policy -> atom { _lbl_ssl_policy = newlbl_ssl_policy }) (k (_lbl_ssl_policy atom))
-- lbl_certificate_arn :: Lens' AwsLbListenerParams Maybe (AwsArn AwsAcmCertificate)
lbl_certificate_arn :: Functor f => (Maybe (AwsArn AwsAcmCertificate) -> f (Maybe (AwsArn AwsAcmCertificate))) -> AwsLbListenerParams -> f AwsLbListenerParams
lbl_certificate_arn k atom = fmap (\newlbl_certificate_arn -> atom { _lbl_certificate_arn = newlbl_certificate_arn }) (k (_lbl_certificate_arn atom))
-- lbl_default_action :: Lens' AwsLbListenerParams ListenerActionParams
lbl_default_action :: Functor f => (ListenerActionParams -> f (ListenerActionParams)) -> AwsLbListenerParams -> f AwsLbListenerParams
lbl_default_action k atom = fmap (\newlbl_default_action -> atom { _lbl_default_action = newlbl_default_action }) (k (_lbl_default_action atom))

makeAwsLbListenerParams :: AwsArn AwsLb -> Int -> ListenerActionParams -> AwsLbListenerParams
makeAwsLbListenerParams loadBalancerArn port' defaultAction = AwsLbListenerParams
  { _lbl_load_balancer_arn = loadBalancerArn
  , _lbl_port' = port'
  , _lbl_default_action = defaultAction
  , _lbl_protocol = LB_HTTP
  , _lbl_ssl_policy = Nothing
  , _lbl_certificate_arn = Nothing
  }

instance ToResourceFieldMap AwsLbListenerParams where
  toResourceFieldMap params
    =  rfmField "load_balancer_arn" (_lbl_load_balancer_arn params)
    <> rfmField "port" (_lbl_port' params)
    <> rfmOptionalDefField "protocol" LB_HTTP (_lbl_protocol params)
    <> rfmOptionalField "ssl_policy" (_lbl_ssl_policy params)
    <> rfmOptionalField "certificate_arn" (_lbl_certificate_arn params)
    <> rfmField "default_action" (_lbl_default_action params)
    

instance ToResourceField AwsLbListenerParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data LoadBalancerType = LB_application | LB_network deriving (Eq)

instance ToResourceField LoadBalancerType where
    toResourceField LB_application = "application"
    toResourceField LB_network = "network"

----------------------------------------------------------------------

data LoadBalancerProtocol = LB_TCP | LB_HTTP | LB_HTTPS deriving (Eq)

instance ToResourceField LoadBalancerProtocol where
    toResourceField LB_TCP = "TCP"
    toResourceField LB_HTTP = "HTTP"
    toResourceField LB_HTTPS = "HTTPS"

----------------------------------------------------------------------

data ListenerActionParams = ListenerActionParams
  { _la_target_group_arn :: AwsArn AwsLbTargetGroup
  , _la_type :: ListenerActionType
  }
  deriving (Eq)

-- la_target_group_arn :: Lens' ListenerActionParams AwsArn AwsLbTargetGroup
la_target_group_arn :: Functor f => (AwsArn AwsLbTargetGroup -> f (AwsArn AwsLbTargetGroup)) -> ListenerActionParams -> f ListenerActionParams
la_target_group_arn k atom = fmap (\newla_target_group_arn -> atom { _la_target_group_arn = newla_target_group_arn }) (k (_la_target_group_arn atom))
-- la_type :: Lens' ListenerActionParams ListenerActionType
la_type :: Functor f => (ListenerActionType -> f (ListenerActionType)) -> ListenerActionParams -> f ListenerActionParams
la_type k atom = fmap (\newla_type -> atom { _la_type = newla_type }) (k (_la_type atom))

makeListenerActionParams :: AwsArn AwsLbTargetGroup -> ListenerActionType -> ListenerActionParams
makeListenerActionParams targetGroupArn type_ = ListenerActionParams
  { _la_target_group_arn = targetGroupArn
  , _la_type = type_
  }

instance ToResourceFieldMap ListenerActionParams where
  toResourceFieldMap params
    =  rfmField "target_group_arn" (_la_target_group_arn params)
    <> rfmField "type" (_la_type params)
    

instance ToResourceField ListenerActionParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data ListenerActionType = LA_forward deriving (Eq)

instance ToResourceField ListenerActionType where
    toResourceField LA_forward = "forward"

----------------------------------------------------------------------

-- | Add a resource of type AwsLbTargetGroup to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/lb_target_group.html aws_lb_target_group> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'lbtg_')

awsLbTargetGroup :: NameElement -> Int -> LoadBalancerProtocol -> TFRef (AwsId AwsVpc) ->(AwsLbTargetGroupParams -> AwsLbTargetGroupParams) -> TF AwsLbTargetGroup
awsLbTargetGroup name0 port protocol vpcId modf = newAwsLbTargetGroup name0 (modf (makeAwsLbTargetGroupParams port protocol vpcId))

awsLbTargetGroup' :: NameElement -> Int -> LoadBalancerProtocol -> TFRef (AwsId AwsVpc) -> TF AwsLbTargetGroup
awsLbTargetGroup' name0 port protocol vpcId = newAwsLbTargetGroup name0 (makeAwsLbTargetGroupParams port protocol vpcId)

newAwsLbTargetGroup :: NameElement -> AwsLbTargetGroupParams -> TF AwsLbTargetGroup
newAwsLbTargetGroup name0 params = do
  rid <- mkResource "aws_lb_target_group" name0 (toResourceFieldMap params)
  return AwsLbTargetGroup
    { lbtg_id = resourceAttr rid "id"
    , lbtg_arn = resourceAttr rid "arn"
    , lbtg_name = resourceAttr rid "name"
    , lbtg_resource = rid
    }

data AwsLbTargetGroup = AwsLbTargetGroup
  { lbtg_id :: TFRef (AwsId AwsLbTargetGroup)
  , lbtg_arn :: TFRef (AwsArn AwsLbTargetGroup)
  , lbtg_name :: TFRef T.Text
  , lbtg_resource :: ResourceId
  }

instance IsResource AwsLbTargetGroup where
  resourceId = lbtg_resource

data AwsLbTargetGroupParams = AwsLbTargetGroupParams
  { _lbtg_port :: Int
  , _lbtg_protocol :: LoadBalancerProtocol
  , _lbtg_vpc_id :: TFRef (AwsId AwsVpc)
  , _lbtg_name' :: Maybe (T.Text)
  , _lbtg_name_prefix :: Maybe (T.Text)
  , _lbtg_deregistration_delay :: Int
  , _lbtg_slow_start :: Int
  , _lbtg_proxy_protocol_v2 :: Maybe (Bool)
  , _lbtg_stickiness :: Maybe (TargetGroupStickinessParams)
  , _lbtg_health_check :: Maybe (TargetGroupHealthCheckParams)
  , _lbtg_target_type :: TargetGroupTargetType
  , _lbtg_tags :: M.Map T.Text T.Text
  }

-- lbtg_name' :: Lens' AwsLbTargetGroupParams Maybe (T.Text)
lbtg_name' :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsLbTargetGroupParams -> f AwsLbTargetGroupParams
lbtg_name' k atom = fmap (\newlbtg_name' -> atom { _lbtg_name' = newlbtg_name' }) (k (_lbtg_name' atom))
-- lbtg_name_prefix :: Lens' AwsLbTargetGroupParams Maybe (T.Text)
lbtg_name_prefix :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> AwsLbTargetGroupParams -> f AwsLbTargetGroupParams
lbtg_name_prefix k atom = fmap (\newlbtg_name_prefix -> atom { _lbtg_name_prefix = newlbtg_name_prefix }) (k (_lbtg_name_prefix atom))
-- lbtg_port :: Lens' AwsLbTargetGroupParams Int
lbtg_port :: Functor f => (Int -> f (Int)) -> AwsLbTargetGroupParams -> f AwsLbTargetGroupParams
lbtg_port k atom = fmap (\newlbtg_port -> atom { _lbtg_port = newlbtg_port }) (k (_lbtg_port atom))
-- lbtg_protocol :: Lens' AwsLbTargetGroupParams LoadBalancerProtocol
lbtg_protocol :: Functor f => (LoadBalancerProtocol -> f (LoadBalancerProtocol)) -> AwsLbTargetGroupParams -> f AwsLbTargetGroupParams
lbtg_protocol k atom = fmap (\newlbtg_protocol -> atom { _lbtg_protocol = newlbtg_protocol }) (k (_lbtg_protocol atom))
-- lbtg_vpc_id :: Lens' AwsLbTargetGroupParams TFRef (AwsId AwsVpc)
lbtg_vpc_id :: Functor f => (TFRef (AwsId AwsVpc) -> f (TFRef (AwsId AwsVpc))) -> AwsLbTargetGroupParams -> f AwsLbTargetGroupParams
lbtg_vpc_id k atom = fmap (\newlbtg_vpc_id -> atom { _lbtg_vpc_id = newlbtg_vpc_id }) (k (_lbtg_vpc_id atom))
-- lbtg_deregistration_delay :: Lens' AwsLbTargetGroupParams Int
lbtg_deregistration_delay :: Functor f => (Int -> f (Int)) -> AwsLbTargetGroupParams -> f AwsLbTargetGroupParams
lbtg_deregistration_delay k atom = fmap (\newlbtg_deregistration_delay -> atom { _lbtg_deregistration_delay = newlbtg_deregistration_delay }) (k (_lbtg_deregistration_delay atom))
-- lbtg_slow_start :: Lens' AwsLbTargetGroupParams Int
lbtg_slow_start :: Functor f => (Int -> f (Int)) -> AwsLbTargetGroupParams -> f AwsLbTargetGroupParams
lbtg_slow_start k atom = fmap (\newlbtg_slow_start -> atom { _lbtg_slow_start = newlbtg_slow_start }) (k (_lbtg_slow_start atom))
-- lbtg_proxy_protocol_v2 :: Lens' AwsLbTargetGroupParams Maybe (Bool)
lbtg_proxy_protocol_v2 :: Functor f => (Maybe (Bool) -> f (Maybe (Bool))) -> AwsLbTargetGroupParams -> f AwsLbTargetGroupParams
lbtg_proxy_protocol_v2 k atom = fmap (\newlbtg_proxy_protocol_v2 -> atom { _lbtg_proxy_protocol_v2 = newlbtg_proxy_protocol_v2 }) (k (_lbtg_proxy_protocol_v2 atom))
-- lbtg_stickiness :: Lens' AwsLbTargetGroupParams Maybe (TargetGroupStickinessParams)
lbtg_stickiness :: Functor f => (Maybe (TargetGroupStickinessParams) -> f (Maybe (TargetGroupStickinessParams))) -> AwsLbTargetGroupParams -> f AwsLbTargetGroupParams
lbtg_stickiness k atom = fmap (\newlbtg_stickiness -> atom { _lbtg_stickiness = newlbtg_stickiness }) (k (_lbtg_stickiness atom))
-- lbtg_health_check :: Lens' AwsLbTargetGroupParams Maybe (TargetGroupHealthCheckParams)
lbtg_health_check :: Functor f => (Maybe (TargetGroupHealthCheckParams) -> f (Maybe (TargetGroupHealthCheckParams))) -> AwsLbTargetGroupParams -> f AwsLbTargetGroupParams
lbtg_health_check k atom = fmap (\newlbtg_health_check -> atom { _lbtg_health_check = newlbtg_health_check }) (k (_lbtg_health_check atom))
-- lbtg_target_type :: Lens' AwsLbTargetGroupParams TargetGroupTargetType
lbtg_target_type :: Functor f => (TargetGroupTargetType -> f (TargetGroupTargetType)) -> AwsLbTargetGroupParams -> f AwsLbTargetGroupParams
lbtg_target_type k atom = fmap (\newlbtg_target_type -> atom { _lbtg_target_type = newlbtg_target_type }) (k (_lbtg_target_type atom))
-- lbtg_tags :: Lens' AwsLbTargetGroupParams M.Map T.Text T.Text
lbtg_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsLbTargetGroupParams -> f AwsLbTargetGroupParams
lbtg_tags k atom = fmap (\newlbtg_tags -> atom { _lbtg_tags = newlbtg_tags }) (k (_lbtg_tags atom))

makeAwsLbTargetGroupParams :: Int -> LoadBalancerProtocol -> TFRef (AwsId AwsVpc) -> AwsLbTargetGroupParams
makeAwsLbTargetGroupParams port protocol vpcId = AwsLbTargetGroupParams
  { _lbtg_port = port
  , _lbtg_protocol = protocol
  , _lbtg_vpc_id = vpcId
  , _lbtg_name' = Nothing
  , _lbtg_name_prefix = Nothing
  , _lbtg_deregistration_delay = 300
  , _lbtg_slow_start = 0
  , _lbtg_proxy_protocol_v2 = Nothing
  , _lbtg_stickiness = Nothing
  , _lbtg_health_check = Nothing
  , _lbtg_target_type = TG_instance
  , _lbtg_tags = M.empty
  }

instance ToResourceFieldMap AwsLbTargetGroupParams where
  toResourceFieldMap params
    =  rfmOptionalField "name" (_lbtg_name' params)
    <> rfmOptionalField "name_prefix" (_lbtg_name_prefix params)
    <> rfmField "port" (_lbtg_port params)
    <> rfmField "protocol" (_lbtg_protocol params)
    <> rfmField "vpc_id" (_lbtg_vpc_id params)
    <> rfmOptionalDefField "deregistration_delay" 300 (_lbtg_deregistration_delay params)
    <> rfmOptionalDefField "slow_start" 0 (_lbtg_slow_start params)
    <> rfmOptionalField "proxy_protocol_v2" (_lbtg_proxy_protocol_v2 params)
    <> rfmOptionalField "stickiness" (_lbtg_stickiness params)
    <> rfmOptionalField "health_check" (_lbtg_health_check params)
    <> rfmOptionalDefField "target_type" TG_instance (_lbtg_target_type params)
    <> rfmOptionalDefField "tags" M.empty (_lbtg_tags params)
    

instance ToResourceField AwsLbTargetGroupParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data TargetGroupTargetType = TG_instance | TG_ip deriving (Eq)

instance ToResourceField TargetGroupTargetType where
    toResourceField TG_instance = "instance"
    toResourceField TG_ip = "ip"

----------------------------------------------------------------------

data TargetGroupStickinessParams = TargetGroupStickinessParams
  { _tgs_type :: TargetGroupStickinessType
  , _tgs_cookie_duration :: Int
  , _tgs_enabled :: Bool
  }
  deriving (Eq)

-- tgs_type :: Lens' TargetGroupStickinessParams TargetGroupStickinessType
tgs_type :: Functor f => (TargetGroupStickinessType -> f (TargetGroupStickinessType)) -> TargetGroupStickinessParams -> f TargetGroupStickinessParams
tgs_type k atom = fmap (\newtgs_type -> atom { _tgs_type = newtgs_type }) (k (_tgs_type atom))
-- tgs_cookie_duration :: Lens' TargetGroupStickinessParams Int
tgs_cookie_duration :: Functor f => (Int -> f (Int)) -> TargetGroupStickinessParams -> f TargetGroupStickinessParams
tgs_cookie_duration k atom = fmap (\newtgs_cookie_duration -> atom { _tgs_cookie_duration = newtgs_cookie_duration }) (k (_tgs_cookie_duration atom))
-- tgs_enabled :: Lens' TargetGroupStickinessParams Bool
tgs_enabled :: Functor f => (Bool -> f (Bool)) -> TargetGroupStickinessParams -> f TargetGroupStickinessParams
tgs_enabled k atom = fmap (\newtgs_enabled -> atom { _tgs_enabled = newtgs_enabled }) (k (_tgs_enabled atom))

makeTargetGroupStickinessParams :: TargetGroupStickinessType -> TargetGroupStickinessParams
makeTargetGroupStickinessParams type_ = TargetGroupStickinessParams
  { _tgs_type = type_
  , _tgs_cookie_duration = 86400
  , _tgs_enabled = True
  }

instance ToResourceFieldMap TargetGroupStickinessParams where
  toResourceFieldMap params
    =  rfmField "type" (_tgs_type params)
    <> rfmOptionalDefField "cookie_duration" 86400 (_tgs_cookie_duration params)
    <> rfmOptionalDefField "enabled" True (_tgs_enabled params)
    

instance ToResourceField TargetGroupStickinessParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data TargetGroupStickinessType = TG_lb_cookie deriving (Eq)

instance ToResourceField TargetGroupStickinessType where
    toResourceField TG_lb_cookie = "lb_cookie"

----------------------------------------------------------------------

data TargetGroupHealthCheckParams = TargetGroupHealthCheckParams
  { _tghc_interval :: Int
  , _tghc_path :: Maybe (T.Text)
  , _tghc_port :: T.Text
  , _tghc_protocol :: LoadBalancerProtocol
  , _tghc_timeout :: Int
  , _tghc_healthy_threshold :: Int
  , _tghc_unhealthy_threshold :: Int
  , _tghc_matcher :: Maybe (T.Text)
  }
  deriving (Eq)

-- tghc_interval :: Lens' TargetGroupHealthCheckParams Int
tghc_interval :: Functor f => (Int -> f (Int)) -> TargetGroupHealthCheckParams -> f TargetGroupHealthCheckParams
tghc_interval k atom = fmap (\newtghc_interval -> atom { _tghc_interval = newtghc_interval }) (k (_tghc_interval atom))
-- tghc_path :: Lens' TargetGroupHealthCheckParams Maybe (T.Text)
tghc_path :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> TargetGroupHealthCheckParams -> f TargetGroupHealthCheckParams
tghc_path k atom = fmap (\newtghc_path -> atom { _tghc_path = newtghc_path }) (k (_tghc_path atom))
-- tghc_port :: Lens' TargetGroupHealthCheckParams T.Text
tghc_port :: Functor f => (T.Text -> f (T.Text)) -> TargetGroupHealthCheckParams -> f TargetGroupHealthCheckParams
tghc_port k atom = fmap (\newtghc_port -> atom { _tghc_port = newtghc_port }) (k (_tghc_port atom))
-- tghc_protocol :: Lens' TargetGroupHealthCheckParams LoadBalancerProtocol
tghc_protocol :: Functor f => (LoadBalancerProtocol -> f (LoadBalancerProtocol)) -> TargetGroupHealthCheckParams -> f TargetGroupHealthCheckParams
tghc_protocol k atom = fmap (\newtghc_protocol -> atom { _tghc_protocol = newtghc_protocol }) (k (_tghc_protocol atom))
-- tghc_timeout :: Lens' TargetGroupHealthCheckParams Int
tghc_timeout :: Functor f => (Int -> f (Int)) -> TargetGroupHealthCheckParams -> f TargetGroupHealthCheckParams
tghc_timeout k atom = fmap (\newtghc_timeout -> atom { _tghc_timeout = newtghc_timeout }) (k (_tghc_timeout atom))
-- tghc_healthy_threshold :: Lens' TargetGroupHealthCheckParams Int
tghc_healthy_threshold :: Functor f => (Int -> f (Int)) -> TargetGroupHealthCheckParams -> f TargetGroupHealthCheckParams
tghc_healthy_threshold k atom = fmap (\newtghc_healthy_threshold -> atom { _tghc_healthy_threshold = newtghc_healthy_threshold }) (k (_tghc_healthy_threshold atom))
-- tghc_unhealthy_threshold :: Lens' TargetGroupHealthCheckParams Int
tghc_unhealthy_threshold :: Functor f => (Int -> f (Int)) -> TargetGroupHealthCheckParams -> f TargetGroupHealthCheckParams
tghc_unhealthy_threshold k atom = fmap (\newtghc_unhealthy_threshold -> atom { _tghc_unhealthy_threshold = newtghc_unhealthy_threshold }) (k (_tghc_unhealthy_threshold atom))
-- tghc_matcher :: Lens' TargetGroupHealthCheckParams Maybe (T.Text)
tghc_matcher :: Functor f => (Maybe (T.Text) -> f (Maybe (T.Text))) -> TargetGroupHealthCheckParams -> f TargetGroupHealthCheckParams
tghc_matcher k atom = fmap (\newtghc_matcher -> atom { _tghc_matcher = newtghc_matcher }) (k (_tghc_matcher atom))

makeTargetGroupHealthCheckParams ::  TargetGroupHealthCheckParams
makeTargetGroupHealthCheckParams  = TargetGroupHealthCheckParams
  { _tghc_interval = 30
  , _tghc_path = Nothing
  , _tghc_port = "traffic-port"
  , _tghc_protocol = LB_HTTP
  , _tghc_timeout = 5
  , _tghc_healthy_threshold = 3
  , _tghc_unhealthy_threshold = 3
  , _tghc_matcher = Nothing
  }

instance ToResourceFieldMap TargetGroupHealthCheckParams where
  toResourceFieldMap params
    =  rfmOptionalDefField "interval" 30 (_tghc_interval params)
    <> rfmOptionalField "path" (_tghc_path params)
    <> rfmOptionalDefField "port" "traffic-port" (_tghc_port params)
    <> rfmOptionalDefField "protocol" LB_HTTP (_tghc_protocol params)
    <> rfmOptionalDefField "timeout" 5 (_tghc_timeout params)
    <> rfmOptionalDefField "healthy_threshold" 3 (_tghc_healthy_threshold params)
    <> rfmOptionalDefField "unhealthy_threshold" 3 (_tghc_unhealthy_threshold params)
    <> rfmOptionalField "matcher" (_tghc_matcher params)
    

instance ToResourceField TargetGroupHealthCheckParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsLbTargetGroupAttachment to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/lb_target_group_attachment.html aws_lb_target_group_attachment> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'lbtga_')

awsLbTargetGroupAttachment :: NameElement -> AwsArn AwsLbTargetGroup -> T.Text ->(AwsLbTargetGroupAttachmentParams -> AwsLbTargetGroupAttachmentParams) -> TF AwsLbTargetGroupAttachment
awsLbTargetGroupAttachment name0 targetGroupArn targetId modf = newAwsLbTargetGroupAttachment name0 (modf (makeAwsLbTargetGroupAttachmentParams targetGroupArn targetId))

awsLbTargetGroupAttachment' :: NameElement -> AwsArn AwsLbTargetGroup -> T.Text -> TF AwsLbTargetGroupAttachment
awsLbTargetGroupAttachment' name0 targetGroupArn targetId = newAwsLbTargetGroupAttachment name0 (makeAwsLbTargetGroupAttachmentParams targetGroupArn targetId)

newAwsLbTargetGroupAttachment :: NameElement -> AwsLbTargetGroupAttachmentParams -> TF AwsLbTargetGroupAttachment
newAwsLbTargetGroupAttachment name0 params = do
  rid <- mkResource "aws_lb_target_group_attachment" name0 (toResourceFieldMap params)
  return AwsLbTargetGroupAttachment
    { lbtga_id = resourceAttr rid "id"
    , lbtga_resource = rid
    }

data AwsLbTargetGroupAttachment = AwsLbTargetGroupAttachment
  { lbtga_id :: TFRef (AwsId AwsLbTargetGroupAttachment)
  , lbtga_resource :: ResourceId
  }

instance IsResource AwsLbTargetGroupAttachment where
  resourceId = lbtga_resource

data AwsLbTargetGroupAttachmentParams = AwsLbTargetGroupAttachmentParams
  { _lbtga_target_group_arn :: AwsArn AwsLbTargetGroup
  , _lbtga_target_id :: T.Text
  , _lbtga_port :: Maybe (Int)
  , _lbtga_availability_zone :: Maybe (AvailabilityZone)
  }

-- lbtga_target_group_arn :: Lens' AwsLbTargetGroupAttachmentParams AwsArn AwsLbTargetGroup
lbtga_target_group_arn :: Functor f => (AwsArn AwsLbTargetGroup -> f (AwsArn AwsLbTargetGroup)) -> AwsLbTargetGroupAttachmentParams -> f AwsLbTargetGroupAttachmentParams
lbtga_target_group_arn k atom = fmap (\newlbtga_target_group_arn -> atom { _lbtga_target_group_arn = newlbtga_target_group_arn }) (k (_lbtga_target_group_arn atom))
-- lbtga_target_id :: Lens' AwsLbTargetGroupAttachmentParams T.Text
lbtga_target_id :: Functor f => (T.Text -> f (T.Text)) -> AwsLbTargetGroupAttachmentParams -> f AwsLbTargetGroupAttachmentParams
lbtga_target_id k atom = fmap (\newlbtga_target_id -> atom { _lbtga_target_id = newlbtga_target_id }) (k (_lbtga_target_id atom))
-- lbtga_port :: Lens' AwsLbTargetGroupAttachmentParams Maybe (Int)
lbtga_port :: Functor f => (Maybe (Int) -> f (Maybe (Int))) -> AwsLbTargetGroupAttachmentParams -> f AwsLbTargetGroupAttachmentParams
lbtga_port k atom = fmap (\newlbtga_port -> atom { _lbtga_port = newlbtga_port }) (k (_lbtga_port atom))
-- lbtga_availability_zone :: Lens' AwsLbTargetGroupAttachmentParams Maybe (AvailabilityZone)
lbtga_availability_zone :: Functor f => (Maybe (AvailabilityZone) -> f (Maybe (AvailabilityZone))) -> AwsLbTargetGroupAttachmentParams -> f AwsLbTargetGroupAttachmentParams
lbtga_availability_zone k atom = fmap (\newlbtga_availability_zone -> atom { _lbtga_availability_zone = newlbtga_availability_zone }) (k (_lbtga_availability_zone atom))

makeAwsLbTargetGroupAttachmentParams :: AwsArn AwsLbTargetGroup -> T.Text -> AwsLbTargetGroupAttachmentParams
makeAwsLbTargetGroupAttachmentParams targetGroupArn targetId = AwsLbTargetGroupAttachmentParams
  { _lbtga_target_group_arn = targetGroupArn
  , _lbtga_target_id = targetId
  , _lbtga_port = Nothing
  , _lbtga_availability_zone = Nothing
  }

instance ToResourceFieldMap AwsLbTargetGroupAttachmentParams where
  toResourceFieldMap params
    =  rfmField "target_group_arn" (_lbtga_target_group_arn params)
    <> rfmField "target_id" (_lbtga_target_id params)
    <> rfmOptionalField "port" (_lbtga_port params)
    <> rfmOptionalField "availability_zone" (_lbtga_availability_zone params)
    

instance ToResourceField AwsLbTargetGroupAttachmentParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

-- | Add a resource of type AwsLbListenerRule to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/lb_listener_rule.html aws_lb_listener_rule> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'lblr_')

awsLbListenerRule :: NameElement -> AwsArn AwsLbListener -> ListenerActionParams -> ListenerConditionParams ->(AwsLbListenerRuleParams -> AwsLbListenerRuleParams) -> TF AwsLbListenerRule
awsLbListenerRule name0 listenerArn action condition modf = newAwsLbListenerRule name0 (modf (makeAwsLbListenerRuleParams listenerArn action condition))

awsLbListenerRule' :: NameElement -> AwsArn AwsLbListener -> ListenerActionParams -> ListenerConditionParams -> TF AwsLbListenerRule
awsLbListenerRule' name0 listenerArn action condition = newAwsLbListenerRule name0 (makeAwsLbListenerRuleParams listenerArn action condition)

newAwsLbListenerRule :: NameElement -> AwsLbListenerRuleParams -> TF AwsLbListenerRule
newAwsLbListenerRule name0 params = do
  rid <- mkResource "aws_lb_listener_rule" name0 (toResourceFieldMap params)
  return AwsLbListenerRule
    { lblr_id = resourceAttr rid "id"
    , lblr_arn = resourceAttr rid "arn"
    , lblr_resource = rid
    }

data AwsLbListenerRule = AwsLbListenerRule
  { lblr_id :: TFRef (AwsId AwsLbTargetGroupAttachment)
  , lblr_arn :: TFRef (AwsArn AwsLbListenerRule)
  , lblr_resource :: ResourceId
  }

instance IsResource AwsLbListenerRule where
  resourceId = lblr_resource

data AwsLbListenerRuleParams = AwsLbListenerRuleParams
  { _lblr_listener_arn :: AwsArn AwsLbListener
  , _lblr_action :: ListenerActionParams
  , _lblr_condition :: ListenerConditionParams
  , _lblr_priority :: Maybe (Int)
  }

-- lblr_listener_arn :: Lens' AwsLbListenerRuleParams AwsArn AwsLbListener
lblr_listener_arn :: Functor f => (AwsArn AwsLbListener -> f (AwsArn AwsLbListener)) -> AwsLbListenerRuleParams -> f AwsLbListenerRuleParams
lblr_listener_arn k atom = fmap (\newlblr_listener_arn -> atom { _lblr_listener_arn = newlblr_listener_arn }) (k (_lblr_listener_arn atom))
-- lblr_priority :: Lens' AwsLbListenerRuleParams Maybe (Int)
lblr_priority :: Functor f => (Maybe (Int) -> f (Maybe (Int))) -> AwsLbListenerRuleParams -> f AwsLbListenerRuleParams
lblr_priority k atom = fmap (\newlblr_priority -> atom { _lblr_priority = newlblr_priority }) (k (_lblr_priority atom))
-- lblr_action :: Lens' AwsLbListenerRuleParams ListenerActionParams
lblr_action :: Functor f => (ListenerActionParams -> f (ListenerActionParams)) -> AwsLbListenerRuleParams -> f AwsLbListenerRuleParams
lblr_action k atom = fmap (\newlblr_action -> atom { _lblr_action = newlblr_action }) (k (_lblr_action atom))
-- lblr_condition :: Lens' AwsLbListenerRuleParams ListenerConditionParams
lblr_condition :: Functor f => (ListenerConditionParams -> f (ListenerConditionParams)) -> AwsLbListenerRuleParams -> f AwsLbListenerRuleParams
lblr_condition k atom = fmap (\newlblr_condition -> atom { _lblr_condition = newlblr_condition }) (k (_lblr_condition atom))

makeAwsLbListenerRuleParams :: AwsArn AwsLbListener -> ListenerActionParams -> ListenerConditionParams -> AwsLbListenerRuleParams
makeAwsLbListenerRuleParams listenerArn action condition = AwsLbListenerRuleParams
  { _lblr_listener_arn = listenerArn
  , _lblr_action = action
  , _lblr_condition = condition
  , _lblr_priority = Nothing
  }

instance ToResourceFieldMap AwsLbListenerRuleParams where
  toResourceFieldMap params
    =  rfmField "listener_arn" (_lblr_listener_arn params)
    <> rfmOptionalField "priority" (_lblr_priority params)
    <> rfmField "action" (_lblr_action params)
    <> rfmField "condition" (_lblr_condition params)
    

instance ToResourceField AwsLbListenerRuleParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data ListenerConditionParams = ListenerConditionParams
  { _lblrc_field :: ListenerConditionField
  , _lblrc_values :: [T.Text]
  }
  deriving (Eq)

-- lblrc_field :: Lens' ListenerConditionParams ListenerConditionField
lblrc_field :: Functor f => (ListenerConditionField -> f (ListenerConditionField)) -> ListenerConditionParams -> f ListenerConditionParams
lblrc_field k atom = fmap (\newlblrc_field -> atom { _lblrc_field = newlblrc_field }) (k (_lblrc_field atom))
-- lblrc_values :: Lens' ListenerConditionParams [T.Text]
lblrc_values :: Functor f => ([T.Text] -> f ([T.Text])) -> ListenerConditionParams -> f ListenerConditionParams
lblrc_values k atom = fmap (\newlblrc_values -> atom { _lblrc_values = newlblrc_values }) (k (_lblrc_values atom))

makeListenerConditionParams :: ListenerConditionField -> [T.Text] -> ListenerConditionParams
makeListenerConditionParams field values = ListenerConditionParams
  { _lblrc_field = field
  , _lblrc_values = values
  }

instance ToResourceFieldMap ListenerConditionParams where
  toResourceFieldMap params
    =  rfmField "field" (_lblrc_field params)
    <> rfmField "values" (_lblrc_values params)
    

instance ToResourceField ListenerConditionParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data ListenerConditionField = LCF_path_pattern | LCF_host_header deriving (Eq)

instance ToResourceField ListenerConditionField where
    toResourceField LCF_path_pattern = "path-pattern"
    toResourceField LCF_host_header = "host-header"

----------------------------------------------------------------------

-- | Add a resource of type AwsAcmCertificate to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/d/acm_certificate.html aws_acm_certificate> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'ac_')

awsAcmCertificate :: NameElement -> T.Text -> CertValidationMethod ->(AwsAcmCertificateParams -> AwsAcmCertificateParams) -> TF AwsAcmCertificate
awsAcmCertificate name0 domainName validationMethod modf = newAwsAcmCertificate name0 (modf (makeAwsAcmCertificateParams domainName validationMethod))

awsAcmCertificate' :: NameElement -> T.Text -> CertValidationMethod -> TF AwsAcmCertificate
awsAcmCertificate' name0 domainName validationMethod = newAwsAcmCertificate name0 (makeAwsAcmCertificateParams domainName validationMethod)

newAwsAcmCertificate :: NameElement -> AwsAcmCertificateParams -> TF AwsAcmCertificate
newAwsAcmCertificate name0 params = do
  rid <- mkResource "aws_acm_certificate" name0 (toResourceFieldMap params)
  return AwsAcmCertificate
    { ac_id = resourceAttr rid "id"
    , ac_arn = resourceAttr rid "arn"
    , ac_resource = rid
    }

data AwsAcmCertificate = AwsAcmCertificate
  { ac_id :: TFRef (AwsId AwsAcmCertificate)
  , ac_arn :: TFRef (AwsArn AwsAcmCertificate)
  , ac_resource :: ResourceId
  }

instance IsResource AwsAcmCertificate where
  resourceId = ac_resource

data AwsAcmCertificateParams = AwsAcmCertificateParams
  { _ac_domain_name :: T.Text
  , _ac_validation_method :: CertValidationMethod
  , _ac_subject_alternative_names :: [T.Text]
  , _ac_tags :: M.Map T.Text T.Text
  }

-- ac_domain_name :: Lens' AwsAcmCertificateParams T.Text
ac_domain_name :: Functor f => (T.Text -> f (T.Text)) -> AwsAcmCertificateParams -> f AwsAcmCertificateParams
ac_domain_name k atom = fmap (\newac_domain_name -> atom { _ac_domain_name = newac_domain_name }) (k (_ac_domain_name atom))
-- ac_subject_alternative_names :: Lens' AwsAcmCertificateParams [T.Text]
ac_subject_alternative_names :: Functor f => ([T.Text] -> f ([T.Text])) -> AwsAcmCertificateParams -> f AwsAcmCertificateParams
ac_subject_alternative_names k atom = fmap (\newac_subject_alternative_names -> atom { _ac_subject_alternative_names = newac_subject_alternative_names }) (k (_ac_subject_alternative_names atom))
-- ac_validation_method :: Lens' AwsAcmCertificateParams CertValidationMethod
ac_validation_method :: Functor f => (CertValidationMethod -> f (CertValidationMethod)) -> AwsAcmCertificateParams -> f AwsAcmCertificateParams
ac_validation_method k atom = fmap (\newac_validation_method -> atom { _ac_validation_method = newac_validation_method }) (k (_ac_validation_method atom))
-- ac_tags :: Lens' AwsAcmCertificateParams M.Map T.Text T.Text
ac_tags :: Functor f => (M.Map T.Text T.Text -> f (M.Map T.Text T.Text)) -> AwsAcmCertificateParams -> f AwsAcmCertificateParams
ac_tags k atom = fmap (\newac_tags -> atom { _ac_tags = newac_tags }) (k (_ac_tags atom))

makeAwsAcmCertificateParams :: T.Text -> CertValidationMethod -> AwsAcmCertificateParams
makeAwsAcmCertificateParams domainName validationMethod = AwsAcmCertificateParams
  { _ac_domain_name = domainName
  , _ac_validation_method = validationMethod
  , _ac_subject_alternative_names = []
  , _ac_tags = M.empty
  }

instance ToResourceFieldMap AwsAcmCertificateParams where
  toResourceFieldMap params
    =  rfmField "domain_name" (_ac_domain_name params)
    <> rfmOptionalDefField "subject_alternative_names" [] (_ac_subject_alternative_names params)
    <> rfmField "validation_method" (_ac_validation_method params)
    <> rfmOptionalDefField "tags" M.empty (_ac_tags params)
    

instance ToResourceField AwsAcmCertificateParams where
  toResourceField = RF_Map . toResourceFieldMap 

----------------------------------------------------------------------

data CertValidationMethod = CVM_DNS | CVM_EMAIL | CVM_NONE deriving (Eq)

instance ToResourceField CertValidationMethod where
    toResourceField CVM_DNS = "DNS"
    toResourceField CVM_EMAIL = "EMAIL"
    toResourceField CVM_NONE = "NONE"

----------------------------------------------------------------------

-- | Add a resource of type AwsAcmCertificateValidation to the resource graph.
--
-- See the terraform <https://www.terraform.io/docs/providers/aws/r/acm_certificate_validation.html aws_acm_certificate_validation> documentation
-- for details.
-- (In this binding attribute and argument names all have the prefix 'acv_')

awsAcmCertificateValidation :: NameElement -> AwsArn AwsAcmCertificate ->(AwsAcmCertificateValidationParams -> AwsAcmCertificateValidationParams) -> TF AwsAcmCertificateValidation
awsAcmCertificateValidation name0 certificateArn modf = newAwsAcmCertificateValidation name0 (modf (makeAwsAcmCertificateValidationParams certificateArn))

awsAcmCertificateValidation' :: NameElement -> AwsArn AwsAcmCertificate -> TF AwsAcmCertificateValidation
awsAcmCertificateValidation' name0 certificateArn = newAwsAcmCertificateValidation name0 (makeAwsAcmCertificateValidationParams certificateArn)

newAwsAcmCertificateValidation :: NameElement -> AwsAcmCertificateValidationParams -> TF AwsAcmCertificateValidation
newAwsAcmCertificateValidation name0 params = do
  rid <- mkResource "aws_acm_certificate_validation" name0 (toResourceFieldMap params)
  return AwsAcmCertificateValidation
    { acv_resource = rid
    }

data AwsAcmCertificateValidation = AwsAcmCertificateValidation
  { acv_resource :: ResourceId
  }

instance IsResource AwsAcmCertificateValidation where
  resourceId = acv_resource

data AwsAcmCertificateValidationParams = AwsAcmCertificateValidationParams
  { _acv_certificate_arn :: AwsArn AwsAcmCertificate
  , _acv_validation_record_fqdns :: [T.Text]
  }

-- acv_certificate_arn :: Lens' AwsAcmCertificateValidationParams AwsArn AwsAcmCertificate
acv_certificate_arn :: Functor f => (AwsArn AwsAcmCertificate -> f (AwsArn AwsAcmCertificate)) -> AwsAcmCertificateValidationParams -> f AwsAcmCertificateValidationParams
acv_certificate_arn k atom = fmap (\newacv_certificate_arn -> atom { _acv_certificate_arn = newacv_certificate_arn }) (k (_acv_certificate_arn atom))
-- acv_validation_record_fqdns :: Lens' AwsAcmCertificateValidationParams [T.Text]
acv_validation_record_fqdns :: Functor f => ([T.Text] -> f ([T.Text])) -> AwsAcmCertificateValidationParams -> f AwsAcmCertificateValidationParams
acv_validation_record_fqdns k atom = fmap (\newacv_validation_record_fqdns -> atom { _acv_validation_record_fqdns = newacv_validation_record_fqdns }) (k (_acv_validation_record_fqdns atom))

makeAwsAcmCertificateValidationParams :: AwsArn AwsAcmCertificate -> AwsAcmCertificateValidationParams
makeAwsAcmCertificateValidationParams certificateArn = AwsAcmCertificateValidationParams
  { _acv_certificate_arn = certificateArn
  , _acv_validation_record_fqdns = []
  }

instance ToResourceFieldMap AwsAcmCertificateValidationParams where
  toResourceFieldMap params
    =  rfmField "certificate_arn" (_acv_certificate_arn params)
    <> rfmOptionalDefField "validation_record_fqdns" [] (_acv_validation_record_fqdns params)
    

instance ToResourceField AwsAcmCertificateValidationParams where
  toResourceField = RF_Map . toResourceFieldMap 