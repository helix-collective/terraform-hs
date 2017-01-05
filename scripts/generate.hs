{-# LANGUAGE OverloadedStrings #-}

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
    , ""
    , "-- | Add an aws provider to the resource graph."
    , "--"
    , "-- See https://www.terraform.io/docs/providers/aws/index.html"
    , "-- in the terraform documentation for descriptions of the arguments."
    , ""
    , "aws :: AwsParams -> TF ()"
    , "aws params ="
    , "  mkProvider \"aws\" $ catMaybes"
    , "    [ Just (\"region\", toResourceField (aws_region params))"
    , "    , let v = aws_access_key (aws_options params) in if v == \"\" then Nothing else (Just (\"access_key\", toResourceField v))"
    , "    , let v = aws_secret_key (aws_options params) in if v == \"\" then Nothing else (Just (\"secret_key\", toResourceField v))"
    , "    ]"
    , ""
    , "data AwsParams = AwsParams"
    , "  { aws_region :: AwsRegion"
    , "  , aws_options :: AwsOptions"
    , "  }"
    , ""
    , "data AwsOptions = AwsOptions"
    , "  { aws_access_key :: T.Text"
    , "  , aws_secret_key :: T.Text"
    , "  }"
    , ""
    , "instance Default AwsOptions where"
    , "  def = AwsOptions \"\" \"\""
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
    [ ("availability_zone", NamedType "AvailabilityZone", OptionalWithDefault "\"\"")
    , ("cidr_block", NamedType "CidrBlock", Required)
    , ("map_public_ip_on_launch", NamedType "Bool", OptionalWithDefault "False")
    , ("vpc_id", AwsIdRef "aws_vpc", Required)
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
    , ("instance_type", NamedType "InstanceType", Required)
    , ("key_name", NamedType "KeyName", Optional)
    , ("subnet_id", AwsIdRef "aws_subnet", Optional)
    , ("root_block_device", NamedType "RootBlockDeviceParams", Optional)
    , ("user_data", NamedType "T.Text", OptionalWithDefault "\"\"")
    , ("vpc_security_group_ids", FTList (AwsIdRef "aws_security_group"), OptionalWithDefault "[]")
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    ]
    [ ("id", AwsIdRef "aws_instance")
    , ("public_ip", TFRef "IpAddress")
    , ("private_ip", TFRef "IpAddress")
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
    , ("lb_protocol", NamedType "Int", Required)
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
    [ ("name", NamedType "T.Text", Optional)
    , ("access_logs", NamedType "AccessLogsParams", Optional)
    , ("security_groups", FTList (AwsIdRef "aws_security_group"), OptionalWithDefault "[]")
    , ("subnets", FTList (AwsIdRef "aws_subnet"), OptionalWithDefault "[]")
    , ("instances", FTList (AwsIdRef "aws_instance"), OptionalWithDefault "[]")
    , ("listener", FTList (NamedType "ListenerParams"), Required)
    , ("health_check", NamedType "HealthCheckParams", Optional)
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    ]
    [ ("id", TFRef "T.Text")
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

  , resourceCode "aws_s3_bucket" "s3"
    "https://www.terraform.io/docs/providers/aws/r/s3_bucket.html"
    [ ("bucket", NamedType "T.Text", Required)
    , ("acl", NamedType "CannedAcl", OptionalWithDefault "\"private\"")
    , ("tags", TagsMap, OptionalWithDefault "M.empty")
    , ("versioning", NamedType "BucketVersioningParams", Optional)
    , ("lifecycle_rule", NamedType "LifecycleRuleParams", Optional)
    ]
    [ ("id", TFRef "S3BucketName")
    ]

  , resourceCode "aws_s3_bucket_object" "s3o"
    "https://www.terraform.io/docs/providers/aws/d/s3_bucket_object.html"
    [ ("bucket", AwsIdRef "aws_s3_bucket", Required)
    , ("key", NamedType "S3Key", Required)
    , ("source", NamedType" FilePath", Optional)
    , ("content", NamedType" T.Text", Optional)
    ]
    [ ("id", TFRef "T.Text")
    , ("etag", TFRef "T.Text")
    , ("version_id", TFRef "T.Text")
    ]
  ]

data FieldType = NamedType T.Text | TFRef T.Text | AwsIdRef T.Text | FTList FieldType | TagsMap
data FieldMode = Required | Optional | OptionalWithDefault T.Text

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


fieldsCode :: T.Text -> T.Text -> Bool -> [(T.Text, FieldType, FieldMode)] -> Code
fieldsCode htypename fieldprefix deriveInstances args
  =  mconcat (intersperse cblank [params,options,defaultInstance,toResourceInstance])
  where
    params =
      (  ctemplate "data $1Params = $1Params" [htypename]
      <> CIndent (cgroup "{ " ", " "}"
          ( [T.template "$1 :: $2" [hname fname,hftype ftype] | (fname,ftype,Required) <- args]
            <>
            [T.template "$1_options :: $2Options" [fieldprefix,htypename]]
          )
          <> if deriveInstances then cline "deriving (Eq)" else mempty
         )
      )
    options =
      (  ctemplate "data $1Options = $1Options" [htypename]
      <> CIndent
         (cgroup "{ " ", " "}" [ T.template "$1 :: $2" [hname fname,optionalType ftype fmode]
                               | (fname,ftype,fmode) <- args,  isOptional fmode]
          <> if deriveInstances then cline "deriving (Eq)" else mempty
         )
      )

    toResourceInstance
      =  (ctemplate "instance ToResourceFieldMap $1Params where" [htypename])
      <> CIndent
         (cline "toResourceFieldMap params = M.fromList $ catMaybes"
         <> (CIndent (cgroup "[ " ", " "]"  (map createValue args)))
         )
      <> cblank
      <> (ctemplate "instance ToResourceField $1Params where" [htypename])
      <> CIndent
         (cline "toResourceField = RF_Map . toResourceFieldMap "
         )
        
    createValue (fname,ftype,Required) =
      T.template "Just $1" [fieldValue fname (T.template "($1 params)" [hname fname])]
    createValue (fname,ftype,Optional) =
      T.template "fmap (\\v-> $1) ($2 ($3_options params))" [fieldValue fname "v",hname fname,fieldprefix]
    createValue (fname,ftype,OptionalWithDefault defv) =
      T.template "let v = $1 ($2_options params) in if v == $3 then Nothing else (Just $4)"[hname fname,fieldprefix,defv,fieldValue fname "v"]

    fieldValue fname v = T.template "(\"$1\", toResourceField $2)" [fname,v]

    defaultInstance =
      ctemplate "instance Default $1Options where" [htypename]
      <> CIndent (ctemplate "def = $1Options $2" [htypename,T.intercalate " " [optionalDefault fmode | (_,_,fmode) <- args, isOptional fmode]])

    hname n = fieldprefix <> "_" <> n
    
resourceCode :: T.Text -> T.Text -> T.Text -> [(T.Text, FieldType, FieldMode)] -> [(T.Text, FieldType)] -> Code
resourceCode tftypename fieldprefix docurl args attrs
  =  mconcat (intersperse cblank [function,function',argsTypes,value,isResourceInstance])
  where
    function
      =  ctemplate "-- | Add a resource of type $1 to the resource graph." [htypename tftypename]
      <> cline     "--"
      <> ctemplate "-- See $1" [docurl]
      <> cline     "-- in the terraform documentation for descriptions of the arguments and attributes."
      <> ctemplate "-- (Note that attribute and argument names all have the prefix '$1_')" [fieldprefix]
      <> cline     ""
      <> ctemplate
           "$1 :: NameElement -> $2 $3Options -> TF $3"
           [ hfnname tftypename
           , T.intercalate " " [hftype ftype <> " ->" | (_,ftype,Required) <- args]
           , htypename tftypename
           ]
      <> ctemplate
           "$1 name0 $2 opts = $1' name0 ($3Params $2 opts)"
           [ hfnname tftypename
           , T.intercalate " " [hfnname fname | (fname,_,Required) <- args]
           , htypename tftypename
           ]

    function'
      =  ctemplate "$1' :: NameElement -> $2Params -> TF $2" [hfnname tftypename, htypename tftypename]
      <> ctemplate "$1' name0 params = do" [hfnname tftypename]
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

    hfnname tftype = let (c1,cs) = T.splitAt 1 (htypename tftype) in T.toLower c1 <> cs

    hname n = fieldprefix <> "_" <> n

htypename tftype = T.concat (map T.toTitle (T.splitOn "_" tftype))

isOptional Optional = True
isOptional (OptionalWithDefault _)  = True
isOptional _ = False

optionalType ftype Optional = T.template "Maybe ($1)" [hftype ftype]
optionalType ftype (OptionalWithDefault _) = hftype ftype

optionalDefault  Required = "??"
optionalDefault  Optional = "Nothing"
optionalDefault (OptionalWithDefault def) = def

hftype (NamedType t) = t
hftype (TFRef t) = T.template "TFRef $1" [t]
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
      , T.template "module Language.Terraform.$1 where" [moduleName]
      , ""
      , "import qualified Data.Map as M"
      , "import qualified Data.Text as T"
      , ""
      , "import Data.Default "
      , "import Data.Maybe(catMaybes)"
      , "import Data.Monoid"
      , "import Language.Terraform.Core"
      ]

generate :: FilePath -> IO ()
generate outdir = do
  generateModule outdir "Aws" awsHeader awsResources
  

main :: IO ()
main = generate "src/Language/Terraform"
