{-# LANGUAGE OverloadedStrings,FlexibleInstances #-}
-- | An EDSL for generating terraform code.
--
-- The basic idea is that you compose an monadic `TF` action to specify
-- the terraform resources and their dependencies, and then call `generateFiles`
-- to actually generate the terraform files. An example:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- import Language.Terraform.Core
-- import Language.Terraform.Aws
-- import Data.Default
-- 
-- main = generateFiles "/tmp" $ do
--    -- Construct an AWS virtual private cloud
--    vpc <- awsVpc "vpc" "10.30.0.0/16" def
--    -- Construct a route table
--    awsRouteTable "rt" (vpc_id vpc) def
--    return ()
-- @
--
-- In terraform, each resource requires a unique name. The name is
-- provided as the first parameter to the function creating that
-- resource. The `withNameScope` function helps composability by creating
-- independent scopes for names.
--
-- For more details on terraform, see
-- https://www.terraform.io/intro/index.html

module Language.Terraform.Core(
  ToResourceField(..),
  ToResourceFieldMap(..),
  IsResource(..),
  
  ResourceField(..),
  TF,
  TFRef(..),
  NameElement,
  Name,
  ResourceId,

  mkProvider,
  mkResource,
  output,
  resourceAttr,
  dependsOn,
  withNameScope,
  scopedName,
  generateFiles,
  ) where

import Data.Monoid
import Control.Monad(void)
import Control.Monad.Trans.State.Lazy(StateT,get,put,modify',runStateT)
import System.FilePath((</>))
import Data.Foldable(for_)
import Data.String(IsString(..))

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Terraform.Util.Text as T

type NameElement = T.Text

-- | Every terraform resource has a unique name that is
-- generated from it's name scope and the name provided
-- to the resource constructor function.
type Name = [NameElement]

type TFType = T.Text

data Provider = Provider {
  p_type :: T.Text,
  p_name :: [NameElement],
  p_fields :: ResourceFieldMap
}
                
data Resource = Resource {
  r_type :: T.Text,
  r_name :: [NameElement],
  r_fields :: ResourceFieldMap
}

data Output  = Output {
  o_name :: [NameElement],
  o_value :: T.Text
}

-- | A map to be embedded in the terraform output.
type ResourceFieldMap = M.Map T.Text ResourceField

-- | A value to be embedded in the terraform output.
data ResourceField = RF_Text T.Text
                   | RF_List [ResourceField]
                   | RF_Map ResourceFieldMap

-- | Typeclass for overloading the conversion of values to
-- a `ResourceField` value.
class ToResourceField a where
  toResourceField :: a -> ResourceField
  toResourceFieldList ::  [a] -> ResourceField
  toResourceFieldList as = RF_List (map toResourceField as)

-- | Typeclass for overloading the conversion of values to
-- a `ResourceFieldMap` value.
class ToResourceFieldMap a where
  toResourceFieldMap :: a -> ResourceFieldMap

instance IsString ResourceField where
  fromString = RF_Text . T.pack

-- | A unique identifier for a source.
data ResourceId = ResourceId TFType Name
 deriving (Eq,Ord)

-- | All terraform resources implement this class to support
-- overloaded access to common features.
class IsResource a where
  resourceId :: a -> ResourceId

-- | `TFRef t` is a reference to a terraform derived value of type t.
-- Such values typically depend on actually deploying the infrastructure
-- before they become known.
newtype TFRef t = TFRef {
  tfRefText :: T.Text
} deriving (Eq)

instance ToResourceField (TFRef t) where
  toResourceField (TFRef t) = RF_Text t

instance ToResourceField Int where
  toResourceField v = RF_Text (T.pack (show v))

instance ToResourceField T.Text where
  toResourceField t = RF_Text t

instance ToResourceField Char where
  toResourceField c = RF_Text (T.singleton c)

instance ToResourceField Bool where
  toResourceField True = RF_Text "true"
  toResourceField False = RF_Text "false"

instance ToResourceField a => ToResourceField [a] where
  toResourceField = RF_List . map toResourceField

instance ToResourceField a => ToResourceField (M.Map T.Text a) where
  toResourceField = RF_Map . M.map toResourceField

data TFState = TFState {
  tf_context :: [NameElement],
  tf_providers :: [Provider],
  tf_resources :: [Resource],
  tf_outputs :: [Output],
  tf_dependencies :: S.Set (ResourceId,ResourceId)
  }

-- | A state monad over IO that accumulates the
-- terraform resource graph.
type TF a = StateT TFState IO a

nameText :: Name -> T.Text
nameText nameElements = T.intercalate "_" (reverse nameElements)

-- | Generate a global name based upon the the current scope.
scopedName :: NameElement -> TF T.Text
scopedName name0 = do
  context <- tf_context <$> get
  return (nameText (name0:context))

-- | Provide a more specific naming scope for the specified terraform
-- action.  
withNameScope:: NameElement -> TF a -> TF a
withNameScope name tfa = do
  s0 <- get
  put s0{tf_context=name:tf_context s0}
  a <- tfa
  s1 <- get
  put s1{tf_context=tf_context s0}
  return a

-- | Internal function for constructing terraform providers
mkProvider :: TFType -> [(T.Text,ResourceField)] -> TF ()
mkProvider tftype fields  = do
  name <- fmap tf_context get
  let provider = Provider tftype name (M.fromList fields)
  modify' (\s -> s{tf_providers=provider:tf_providers s})

-- | Internal function for constructing terraform resources
mkResource :: TFType -> NameElement -> ResourceFieldMap -> TF ResourceId 
mkResource tftype name0 fieldmap  = do
  s <- get
  let name = name0:tf_context s
  let resource = Resource tftype name fieldmap
  modify' (\s -> s{tf_resources=resource:tf_resources s})
  return (ResourceId tftype name)

-- | Internal function for constructing resource attributes
resourceAttr :: ResourceId -> T.Text -> TFRef a
resourceAttr (ResourceId tftype name) attr = TFRef (T.template "${$1.$2.$3}" [tftype, nameText name, attr])

-- | Add an output to the generated terraform.
-- (See https://www.terraform.io/intro/getting-started/outputs.html)
output :: NameElement -> T.Text -> TF ()
output name0 value = do
  s <- get
  let name = name0:tf_context s
  let output = Output name value
  modify' (\s -> s{tf_outputs=output:tf_outputs s})

-- | Specifiy an explicit depedency betweeen resources.
-- (See https://www.terraform.io/intro/getting-started/dependencies.html)
dependsOn :: (IsResource r1,IsResource r2) => r1 -> r2 -> TF ()
dependsOn r1 r2 = modify' (\s->s{tf_dependencies=S.insert (resourceId r1, resourceId r2) (tf_dependencies s)})

-- | Execute the TF monadic action to generating the graph of terraform
-- resources, writing them to the specified directory.
generateFiles :: FilePath -> TF a -> IO a
generateFiles outDir tfa = do
  (a,state) <- runStateT tfa state0
  let files = S.fromList [ file | (file:_) <- map (reverse.r_name) (tf_resources state)]
              `S.union`
              S.fromList [ file | (file:_) <- map (reverse.o_name) (tf_outputs state)]
  for_ files $ \file -> do
    let content
          =  [generateProvider state p | p <- reverse (tf_providers state), matchName0 file (p_name p) ]
          <> [generateResource state r | r <- reverse (tf_resources state), matchName0 file (r_name r) ]
          <> [generateOutput r | r <- reverse (tf_outputs state), matchName0 file (o_name r) ]
    T.writeFile (outDir </> T.unpack file <> ".tf") (T.intercalate "\n\n" content)
  return a
  where
    state0 = TFState [] [] [] [] S.empty
    matchName0 n ns = case reverse ns of
      (n0:_) -> n == n0
      _ -> False

    generateProvider state p = T.intercalate "\n" (
      [ T.template  "provider \"$1\" {" [p_type p] ]
      <>
      generateFieldMap "  " (p_fields p)
      <>
      ["}"]
      )

    generateResource state r = T.intercalate "\n" (
      [ T.template  "resource \"$1\" \"$2\" {" [r_type r, nameText (r_name r)] ]
      <>
      generateFieldMap "  " fieldMap
      <>
      ["}"]
      )
      where
        fieldMap = M.union (r_fields r) dependsMap
        dependsMap | null depends = M.empty
                   | otherwise = M.singleton "depends_on" (toResourceField [rtype <> "." <> nameText rname | (ResourceId rtype rname) <- depends])
        rid = ResourceId (r_type r) (r_name r)
        depends = [r2 | (r1,r2) <- S.toList (tf_dependencies state), r1 == rid]


    generateFieldMap indent fieldMap = concatMap generateField (M.toList fieldMap)
      where
        generateField (field,RF_Text value)  =  [T.template "$1$2 = $3" [indent,field,quotedText value]]
        generateField (field,RF_List values)
          =  [T.template "$1$2 = [" [indent,field]]
          <> generateValues (indent <> "  ") values
          <> [T.template "$1]" [indent]]
        generateField (field,RF_Map map)
          =  [T.template "$1$2 {" [indent,field]]
          <> generateFieldMap (indent <> "  ") map
          <> [T.template "$1}" [indent]]

    generateValues indent values = concatMap generateValue (zip values terms)
      where
        generateValue (RF_Text value,term)
          =  [T.template "$1\"$2\"$3" [indent,value,term]]
        generateValue (RF_List values,term)
          =  [indent <> "["]
          <> generateValues (indent <> "  ") values
          <> [indent <> "]" <> term]
        generateValue (RF_Map map,term)
          =  [indent <> "{"]
          <> generateFieldMap (indent <> "  ") map
          <> [indent <> "}" <> term]

        terms = replicate (length values - 1) "," <> [""]

    generateOutput o = T.intercalate "\n"
      [ T.template "output \"$1\" {" [nameText (o_name o)]
      , T.template "  value = \"$1\"" [o_value o]
      , "}"
      ]

    quotedText :: T.Text -> T.Text
    quotedText value
       | needsQuoting value = T.template "<<$1\n$2$3$1"
           [uniqueEof value, value, if T.isSuffixOf "\n" value then "" else "\n"]
       | otherwise =  "\"" <> value <> "\""

    needsQuoting :: T.Text -> Bool
    needsQuoting value = T.isInfixOf "\n" value || T.isInfixOf "\"" value

    uniqueEof :: T.Text -> T.Text
    uniqueEof value = head (filter (\eof -> not (T.isInfixOf eof value)) eofs)
      where
        eofs =  ["EOF"] <> ["EOF" <> (T.pack (show n)) | n <- [1,2..]]
    
