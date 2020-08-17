{-# LANGUAGE OverloadedStrings,FlexibleInstances, ScopedTypeVariables #-}
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
  ResourceFieldMap(..),
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
  ignoreChanges,
  createBeforeDestroy,
  localExecProvisioner,
  withNameScope,
  scopedName,
  scopedName',
  nameContext,
  withContext,
  getContext,
  generateFiles,
  rfmField,
  rfmOptionalField,
  rfmOptionalDefField,
  rfmExpandedList,
  ) where

import Data.Default
import Data.Maybe(fromMaybe)
import Data.Semigroup
import Data.Monoid (Monoid (..))
import Data.Typeable
import Data.Dynamic
import Control.Applicative ((<$>))
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

data Provisioner = Provisioner {
  pv_type :: T.Text,
  pv_fields :: ResourceFieldMap
}

-- | A map to be embedded in the terraform output.
--
-- we can't actually use a regular map here, as repeated
-- keys are allowed.
newtype ResourceFieldMap = ResourceFieldMap { unResourceFieldMap :: [(T.Text,ResourceField)] }

instance Semigroup ResourceFieldMap where
  ResourceFieldMap f1 <> ResourceFieldMap f2 = ResourceFieldMap (f1 <> f2)

instance Monoid ResourceFieldMap where
  mempty = ResourceFieldMap []
  mappend = (<>)

rfmField :: ToResourceField f => T.Text -> f -> ResourceFieldMap
rfmField field v = ResourceFieldMap [(field, toResourceField v)]

rfmOptionalField :: ToResourceField f => T.Text -> Maybe f -> ResourceFieldMap
rfmOptionalField _ Nothing = mempty
rfmOptionalField field (Just v) = rfmField field v

rfmOptionalDefField :: (Eq f,ToResourceField f) => T.Text -> f -> f -> ResourceFieldMap
rfmOptionalDefField field defv v | defv == v = mempty
                                 | otherwise = rfmField field v

rfmExpandedList :: ToResourceField f => T.Text -> [f]  -> ResourceFieldMap
rfmExpandedList field vs = ResourceFieldMap [(field, toResourceField v) | v <- vs]

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
  toResourceFieldList cs = RF_Text (T.pack cs)

instance ToResourceField Bool where
  toResourceField True = RF_Text "true"
  toResourceField False = RF_Text "false"

instance ToResourceField a => ToResourceField [a] where
  toResourceField = toResourceFieldList

instance ToResourceField a => ToResourceField (M.Map T.Text a) where
  toResourceField = RF_Map . ResourceFieldMap . M.toList . M.map toResourceField

data ResourceLifeCycle = ResourceLifeCycle {
  rlc_ignoreChanges :: S.Set T.Text,
  rlc_createBeforeDestroy :: Bool
  }

instance Default ResourceLifeCycle where
  def = ResourceLifeCycle S.empty False
     
data TFState = TFState {
  tf_nameContext :: [NameElement],
  tf_context :: M.Map TypeRep Dynamic,
  tf_providers :: [Provider],
  tf_resources :: [Resource],
  tf_outputs :: [Output],
  tf_dependencies :: S.Set (ResourceId,ResourceId),
  tf_lifecycle :: M.Map ResourceId ResourceLifeCycle,
  tf_provisioners :: M.Map ResourceId [Provisioner]
  }

-- | A state monad over IO that accumulates the
-- terraform resource graph.
type TF a = StateT TFState IO a

nameText :: Name -> T.Text
nameText nameElements = T.intercalate "_" (reverse nameElements)

-- | Generate a global name based upon the the current scope.
scopedName :: NameElement -> TF T.Text
scopedName name0 = do
  context <- tf_nameContext <$> get
  return (nameText (name0:context))

-- | Generate a global name based upon the the current scope, returning
-- the name components.  
scopedName' :: NameElement -> TF Name
scopedName' name0 = do
  context <- tf_nameContext <$> get
  return (reverse (name0:context))

nameContext :: TF [NameElement]
nameContext = tf_nameContext <$> get

-- | Provide a more specific naming scope for the specified terraform
-- action.  
withNameScope:: NameElement -> TF a -> TF a
withNameScope name tfa = do
  s0 <- get
  put s0{tf_nameContext=name:tf_nameContext s0}
  a <- tfa
  s1 <- get
  put s1{tf_nameContext=tf_nameContext s0}
  return a

withContext :: Typeable c => c ->  TF a -> TF a
withContext c tfa = do
  s0 <- get
  let dyn = toDyn c
  put s0{tf_context=M.insert (dynTypeRep dyn) dyn (tf_context s0)}
  a <- tfa
  s1 <- get
  put s1{tf_context=tf_context s0}
  return a

getContext :: forall c . Typeable c => TF (Maybe c)
getContext = do
  s0 <- get
  case M.lookup (typeRep (Proxy :: Proxy c)) (tf_context s0) of
     Nothing -> return Nothing
     (Just dyn) -> return (fromDynamic dyn)

-- | Internal function for constructing terraform providers
mkProvider :: TFType -> [(T.Text,ResourceField)] -> TF ()
mkProvider tftype fields  = do
  name <- fmap tf_nameContext get
  let provider = Provider tftype name (ResourceFieldMap fields)
  modify' (\s -> s{tf_providers=provider:tf_providers s})

-- | Internal function for constructing terraform resources
mkResource :: TFType -> NameElement -> ResourceFieldMap -> TF ResourceId 
mkResource tftype name0 fieldmap  = do
  s <- get
  let name = name0:tf_nameContext s
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
  let name = name0:tf_nameContext s
  let output = Output name value
  modify' (\s -> s{tf_outputs=output:tf_outputs s})

-- | Specifiy an explicit depedency betweeen resources.
-- (See https://www.terraform.io/intro/getting-started/dependencies.html)
dependsOn :: (IsResource r1,IsResource r2) => r1 -> r2 -> TF ()
dependsOn r1 r2 = modify' (\s->s{tf_dependencies=S.insert (resourceId r1, resourceId r2) (tf_dependencies s)})

-- | Specify that a resource will ignore changes to the specified attribute
-- (See https://www.terraform.io/docs/configuration/resources.html#ignore_changes)
ignoreChanges :: (IsResource r) => r -> T.Text -> TF ()
ignoreChanges r attr = modify' (\s->s{tf_lifecycle=M.alter (addIgnored attr) (resourceId r) (tf_lifecycle s)})
  where
    addIgnored :: T.Text -> Maybe ResourceLifeCycle -> Maybe ResourceLifeCycle
    addIgnored field Nothing = addIgnored field (Just def)
    addIgnored field (Just rlc) = Just rlc{rlc_ignoreChanges=S.insert field (rlc_ignoreChanges rlc)}

-- | Specify that a new resource will be created before an old one is destroyed.
-- (See https://www.terraform.io/docs/configuration/resources.html#createBeforeDestroy)
createBeforeDestroy :: (IsResource r) => r -> Bool -> TF ()
createBeforeDestroy r v = modify' (\s->s{tf_lifecycle=M.alter (setFlag v) (resourceId r) (tf_lifecycle s)})
  where
    setFlag :: Bool -> Maybe ResourceLifeCycle -> Maybe ResourceLifeCycle
    setFlag v Nothing = setFlag v (Just def)
    setFlag v (Just rlc) = Just rlc{rlc_createBeforeDestroy=v}

-- | Add a local command to run after a resource is provisioned
localExecProvisioner :: IsResource r => r -> T.Text -> TF ()
localExecProvisioner r command = modify' (\s->s{tf_provisioners=M.insertWith (<>) (resourceId r) [provisioner] (tf_provisioners s)})
  where
    provisioner = Provisioner "local-exec" (ResourceFieldMap [("command", (RF_Text command))])

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
    state0 = TFState [] M.empty [] [] [] S.empty M.empty M.empty
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
      mconcat [ [T.template "  provisioner \"$1\" {" [pv_type pv]]
                <>
                generateFieldMap "    " (pv_fields pv)
                <>
                ["  }"]
                | pv <- provisioners ]
                
      <>
      lifecycle
      <>
      ["}"]
      )
      where
        fieldMap = r_fields r <> dependsMap
        dependsMap | null depends = mempty
                   | otherwise = ResourceFieldMap [("depends_on",(toResourceField [rtype <> "." <> nameText rname | (ResourceId rtype rname) <- depends]))]
        provisioners = fromMaybe [] (M.lookup rid (tf_provisioners state))
        rid = ResourceId (r_type r) (r_name r)
        depends = [r2 | (r1,r2) <- S.toList (tf_dependencies state), r1 == rid]
        lifecycle = case M.lookup rid (tf_lifecycle state) of
          Nothing -> mempty
          (Just rlc) ->
            let ignoreChanges | S.null (rlc_ignoreChanges rlc) = mempty
                              | otherwise = [T.template "    ignore_changes = [$1]"
                                             [T.intercalate ", " [ "\"" <> attr <> "\"" | attr <- S.toList (rlc_ignoreChanges rlc)]]]
                createBeforeDestroy | rlc_createBeforeDestroy rlc = ["    create_before_destroy = true"]
                                    | otherwise = mempty
            in case (ignoreChanges <> createBeforeDestroy) of
                [] -> mempty
                lines -> ["  lifecycle {"] <> lines <> ["  }"]

    generateFieldMap :: T.Text -> ResourceFieldMap -> [T.Text]
    generateFieldMap indent fieldMap = concatMap generateField (unResourceFieldMap fieldMap)
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
    
