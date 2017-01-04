-- | Helper functions for dealing with text values
module Language.Terraform.Util.Text(
  template,
  show
) where

import Prelude hiding(show)

import qualified Prelude(show)
import qualified Data.Text as T


show :: (Show a) => a -> T.Text
show = T.pack . Prelude.show

-- | `template src substs` will replace all occurences the string $i
-- in src with `substs !! i`
template :: T.Text -> [T.Text] -> T.Text
template t substs = foldr replace t (zip [1,2..] substs)
  where
    replace (i,s) t = T.replace (T.pack ('$':Prelude.show i)) s t


  
