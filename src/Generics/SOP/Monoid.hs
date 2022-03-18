-- | = Generic Semigroup and Monoid instances
-- 
-- We generically define a 'gappend' and 'gempty' function
-- that can be used to implement Semigroup/Monoid instances
-- on any product type whose factors are semigroups/monoids.
-- 
-- For example:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- > import qualified GHC.Generics
-- > import qualified Generics.SOP
-- > import           Generics.SOP.Monoid
-- > import           Data.Semigroup
-- > 
-- > data X = X () (Sum Int) ([()], String)
-- >    deriving (Show, GHC.Generics.Generic)
-- > instance Generics.SOP.Generic X
-- >
-- > instance Semigroup X where (<>) = gappend
-- > instance Monoid    X where mempty = gempty
--
-- >>> X () (Sum 1) ([()], "Hello") <> X () (Sum 41) ([(),()], " world")
-- X () (Sum {getSum = 42}) ([(),(),()],"Hallo Welt")
-- >>> mempty :: X
-- X () (Sum {getSum = 0}) ([],"")
--
-- (Note the explicit type constraints involving 'Compose' and 'I' from
-- the @sop-core@ package in the types of 'gappend' and 'gempty'.
-- Apparently, GHC is not smart enough to promote
-- @All Semigroup xs@ to @All (Compose Semigroup I) xs)@
-- as well as to infer the latter from
-- @All (Compose Monoid I) xs@
-- /generically/, but it seems to be able to sort everything out in
-- /specific instances/ – as in the example above.)
module Generics.SOP.Monoid (
      gappend
    , gempty
    ) where

import Data.Function (on)
import Generics.SOP

gappend :: (IsProductType a xs, All (Compose Semigroup I) xs) => a -> a -> a
gappend = (productTypeTo .) . ((<>) `on` productTypeFrom)

gempty :: (IsProductType a xs, All (Compose Semigroup I) xs, All (Compose Monoid I) xs) => a
gempty = productTypeTo mempty
