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
module Generics.SOP.Monoid (
      gappend
    , gempty
    ) where

import Control.Applicative (liftA2)
import Data.Function (on)
import Generics.SOP

gappend :: (IsProductType a xs, All Semigroup xs) => a -> a -> a
gappend = (to .) . (gappend_SOP `on` from)

gappend_SOP :: (All Semigroup xs) => SOP I '[xs] -> SOP I '[xs] -> SOP I '[xs]
gappend_SOP (SOP (Z x)) (SOP (Z y)) = SOP $ Z $ gappend_NP x y

gappend_NP :: (All Semigroup xs) => NP I xs -> NP I xs -> NP I xs
gappend_NP = hcliftA2 (Proxy :: Proxy Semigroup) (liftA2 (<>))


gempty :: (IsProductType a xs, All Monoid xs) => a
gempty = to gempty_SOP

gempty_SOP :: (All Monoid xs) => SOP I '[xs]
gempty_SOP = SOP $ Z $ hcpure (Proxy :: Proxy Monoid) (I mempty)
