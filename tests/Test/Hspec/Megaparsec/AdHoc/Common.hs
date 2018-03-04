{-# LANGUAGE CPP                  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Hspec.Megaparsec.AdHoc.Common
  ( nes )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Void
import Test.QuickCheck
import Text.Megaparsec
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty   as NE
import qualified Data.Set             as E
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL

----------------------------------------------------------------------------
-- Working with source position

-- | Make a singleton non-empty list from a value.

nes :: a -> NonEmpty a
nes x = x :| []

----------------------------------------------------------------------------
-- Arbitrary instances

instance Arbitrary Void where
  arbitrary = error "Arbitrary Void"

instance Arbitrary Pos where
  arbitrary = mkPos <$> (getSmall <$> arbitrary `suchThat` (> 0))

instance Arbitrary SourcePos where
  arbitrary = SourcePos
    <$> sized (\n -> do
          k <- choose (0, n `div` 2)
          vectorOf k arbitrary)
    <*> arbitrary
    <*> arbitrary

instance Arbitrary t => Arbitrary (ErrorItem t) where
  arbitrary = oneof
    [ Tokens <$> (NE.fromList . getNonEmpty <$> arbitrary)
    , Label  <$> (NE.fromList . getNonEmpty <$> arbitrary)
    , return EndOfInput ]

instance Arbitrary (ErrorFancy a) where
  arbitrary = oneof
    [ sized (\n -> do
        k <- choose (0, n `div` 2)
        ErrorFail <$> vectorOf k arbitrary)
    , ErrorIndentation <$> arbitrary <*> arbitrary <*> arbitrary ]

instance (Arbitrary t, Ord t, Arbitrary e, Ord e)
    => Arbitrary (ParseError t e) where
  arbitrary = oneof
    [ TrivialError
      <$> (NE.fromList . getNonEmpty <$> arbitrary)
      <*> arbitrary
      <*> (E.fromList <$> arbitrary)
    , FancyError
      <$> (NE.fromList . getNonEmpty <$> arbitrary)
      <*> (E.fromList <$> arbitrary) ]

instance Arbitrary a => Arbitrary (State a) where
  arbitrary = State
    <$> arbitrary
    <*> (NE.fromList . getNonEmpty <$> arbitrary)
    <*> choose (1, 10000)
    <*> (mkPos <$> choose (1, 20))

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TL.Text where
  arbitrary = TL.pack <$> arbitrary

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary

instance Arbitrary BL.ByteString where
  arbitrary = BL.pack <$> arbitrary

#if MIN_VERSION_QuickCheck(2,10,0)
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NE.fromList <$> (arbitrary `suchThat` (not . null))
#endif
