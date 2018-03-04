{-# LANGUAGE LambdaCase #-}

module Test.Hspec.Megaparsec.AdHoc.Byte
  ( -- * Types
    Parser
    -- * Helpers to run parsers
  , prs
  , prs'
    -- * Other
  , isDigit
  , isOctDigit
  , isHexDigit
  , isAlpha
  , isAlphaNum
  , isSpace
  , toChar
  , fromChar )
where

import Data.ByteString (ByteString)
import Data.Void
import Data.Word (Word8)
import Test.Hspec.Megaparsec
import Text.Megaparsec
import qualified Data.Char as Char

----------------------------------------------------------------------------
-- Types

type Parser = Parsec Void ByteString

----------------------------------------------------------------------------
-- Helpers to run parsers

-- | Apply parser to given input. This is a specialized version of 'parse'
-- that assumes empty file name.

prs
  :: Parser a          -- ^ Parser to run
  -> ByteString        -- ^ Input for the parser
  -> Either (ParseError Word8 Void) a -- ^ Result of parsing
prs p = parse p ""

prs'
  :: Parser a          -- ^ Parser to run
  -> ByteString        -- ^ Input for the parser
  -> (State ByteString, Either (ParseError Word8 Void) a) -- ^ Result of parsing
prs' p s = runParser' p (initialState s)

----------------------------------------------------------------------------
-- Other

isDigit :: Word8 -> Bool
isDigit = Char.isDigit . toChar

isOctDigit :: Word8 -> Bool
isOctDigit = Char.isOctDigit . toChar

isHexDigit :: Word8 -> Bool
isHexDigit = Char.isHexDigit . toChar

isAlpha :: Word8 -> Bool
isAlpha = Char.isAlpha . toChar

isAlphaNum :: Word8 -> Bool
isAlphaNum = Char.isAlphaNum . toChar

-- | 'Word8'-specialized version of 'isSpace'.

isSpace :: Word8 -> Bool
isSpace w
  | w >= 9 && w <= 13 = True
  | w == 32           = True
  | w == 160          = True
  | otherwise         = False

-- | Convert a byte to char.

toChar :: Word8 -> Char
toChar = Char.chr . fromIntegral

-- | Covert a char to byte.

fromChar :: Char -> Maybe Word8
fromChar x = let p = Char.ord x in
  if p > 0xff
    then Nothing
    else Just (fromIntegral p)
