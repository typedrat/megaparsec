{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Megaparsec.ByteSpec (spec) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Char (isControl, isPrint, isLower, toLower, toUpper)
import Data.Function (on)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc.Byte
import Test.Hspec.Megaparsec.AdHoc.Common (nes)
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

spec :: Spec
spec = do

  describe "newline" $
    checkStrLit "newline" "\n" (tokenToChunk bproxy <$> newline)

  describe "csrf" $
    checkStrLit "crlf newline" "\r\n" crlf

  describe "eol" $ do
    context "when stream begins with a newline" $
      it "succeeds returning the newline" $
        property $ \s -> do
          let s' = "\n" <> s
          prs  eol s' `shouldParse`     "\n"
          prs' eol s' `succeedsLeaving` s
    context "when stream begins with CRLF sequence" $
      it "parses the CRLF sequence" $
        property $ \s -> do
          let s' = "\r\n" <> s
          prs  eol s' `shouldParse`     "\r\n"
          prs' eol s' `succeedsLeaving` s
    context "when stream begins with '\\r', but it's not followed by '\\n'" $
      it "signals correct parse error" $
        property $ \ch -> ch /= 10 ==> do
          let s = "\r" <> B.singleton ch
          prs eol s `shouldFailWith`
            err posI (utoks (B.unpack s) <> elabel "end of line")
    context "when input stream is '\\r'" $
      it "signals correct parse error" $
        prs eol "\r" `shouldFailWith` err posI
          (utok 13 <> elabel "end of line")
    context "when stream does not begin with newline or CRLF sequence" $
      it "signals correct parse error" $
        property $ \ch s -> (ch /= 13 && ch /= 10) ==> do
          let s' = B.singleton ch <> s
          prs eol s' `shouldFailWith` err posI
            (utoks (B.unpack $ B.take 2 s') <> elabel "end of line")
    context "when stream is empty" $
      it "signals correct parse error" $
        prs eol "" `shouldFailWith` err posI
          (ueof <> elabel "end of line")

  describe "tab" $
    checkStrLit "tab" "\t" (tokenToChunk bproxy <$> tab)

  describe "space" $
    it "consumes space up to first non-space character" $
      property $ \s' -> do
        let (s0,s1) = B.partition isSpace s'
            s = s0 <> s1
        prs  space s `shouldParse` ()
        prs' space s `succeedsLeaving` s1

  describe "space1" $ do
    context "when stream does not start with a space character" $
      it "signals correct parse error" $
        property $ \ch s' -> not (isSpace ch) ==> do
          let (s0,s1) = B.partition isSpace s'
              s = B.singleton ch <> s0 <> s1
          prs  space1 s `shouldFailWith` err posI (utok ch <> elabel "white space")
          prs' space1 s `failsLeaving` s
    context "when stream starts with a space character" $
      it "consumes space up to first non-space character" $
        property $ \s' -> do
          let (s0,s1) = B.partition isSpace s'
              s = " " <> s0 <> s1
          prs  space1 s `shouldParse` ()
          prs' space1 s `succeedsLeaving` s1
    context "when stream is empty" $
      it "signals correct parse error" $
        prs space1 "" `shouldFailWith` err posI (ueof <> elabel "white space")

  describe "controlChar" $
    checkCharPred "control character" (isControl . toChar) controlChar

  describe "spaceChar" $
    checkCharRange "white space" [9,10,11,12,13,32,160] spaceChar

  describe "alphaNumChar" $
    checkCharPred "alphanumeric character" isAlphaNum alphaNumChar

  describe "printChar" $
    checkCharPred "printable character" (isPrint . toChar) printChar

  describe "digitChar" $
    checkCharRange "digit" [48..57] digitChar

  describe "octDigitChar" $
    checkCharRange "octal digit" [48..55] octDigitChar

  describe "hexDigitChar" $
    checkCharRange "hexadecimal digit" ([48..57] ++ [97..102] ++ [65..70]) hexDigitChar

  describe "char'" $ do
    context "when stream begins with the character specified as argument" $
      it "parses the character" $
        property $ \ch s -> do
          let sl = B.cons (liftChar toLower ch) s
              su = B.cons (liftChar toUpper ch) s
          prs  (char' ch) sl `shouldParse`     liftChar toLower ch
          prs  (char' ch) su `shouldParse`     liftChar toUpper ch
          prs' (char' ch) sl `succeedsLeaving` s
          prs' (char' ch) su `succeedsLeaving` s
    context "when stream does not begin with the character specified as argument" $
      it "signals correct parse error" $
        property $ \ch ch' s -> liftChar toLower ch /= liftChar toLower ch' ==> do
          let s' = B.cons ch' s
              ms = utok ch' <> etok (liftChar toLower ch) <> etok (liftChar toUpper ch)
          prs  (char' ch) s' `shouldFailWith` err posI ms
          prs' (char' ch) s' `failsLeaving`   s'
    context "when stream is empty" $
      it "signals correct parse error" $
        property $ \ch -> do
          let ms = ueof <> etok (liftChar toLower ch) <> etok (liftChar toUpper ch)
          prs  (char' ch) "" `shouldFailWith` err posI ms

  describe "string" $ do
    context "when stream is prefixed with given string" $
      it "parses the string" $
        property $ \str s -> do
          let s' = str <> s
          prs  (string str) s' `shouldParse`     str
          prs' (string str) s' `succeedsLeaving` s
    context "when stream is not prefixed with given string" $
      it "signals correct parse error" $
        property $ \str s -> not (str `B.isPrefixOf` s) ==> do
          let us = B.take (B.length str) s
          prs (string str) s `shouldFailWith`
            err posI (utoks (B.unpack us) <> etoks (B.unpack str))

  describe "string'" $ do
    context "when stream is prefixed with given string" $
      it "parses the string" $
        property $ \str s ->
          forAll (fuzzyCase str) $ \str' -> do
            let s' = str' <> s
            prs  (string' str) s' `shouldParse`     str'
            prs' (string' str) s' `succeedsLeaving` s
    context "when stream is not prefixed with given string" $
      it "signals correct parse error" $
        property $ \str s -> not (str `isPrefixOfI` s) ==> do
          let us = B.take (B.length str) s
          prs  (string' str) s `shouldFailWith`
            err posI (utoks (B.unpack us) <> etoks (B.unpack str))

----------------------------------------------------------------------------
-- Helpers

checkStrLit :: String -> ByteString -> Parser ByteString -> SpecWith ()
checkStrLit name ts p = do
  context ("when stream begins with " ++ name) $
    it ("parses the " ++ name) $
      property $ \s -> do
        let s' = ts <> s
        prs  p s' `shouldParse`     ts
        prs' p s' `succeedsLeaving` s
  context ("when stream does not begin with " ++ name) $
    it "signals correct parse error" $
      property $ \ch s -> ch /= B.head ts ==> do
       let s' = B.cons ch s
           us = B.unpack $ B.take (B.length ts) s'
           ps = B.unpack ts
       prs  p s' `shouldFailWith` err posI (utoks us <> etoks ps)
       prs' p s' `failsLeaving`   s'
  context "when stream is empty" $
    it "signals correct parse error" $
      prs p "" `shouldFailWith` err posI (ueof <> etoks (B.unpack ts))

checkCharPred :: String -> (Word8 -> Bool) -> Parser Word8 -> SpecWith ()
checkCharPred name f p = do
  context ("when stream begins with " ++ name) $
    it ("parses the " ++ name) $
      property $ \ch s -> f ch ==> do
        let s' = B.singleton ch <> s
        prs  p s' `shouldParse`     ch
        prs' p s' `succeedsLeaving` s
  context ("when stream does not begin with " ++ name) $
    it "signals correct parse error" $
      property $ \ch s -> not (f ch) ==> do
       let s' = B.singleton ch <> s
       prs  p s' `shouldFailWith` err posI (utok ch <> elabel name)
       prs' p s' `failsLeaving`   s'
  context "when stream is empty" $
    it "signals correct parse error" $
      prs p "" `shouldFailWith` err posI (ueof <> elabel name)

checkCharRange :: String -> [Word8] -> Parser Word8 -> SpecWith ()
checkCharRange name tchs p = do
  forM_ tchs $ \tch ->
    context ("when stream begins with " ++ showTokens (nes tch)) $
      it ("parses the " ++ showTokens (nes tch)) $
        property $ \s -> do
          let s' = B.singleton tch <> s
          prs  p s' `shouldParse`     tch
          prs' p s' `succeedsLeaving` s
  context "when stream is empty" $
    it "signals correct parse error" $
      prs p "" `shouldFailWith` err posI (ueof <> elabel name)

bproxy :: Proxy ByteString
bproxy = Proxy

-- | Lift char transformation to byte transformation.

liftChar :: (Char -> Char) -> Word8 -> Word8
liftChar f x = (fromMaybe x . fromChar . f . toChar) x

-- | Randomly change the case in the given string.

fuzzyCase :: ByteString -> Gen ByteString
fuzzyCase s = B8.pack . zipWith f (B8.unpack s) <$> vector (B.length s)
  where
    f k True  = if isLower k then toUpper k else toLower k
    f k False = k

isPrefixOfI :: ByteString -> ByteString -> Bool
isPrefixOfI = isPrefixOf `on` fmap toUpper . B8.unpack
