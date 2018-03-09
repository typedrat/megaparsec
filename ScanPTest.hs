{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Char
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser a = Parsec Void a a

oldImpl :: Parser T.Text
oldImpl = do
    letters <- fmap T.pack $ (:) <$> letterChar <*> many letterChar

    let takeNums :: T.Text -> Parser T.Text
        takeNums prev
            | T.null prev = takeNums =<< (T.singleton <$> digitChar)
            | otherwise = do
                num <- digitChar
                let last' = fromEnum (T.last prev)
                    num'  = fromEnum num
                    next  = T.snoc prev num
                if num' - last' == 1
                    then return next
                    else takeNums next
    nums <- takeNums ""

    return $ T.append letters nums

data PState = Letters | Num Char

scanImpl :: Parser T.Text
scanImpl = scanP (Just "characters with fancy thing at the end") Letters pred
    where
        difference a b = (fromEnum a) - (fromEnum b)
        pred Letters (Just tok)
            | isAlpha tok = Continue Letters
            | isNumber tok = Continue (Num tok)
            | otherwise = Expected tok "alphanumeric character"
        pred Letters Nothing = OutOfInput "alphanumeric character"
        pred (Num last) (Just tok)
            | isNumber tok && difference tok last == 1 = Done
            | isNumber tok = Continue (Num tok)
            | otherwise = Expected tok "digit"
        pred (Num _) Nothing = OutOfInput "digit"

main :: IO ()
main = do
    putStrLn "Non-`scanP` parser, success: (input = \"abc12ccc\")"
    parseTest oldImpl "abc12ccc"
    putStrLn ""

    putStrLn "Non-`scanP` parser, failure: (input = \"abc11ccc\")"
    parseTest oldImpl "abc11ccc"
    putStrLn ""

    putStrLn "`scanP` parser, success: (input = \"abc12ccc\")"
    parseTest scanImpl "abc12ccc"
    putStrLn ""

    putStrLn "`scanP` parser, failure: (input = \"abc11ccc\")"
    parseTest scanImpl "abc11ccc"
    putStrLn ""
