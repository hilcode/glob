{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as DataText

main :: IO ()
main = do
    putStrLn "hello world"

data CharRange
    = CharRange Char Char
    | NegativeCharRange Char Char

match'CharRange :: Char -> CharRange -> Bool
match'CharRange ch (CharRange lowerBound upperBound)
    = lowerBound <= ch && ch <= upperBound
match'CharRange ch (NegativeCharRange lowerBound upperBound)
    = not (lowerBound <= ch && ch <= upperBound)

data FileGlobPart
    = (:?)
    | (:*)
    | Single Char
    | CharMatch [Char] [CharRange]

step'FileGlobPart :: Maybe (Char, Text) -> FileGlobPart -> [FileGlobPart] -> [PossibleMatch]
step'FileGlobPart text fileGlobPart fileGlobParts
    = case fileGlobPart of
        (:?) ->
            case text of
                Nothing         -> [
                                   ]
                Just (_, rest) -> [ PossibleMatch fileGlobParts rest
                                   ]
        (:*) ->
            case text of
                Nothing         -> [ PossibleMatch fileGlobParts ""
                                   ]
                Just (_, rest) -> [ PossibleMatch (fileGlobPart:fileGlobParts) rest
                                   , PossibleMatch fileGlobParts rest
                                   ]
        Single matchChar ->
            case text of
                Nothing         -> [
                                   ]
                Just (ch, rest) ->
                    if matchChar == ch
                        then       [ PossibleMatch fileGlobParts rest
                                   ]
                        else
                                   [
                                   ]
        CharMatch validChars validCharRanges ->
            case text of
                Nothing         -> [
                                   ]
                Just (ch, rest) ->
                    if ch `elem` validChars || True `elem` map (match'CharRange ch) validCharRanges
                        then       [ PossibleMatch fileGlobParts rest
                                   ]
                        else
                                   [
                                   ]

data FileGlob
    = FileGlob [FileGlobPart]

data PathGlobPart
    = (:**)
    | PathGlobPart FileGlob

data Glob
    = Glob [PathGlobPart]

data PossibleMatch
    = PossibleMatch [FileGlobPart] Text
    | Success

step :: PossibleMatch -> [PossibleMatch]
step Success
    = [Success]
step (PossibleMatch [] "")
    = [Success]
step (PossibleMatch [] _)
    = []
step (PossibleMatch (fileGlobPart:fileGlobParts) text)
    = step'FileGlobPart (DataText.uncons text) fileGlobPart fileGlobParts

-- match :: FileGlob -> Text -> Bool
