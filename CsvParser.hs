module CsvParser (csvToDoubles,
                 listsToPairs) where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative hiding (many)

parser = endBy row eol
    where
        row = sepBy value (char ',')
        value = many (noneOf ",\n")
        eol = char '\n'

csvToStrings :: String -> String -> Either ParseError [[String]]
csvToStrings input path = parse parser path input

stringToDouble :: [String] -> [Double]
stringToDouble = map read

stringsToDoubles :: [[String]] -> [[Double]]
stringsToDoubles = map stringToDouble

csvToDoubles :: String -> String -> Either ParseError [[Double]]
csvToDoubles input path = fmap stringsToDoubles $ csvToStrings input path

listsToPairs :: Either ParseError [[Double]] -> Either ParseError [(Double, Double)]
listsToPairs (Left error) = Left error
listsToPairs lists = fmap toPairs lists
    where
        toPair (x:y:_) = (x, y)
        toPairs = map toPair


