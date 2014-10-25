import Text.ParserCombinators.Parsec

parser = endBy row eol
    where
        row = sepBy value (char ',')
        value = many (noneOf ",\n")
        eol = char '\n'
