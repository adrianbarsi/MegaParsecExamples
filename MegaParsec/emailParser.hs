import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment
import Data.Void

-- Parser - Synonym for Parsec
-- Void - Default error messaging
-- String - Type of input stream
-- Parser :: * -> *
-- Parser still needs to know the type for the result of parsing
-- This will be concretized for specific parsers
type Parser = Parsec Void String

main = do
    input <- fmap head getArgs
    parseTest emailP input

emailP :: Parser String
emailP = do
    some (alphaNumChar <|> char '.' <|> char '_' <|> char '%' <|> char '+' <|> char '-')
    char '@'
    some (alphaNumChar <|> char '-')
    char '.'
    count' 2 6 letterChar
    eof
    return "Valid Email"