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

-- Combinators combine one or more parsers in different ways
-- ex. <|> lets you try multiple parsers sequentially. If they all fail the overall parser function fails. If one of the parsers succeed then the overall parser function continues

-- A 'word' is a continuous string of alphanumeric characters. No whitespace or special characters are allowed

main = do
    input <- fmap head getArgs
    parseTest wordP input

-- We are matching a string instead of a character
-- Some will run the parser continuously one or more times as long as it keeps being successful
-- If there are only invalid values it will throw an exception
wordP :: Parser String
wordP = some alphaNumChar

-- Many runs the parser 0 or more times