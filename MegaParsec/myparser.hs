-- myparser.hs

module MyParser (main) where

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
    -- get first command line arg
    input <- fmap head getArgs
    -- test singleLetterP with input
    parseTest singleLetterPOrJ input

-- only parses on first character of string. Ignores the rest of the string
singleLetterP :: Parser Char
singleLetterP = char 'h'

singleLetterPOrJ :: Parser Char
singleLetterPOrJ = char 'h' <|> char 'j'

-- many will repeatedly apply the Parser
-- parseTest (many (char 'a') :: Parser [Char]) "aabbb"
-- "aa"

-- To make it fail with an error:
-- > parseTest (many (char 'a') <* eof :: Parser [Char]) "aabbb"
--1:3:
--1 | aabbb  
--      ^
--unexpected 'b'
--expecting 'a' or end of input

-- char 'h' >> char 'a' is the same as
-- char 'h'
-- char 'a'
-- (<-) can be used instead of bind when consuming characters

-- Combinators
-- <|> This parser or this other parser if the first fails