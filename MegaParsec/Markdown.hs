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
    parseTest boldCombined input

-- Different ways of doing the same thing

boldP :: Parser String
boldP = do
    count 2 (char '*')
    -- should accept basically everything other than '*'
    txt <- some (alphaNumChar <|> char ' ')
    count 2 (char '*')
    return ("<strong>" <> txt <> "<strong>")

boldBoundary :: Parser a -> Parser a
boldBoundary = between (string "**") (string "**")

boldP' :: Parser String
boldP' = do
    txt <- some (alphaNumChar <|> char ' ')
    return $ "<strong>" <> txt <> "<strong>"

boldCombined :: Parser String
boldCombined = boldBoundary internalParser
    where
        boldBoundary :: Parser a -> Parser a
        boldBoundary = between (string "**") (string "**")

        internalParser :: Parser String
        internalParser = do
            txt <- some (alphaNumChar <|> char ' ')
            return $ "<strong>" <> txt <> "<strong>"