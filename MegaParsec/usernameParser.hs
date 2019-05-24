import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment
import Data.Void

type Parser = Parsec Void String

main = do
    input <- fmap head getArgs
    parseTest userNameParser input

userNameParser :: Parser String
userNameParser = do
    x <- count' 3 16 (alphaNumChar <|> char '-' <|> char '_')
    eof
    return x