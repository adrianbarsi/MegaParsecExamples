import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment
import Data.Void
import Data.Char

type Parser = Parsec Void String

main = do
    input <- fmap head getArgs
    parseTest passwordStrengthP input

passwordStrengthP :: Parser String
passwordStrengthP = do
    password <- some (alphaNumChar <|> symbolChar)
    eof
    return $ showIsValid password 
    where
        showIsValid password =
            case isValid password of
                True -> "Valid"
                False -> "Invalid"
        isValid password =
            length password >= 8 &&
            existsIn isUpper password &&
            existsIn isLower password &&
            existsIn isDigit password &&
            existsIn isSymbol password
        existsIn f = (>0) . length . filter f