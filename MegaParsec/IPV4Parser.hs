import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment
import Data.Void
import Data.Char
import Data.Int
import Text.Read

type Parser = Parsec Void String

main = do
    input <- fmap head getArgs
    parseTest iPV4Parser input

octetParser :: Parser String
octetParser = count' 1 3 digitChar

isInt8 :: String -> Bool
isInt8 x = toBool (readMaybe x :: Maybe Integer)
    where
        toBool :: Maybe Integer -> Bool
        toBool (Just x) = if x >= 0 && x <= 255 then True else False
        toBool Nothing = False

iPV4Parser :: Parser String
iPV4Parser = do
    a <- octetParser
    char '.'
    b <- octetParser
    char '.'
    c <- octetParser
    char '.'
    d <- octetParser
    eof
    return $ if isInt8 a && isInt8 b && isInt8 c && isInt8 d then "Valid" else "Invalid"
