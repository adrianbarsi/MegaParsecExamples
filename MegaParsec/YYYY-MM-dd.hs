import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment
import Data.Void
import Data.Char
import Data.Time

type Parser = Parsec Void String

main = do
    input <- fmap head getArgs
    parseTest yearMonthDayParser input

yearMonthDayParser :: Parser String
yearMonthDayParser = do
    year <- count 4 digitChar
    char '-'
    month <- count' 1 2 digitChar
    char '-'
    day <- count' 1 2 digitChar
    eof
    return $ validate year month day
    where
        validate year month day =
            let
                yearNum = read year :: Integer
                monthNum = read month :: Integer
                dayNum = read day :: Integer
            in
                if (yearNum >= 1000 && yearNum <= 9999) && (monthNum >= 1 && monthNum <= 12) && (dayNum >= 1 && dayNum <= (maxDays yearNum monthNum))
                    then
                        "Valid"
                    else
                        "Invalid"
        maxDays _ 1 = 31
        maxDays year 2 = if isLeapYear year then 29 else 28
        maxDays _ 3 = 31
        maxDays _ 4 = 30
        maxDays _ 5 = 31
        maxDays _ 6 = 30
        maxDays _ 7 = 31
        maxDays _ 8 = 31
        maxDays _ 9 = 30
        maxDays _ 10 = 31
        maxDays _ 11 = 30
        maxDays _ 12 = 31