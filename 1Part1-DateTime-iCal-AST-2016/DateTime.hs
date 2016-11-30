import ParseLib.Abstract
import Data.Char
import Data.Maybe


-- Starting Framework


-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year  { unYear  :: Int } deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day   { unDay   :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour   { unHour   :: Int } deriving (Eq, Ord)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord)


-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show


-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime =  (\w x y z -> DateTime w y z) <$> parseDate <*> dateSep <*> parseTime <*> parseUTC

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = Year <$> parse4Digits

parseMonth :: Parser Char Month
parseMonth = Month <$> parseDigits

parseDay :: Parser Char Day
parseDay = Day <$> parseDigits

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseUTC :: Parser Char Bool
parseUTC = (=='Z') <$> symbol 'Z' <|> const False <$> epsilon

parseHour :: Parser Char Hour
parseHour = Hour <$> parseDigits

parseMinute :: Parser Char Minute
parseMinute = Minute <$> parseDigits

parseSecond :: Parser Char Second
parseSecond = Second <$> parseDigits

parse4Digits :: Parser Char Int
parse4Digits = (\w x y z -> 1000*w + 100*x + 10*y + z) <$> parseDigit <*> parseDigit <*> parseDigit <*> parseDigit

parseDigits :: Parser Char Int
parseDigits = (\x y -> 10*x + y) <$> parseDigit <*> parseDigit

parseDigit :: Parser Char Int
parseDigit = f <$> satisfy isDigit
             where f c = ord c - ord '0'

dateSep :: Parser Char Char             
dateSep = symbol 'T'


-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run parser xs = listToMaybe (map fst (filter op (parse parser xs)))
              where op (_, ys) = null ys


-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime date time utc) = printDate date ++ "T" ++ printTime time ++ op utc
        where op True = "Z"
              op False = "" 

printDate :: Date -> String
printDate (Date year month day) = ((addZeros 4).show.unYear) year ++ ((addZeros 2).show.unMonth) month ++ ((addZeros 2).show.unDay) day

printTime :: Time -> String
printTime (Time hour minute second) = ((addZeros 2).show.unHour) hour ++ ((addZeros 2).show.unMinute) minute ++ ((addZeros 2).show.unSecond) second

addZeros :: Int -> String -> String
addZeros n s | n > length s = addZeros n ('0':s)
             | otherwise = s


-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s


-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime {date = d, time = t}) = checkDate d && checkTime t

checkDate :: Date -> Bool
checkDate date@(Date {year = y, month = m, day = d})= checkYear y && checkMonth m && checkDay date

checkTime :: Time -> Bool
checkTime (Time {hour = h, minute = m, second = s}) = checkHour h && checkMinute m && checkSecond s

checkYear :: Year -> Bool
checkYear (Year {unYear = y}) = -1 < y && y < 10000

checkMonth :: Month -> Bool
checkMonth (Month {unMonth = m}) = 0<m && m < 13

checkDay :: Date -> Bool
checkDay (Date {year = y, month = mt@(Month {unMonth = m}), day = dy@(Day{unDay = d})}) | leapYear y && m == 2 = 0 < d && d < 30
                                                                                        | m == 2 = 0 < d && d < 29
                                                                                        | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 0 < d && d < 32
                                                                                        | otherwise = 0 < d && d < 31

checkHour :: Hour -> Bool
checkHour (Hour {unHour = h}) = -1 < h && h < 24

checkMinute :: Minute -> Bool
checkMinute (Minute {unMinute = m}) = -1 < m && m < 60

checkSecond :: Second -> Bool
checkSecond (Second {unSecond = s}) = -1 < s && s < 60

leapYear :: Year -> Bool
leapYear (Year {unYear = y}) | y `mod` 400 == 0 = True
                             | y `mod` 100 == 0 = False
                             | y `mod` 4   == 0 = True
                             | otherwise        = False


-- Exercise 6
data Event = Event { dtstamp     :: DTStamp,
                     uid         :: UID,
                     dtstart     :: DTStart,
                     dtend       :: DTEnd,
                     description :: Maybe Description,
                     summary     :: Maybe Summary,
                     location    :: Maybe Location }
                     
data DTStamp = DTStamp DateTime

data UID = UID String

data DTStart = DTStart DateTime

data DTEnd = DTEnd DateTime

data Description = Description String

data Summary = Summary String

data Location = Location String

data Calendar = Calendar { prodid :: ProdID, 
                           version :: Version,
                           events :: [Event] }
                           
data ProdID = ProdID String

data Version = Version