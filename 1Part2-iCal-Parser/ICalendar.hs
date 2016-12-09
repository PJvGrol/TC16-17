{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}


module ICalendar where

import ParseLib.Abstract
import Data.Char
import Data.Maybe
import System.IO
import Prelude hiding ((<$),(<*),(*>))
import qualified Text.PrettyPrint as PP
import Data.List


data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year  { unYear :: Int }  deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day   { unDay :: Int }   deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour   { unHour :: Int }   deriving (Eq, Ord)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord)


data Calendar = Calendar { prodId :: String
                         , events :: [VEvent] }
    deriving Eq

data VEvent = VEvent { dtStamp     :: DateTime
                     , uid         :: String
                     , dtStart     :: DateTime
                     , dtEnd       :: DateTime
                     , description :: Maybe String
                     , summary     :: Maybe String
                     , location    :: Maybe String }
    deriving Eq

-- All the properties now have a common data type, so we can parse all of them with one method
data Props = DtStamp DateTime
           | Uid String
           | DtStart DateTime
           | DtEnd DateTime
           | Description String
           | Summary String
           | Location String
           deriving (Ord, Eq)
           
run :: Parser a b -> [a] -> Maybe b
run p s = listToMaybe [p | (p, []) <- parse p s]

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar = run parseCalendar


-- "Main" block, DO NOT EDIT.
-- If you want to run the parser + pretty-printing, rename this module (first line) to "Main".
-- DO NOT forget to rename the module back to "ICalendar" before submitting to DomJudge.
main = do
    res <- readCalendar "examples/rooster_infotc.ics"
    putStrLn . PP.render $ maybe (PP.text "Calendar parsing error") (ppMonth (Year 2012) (Month 11)) res



-- Exercise 1
data Token = Token
    deriving (Eq, Ord, Show)

-- Parses the a Calendar, pretty straightforward
parseCalendar :: Parser Char Calendar
parseCalendar = Calendar <$ token "BEGIN:VCALENDAR\r\n" <*> parseCalProp <*> many parseEvent <* token "END:VCALENDAR\r\n" <* eof

-- Parses the properties of a Calendar. Because the order is not predetermined, both possibilities are parsed.
parseCalProp :: Parser Char String
parseCalProp = parseVersion *> parseProdId <|> parseProdId <* parseVersion

parseVersion :: Parser Char ()
parseVersion = const () <$> token "VERSION:2.0\r\n"

parseProdId :: Parser Char String
parseProdId = token "PRODID:" *> parseToEnd

-- Parses all properties of an event, then sorts them and tries to convert them to an Event
parseEvent :: Parser Char VEvent
parseEvent = (token "BEGIN:VEVENT\r\n" *> greedy parseProp <* token "END:VEVENT\r\n") >>= (propsToEvent.sort)

propsToEvent :: [Props] -> Parser Char VEvent
propsToEvent xs =  case mandProps xs of
                        (Nothing, _) -> empty
                        (Just x, ys) -> case testdes x ys of 
                                             Nothing -> empty
                                             Just y  -> succeed y

-- Because all these properties are mandatory, we can fail if at least one of them doesn't occur exactly once.
mandProps :: [Props] -> (Maybe VEvent, [Props])
mandProps (DtStamp dm : Uid u : DtStart ds : DtEnd de : xs) = (Just (VEvent dm u ds de Nothing Nothing Nothing), xs)
mandProps _ = (Nothing, [])

-- Now we test if the optional properties are present at most once
testdes :: VEvent -> [Props] -> Maybe VEvent
testdes v xs = case xs of
                    []                   -> Just v
                    (Description de:xss) -> testsum v{description = Just de} xss
                    _                    -> testsum v xs

testsum :: VEvent -> [Props] -> Maybe VEvent
testsum v xs = case xs of
                    []               -> Just v
                    (Summary su:xss) -> testloc v{summary = Just su} xss
                    _                -> testloc v xs
                                   
testloc :: VEvent -> [Props] -> Maybe VEvent
testloc v xs = case xs of
                    []            -> Just v
                    [Location lo] -> Just v{location = Just lo} 
                    _             -> Nothing -- This only occurs when one of the optional properties occurs more than once.

parseProp :: Parser Char Props
parseProp = DtStamp     <$ token "DTSTAMP:"     <*> parseDTToEnd <|>
            Uid         <$ token "UID:"         <*> parseToEnd   <|>
            DtStart     <$ token "DTSTART:"     <*> parseDTToEnd <|>
            DtEnd       <$ token "DTEND:"       <*> parseDTToEnd <|>
            Description <$ token "DESCRIPTION:" <*> parseToEnd   <|>
            Summary     <$ token "SUMMARY:"     <*> parseToEnd   <|>
            Location    <$ token "LOCATION:"    <*> parseToEnd
            where parseDTToEnd = parseDateTime <* token "\r\n"
            

parseToEnd :: Parser Char String
parseToEnd = greedy (satisfy (/='\r')) <* token "\r\n"

-- DateTime parser from Part 1

parseDateTime :: Parser Char DateTime
parseDateTime =  (\w _ y z -> DateTime w y z) <$> parseDate <*> dateSep <*> parseTime <*> parseUTC

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
parse4Digits = (\w x y z -> 1000 * w + 100 * x + 10 * y + z) <$> parseDigit <*> parseDigit <*> parseDigit <*> parseDigit

parseDigits :: Parser Char Int
parseDigits = (\x y -> 10 * x + y) <$> parseDigit <*> parseDigit

parseDigit :: Parser Char Int
parseDigit = f <$> satisfy isDigit
             where f c = ord c - ord '0'

dateSep :: Parser Char Char             
dateSep = symbol 'T'

-- Exercise 2
-- Straightforward: Open the file in readmode, set the newlinemode to notranslation, get the contents and try to recognize is as a calendar.
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar file = do f <- openFile file ReadMode
                       hSetNewlineMode f noNewlineTranslation
                       c <- hGetContents f
                       return (recognizeCalendar c)


-- Exercise 3
-- Nothing special happening, simply printing the needed strings.
printCalendar :: Calendar -> String
printCalendar (Calendar p e) = "BEGIN:VCALENDAR\r\nPRODID:" ++ p ++ "\r\nVERSION:2.0\r\n" ++ concatMap printEvent e ++ "END:VCALENDAR\r\n"

-- This prints an event, we need to check whether des, sum and loc actually contain anything, since they are of the Maybe type.
printEvent :: VEvent -> String
printEvent (VEvent stamp uid start end des sum loc) = "BEGIN:VEVENT\r\nDTSTAMP:" ++ printDateTime stamp ++
                                                      "\r\nUID:" ++ uid ++ 
                                                      "\r\nDTSTART:" ++ printDateTime start ++ 
                                                      "\r\nDTEND:" ++ printDateTime end ++ "\r\n" ++
                                                      showString "DESCRIPTION" des ++
                                                      showString "SUMMARY"     sum ++
                                                      showString "LOCATION"    loc ++
                                                      "END:VEVENT\r\n"
                                                      where
                                                      showString a b = case des of
                                                                            Nothing -> ""
                                                                            Just x  -> a ++ ":" ++ x ++ "\r\n"
                                                      

-- printDateTime was copied from Part 1. This is the commentary added there:
-- Printing is pretty straight forward. The only thing worth mentioning is addZeros. This function ensures that years, 
-- months, days, hours, minutes and seconds have an equal amount digits by adding 0's to the front, since the first month
-- must be represented as "01"
printDateTime :: DateTime -> String
printDateTime (DateTime dt t utc) = printDate dt ++ "T" ++ printTime t ++ op utc
        where op True  = "Z"
              op False = "" 

printDate :: Date -> String
printDate (Date y m d) = (addZeros 4.show.unYear) y ++ (addZeros 2.show.unMonth) m ++ (addZeros 2.show.unDay) d

printTime :: Time -> String
printTime (Time h m s) = (addZeros 2.show.unHour) h ++ (addZeros 2.show.unMinute) m ++ (addZeros 2.show.unSecond) s

addZeros :: Int -> String -> String
addZeros n s | n > length s = addZeros n ('0':s)
             | otherwise = s

-- Exercise 4

-- We simply count the length of [VEvents] in Calendar
countEvents :: Calendar -> Int
countEvents = length . events

-- Checks whether one or more of the events is happening at given DateTime
findEvents :: DateTime -> Calendar -> [VEvent]
findEvents dt (Calendar _ es) = filter (isHappening dt) es

-- Checks whether a given DateTime is in between the start and end DateTime of the given VEvent, using the Ord, Eq instances
isHappening :: DateTime -> VEvent -> Bool
isHappening dt VEvent{dtStart, dtEnd} = dt >= dtStart && dt <= dtEnd

-- Checks whether a given DateTime is in between the start and end DateTime of given VEvent, using the difference in time between them
inBetween :: DateTime -> VEvent -> Bool
inBetween dt VEvent{dtStart,dtEnd} = totalDiff dtStart dt >= 0 && totalDiff dt dtEnd >= 0

-- Checks whether there are any overlapping events. We require more hits than total events, since overlap returns at least 1 for each event.
checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ e) = overlap e e > length e

-- Recursive; for each event we check how many events are happening at that time. Since we check agains the entire list, we expect at least 1 hit (the event itself)
overlap :: [VEvent] -> [VEvent] -> Int
overlap [] _ = 0
overlap (VEvent{dtStart}:xs) ys = (length . filter id) (map (inBetween dtStart) ys) + overlap xs ys

-- Calculates the total time spent on events, in minutes
timeSpent :: String -> Calendar -> Int
timeSpent s (Calendar _ e) = (sum . map eventTime) (filter (filterEvent s) e)

-- Filters events matching the given summary
filterEvent :: String -> VEvent -> Bool
filterEvent s VEvent{summary} = case summary of
                                     Nothing -> False
                                     Just x  -> x == s

-- Calculates the time spent on a single event, in minutes                                    
eventTime :: VEvent -> Int
eventTime VEvent{dtStart,dtEnd} = totalDiff dtStart dtEnd

-- Calculates the total difference in time, by calculating the difference in Date and Time
-- We delegate the calculations to the lowest possible levels
totalDiff :: DateTime -> DateTime -> Int
totalDiff (DateTime bd bt _) (DateTime ed et _) = dateDiff bd ed + timeDiff bt et

dateDiff :: Date -> Date -> Int
dateDiff bd@(Date y1 _ d1) ed@(Date y2 _ d2) = yearDiff y1 y2 + monthDiff bd ed + dayDiff d1 d2

-- Equal years result in no time, otherwise we add the amount of minutes one year takes, and we recursively call the function whilst adding 1 to the beginyear
yearDiff :: Year -> Year -> Int
yearDiff y1@(Year y) y2 | y1 == y2    = 0
                        | leapYear y1 = rem 366
                        | otherwise   = rem 365
                        where rem n = n * 24 * 60 + yearDiff (Year (y + 1)) y2

-- Similar function to yearDiff, except that we now have to check whether the beginning month is earlier or later in the year.
-- When the beginning date has an earlier month, we can simply add the time one month takes, otherwise we have to substract.                        
monthDiff :: Date -> Date -> Int
monthDiff d@(Date y1 (Month m1) d1) (Date y2 (Month m2) d2) | m1 == m2  = 0
                                                            | m1 < m2   = temp 1
                                                            | otherwise = temp (-1)
                                                            where temp i = i * days d + monthDiff (Date y1 (Month (m1 + i)) d1) (Date y2 (Month m2) d2)

-- Equal to monthDiff, only different amount of minutes in a day                                                         
dayDiff :: Day -> Day -> Int
dayDiff (Day d1) (Day d2) | d1 == d2  = 0
                          | d1 < d2   = temp 1
                          | otherwise = temp (-1)
                          where temp i = i * 24 * 60 + dayDiff (Day (d1 + i)) (Day d2)

-- Again, delegation to the lowest possible level
timeDiff :: Time -> Time -> Int
timeDiff (Time h1 m1 _) (Time h2 m2 _) = hourDiff h1 h2 + minuteDiff m1 m2

-- Equal to dayDiff, only different amount of minutes in an hour                               
hourDiff :: Hour -> Hour -> Int
hourDiff (Hour h1) (Hour h2) | h1 == h2  = 0
                             | h1 < h2   = temp 1
                             | otherwise = temp (-1)
                             where temp i = i * 60 + hourDiff (Hour (h1 + i)) (Hour h2)

-- Again, equal to hourdiff, only we now simply add or substract the minutes                             
minuteDiff :: Minute -> Minute -> Int
minuteDiff (Minute m1) (Minute m2) | m1 == m2  = 0
                                   | m1 < m2   = temp 1
                                   | otherwise = temp (-1)
                                   where temp i = i + minuteDiff (Minute (m1 + i)) (Minute m2)

-- Checks whether a given year is a leapyear                                   
leapYear :: Year -> Bool
leapYear (Year y) | y `mod` 400 == 0 = True
                  | y `mod` 100 == 0 = False
                  | y `mod` 4   == 0 = True
                  | otherwise        = False

-- Calculates the days in a month
days :: Date -> Int
days (Date y (Month m) _) | m == 2 && leapYear y = 29
                          | m == 2 = 28
                          | m == 4 || m == 6 || m == 9 || m == 11 = 30
                          | otherwise = 31

-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> PP.Doc
ppMonth y m c = PP.text (eventsInMonthToString (days (Date y m (Day 1))) (eventsMonth y m c))

eventsInMonthToString :: Int -> [VEvent] -> String
eventsInMonthToString m xs = intercalate ppLine (zipLists (map (ppDayLine m) [7 * k - 6 | k <- [1..5]]) (eventsInWeekToString m 1 xs))

eventsInWeekToString :: Int -> Int -> [VEvent] -> [String]
eventsInWeekToString m d xs | d > m = []
                            | otherwise = ppEvent d (sortOnWeek m (toTuples xs) !! (d `div` 7)) : eventsInWeekToString m (d + 7) xs 

zipLists :: [String] -> [String] -> [String] 
zipLists [] [] = []
zipLists (x:xs) (y:ys) = (x ++ y) : zipLists xs ys

-- Prints the following ---------------+---------------+ etc.
ppLine :: String
ppLine = (tail . concat . replicate 7) ("+" ++ replicate 15 '-')

-- Prints the following  1             | 2             | etc.
-- Since ppDay adds | to the beginning of every day, we have to take the tail of the list (remove the first |)
ppDayLine :: Int -> Int -> String
ppDayLine m n | m < n     = ' ' : tail rem
              | otherwise = tail (concatMap ppDay [n .. x]) ++ rem
                       where
                        x = min (n + 6) m
                        y = n + 6 - x
                        rem = (concat . replicate y) ppEmptyDay

-- Prints a single day: | xx                                     
ppDay :: Int -> String
ppDay n = "| " ++ show n ++ replicate (14 - (length . show) n) ' '

-- Prints an empty day, to fill the calendar (days 32, 33 etc)
ppEmptyDay :: String
ppEmptyDay = "|" ++ replicate 15 ' '

-- Prints the time an event takes for the whole week. If there are no events (left) and we're about to begin a newLine, we return the empty string
-- Otherwise, we check whether there's an event on the given day, and if so we print the time. If there's no event we print an empty box. Then we move to the next day.
-- This function can also print the events for an entire month, it is however not used as such                  

ppEvent :: Int -> [(Int, String)] -> String
ppEvent n es | n `mod` 7 == 1 && null es = ""
             | otherwise = z ++ d ++ ppEvent m y
           where
             x = findIndex (\x -> n == fst x) es
             e = case x of
                    Nothing -> (0,"")
                    Just i  -> es !! i
             d | e == (0,"") && n `mod` 7 == 1 = "              "
               | e == (0,"") = "|              "
               | n `mod` 7 == 1 = " " ++ snd e ++ " "
               | otherwise = "| " ++ snd e ++ " "
             y = case x of
                    Nothing -> es
                    Just _  -> delete e es
             z | n `mod` 7 == 1 = "\r\n"
               | otherwise = ""
             m | n < 7 = (n `mod` 7) + 1
               | n `mod` 7 == 0 && null es = n `mod` 7 + (n `div` 7) * 7 + 1
               | n `mod` 7 == 0 && (fst . head) es <= n = n - 6
               | otherwise = n `mod` 7 + ((n `div` 7) * 7) + 1

-- Distribute the events among the weeks
sortOnWeek :: Int -> [(Int, String)] -> [[(Int, String)]]
sortOnWeek n xs = map (inWeek n xs) [7 * k - 6 | k <- [1..5]]

-- Filter all events that are not in a certain week
inWeek :: Int -> [(Int, String)] -> Int -> [(Int, String)]
inWeek n xs week = filter f xs
        where f (a,_) = a >= week && a < week + 7 && a <= n

-- Convert VEvents to tuples with the day and the string that is to be outputted in the Calendar
toTuples :: [VEvent] -> [(Int, String)]
toTuples = foldr ((++) . toTuple) []

toTuple :: VEvent -> [(Int, String)]
toTuple v | (fullunDay.dtEnd) v == (fullunDay.dtStart) v = [((fullunDay.dtStart) v,timeToString ((time.dtStart) v) ((time.dtEnd) v))]
          | otherwise                                    = ((fullunDay.dtStart) v, timeToString ((time.dtStart) v) (Time (Hour 23) (Minute 59) (Second 0))) : (toTuple . toBeginOfNextDay) v
          where fullunDay = unDay.day.date
          
toBeginOfNextDay :: VEvent -> VEvent
toBeginOfNextDay v@VEvent{dtStart=dt@DateTime{date=d@Date{day},time=t}} = v{dtStart = dt{date=d{day = Day (unDay day + 1)}, time=t{hour = Hour 0, minute = Minute 0}}}
                                                                                      
timeToString :: Time -> Time -> String
timeToString (Time h1 m1 _) (Time h2 m2 _) = (addZeros 2.show.unHour) h1++":"++ (addZeros 2.show.unMinute) m1 ++ " - " ++ (addZeros 2.show.unHour) h2 ++ ":" ++ (addZeros 2.show.unMinute) m2

-- Creates a list of events in the given Year and Month             
eventsMonth :: Year -> Month -> Calendar -> [VEvent]
eventsMonth y m (Calendar _ e) = filter (eventMonth y m) e

-- Checks whether an event is happening in given combinating of year + month
eventMonth :: Year -> Month -> VEvent -> Bool
eventMonth y m VEvent{dtStart = DateTime{date = Date y1 m1 _}, dtEnd = DateTime{date = Date y2 m2 _}} = (y1 == y && m1 == m) || (y2 == y && m2 == m)