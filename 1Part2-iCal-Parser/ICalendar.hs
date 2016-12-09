{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}


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

--instance Show DateTime where
--    show = printDateTime
 
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
    res <- readCalendar "examples/bastille.ics"
    putStrLn . PP.render $ maybe (PP.text "Calendar parsing error") (ppMonth (Year 2012) (Month 11)) res


--main = interact (\x -> (printCalendar.fst) ((parse parseCalendar x) !! 0))

-- Exercise 1
data Token = Token
    deriving (Eq, Ord, Show)

parseCalendar :: Parser Char Calendar
parseCalendar = Calendar <$ token "BEGIN:VCALENDAR\r\n" <*> parseCalProp <*> many parseEvent <* token "END:VCALENDAR\r\n" <* eof

{-toString :: String -> String
toString a = show (parse parseCalendar a)-}

parseCalProp :: Parser Char String
parseCalProp = parseVersion *> parseProdId <|> parseProdId <* parseVersion

parseVersion :: Parser Char ()
parseVersion = const () <$> token "VERSION:2.0\r\n"

parseProdId :: Parser Char String
parseProdId = token "PRODID:" *> parseToEnd

parseEvent :: Parser Char VEvent
parseEvent = (token "BEGIN:VEVENT\r\n" *> greedy parseProp <* token "END:VEVENT\r\n") >>= (f.sort)

f :: [Props] -> Parser Char VEvent
f xs =  case g xs of
            (Nothing, _) -> empty
            (Just x, ys) -> case h x ys of 
                                Nothing -> empty
                                Just y  -> succeed y

g :: [Props] -> (Maybe VEvent, [Props])
g (DtStamp dm : Uid u : DtStart ds : DtEnd de : xs) = (Just (VEvent dm u ds de Nothing Nothing Nothing), xs)
g _ = (Nothing, [])

h :: VEvent -> [Props] -> Maybe VEvent
h v xs = case xs of
              []                   -> Just v
              (Description de:xss) -> h1 v{description = Just de} xss
              _                    -> h1 v xs

h1 :: VEvent -> [Props] -> Maybe VEvent
h1 v xs = case xs of
               []               -> Just v
               (Summary su:xss) -> h2 v{summary = Just su} xss
               _                -> h2 v xs
                                   
h2 :: VEvent -> [Props] -> Maybe VEvent
h2 v xs = case xs of
               []            -> Just v
               [Location lo] -> Just v{location = Just lo}
               _             -> Nothing

parseProp :: Parser Char Props
parseProp = DtStamp     <$ token "DTSTAMP:"     <*> parseDTToEnd  <|>
            Uid         <$ token "UID:"         <*> parseToEnd    <|>
            DtStart     <$ token "DTSTART:"     <*> parseDTToEnd  <|>
            DtEnd       <$ token "DTEND:"       <*> parseDTToEnd  <|>
            Description <$ token "DESCRIPTION:" <*> parseToEnd    <|>
            Summary     <$ token "SUMMARY:"     <*> parseToEnd    <|>
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
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar file = do f <- openFile file ReadMode
                       hSetNewlineMode f noNewlineTranslation
                       c <- hGetContents f
                       return (recognizeCalendar c)


-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)


printCalendar :: Calendar -> String
printCalendar (Calendar p e) = "BEGIN:VCALENDAR\r\nPRODID:" ++ p ++ "\r\nVERSION:2.0\r\n" ++ concatMap printEvent e ++ "END:VCALENDAR\r\n"

printEvent :: VEvent -> String
printEvent (VEvent stamp uid start end des sum loc) = "BEGIN:VEVENT\r\nDTSTAMP:" ++ printDateTime stamp ++
                                                      "\r\nUID:" ++ uid ++ 
                                                      "\r\nDTSTART:" ++ printDateTime start ++ 
                                                      "\r\nDTEND:" ++ printDateTime end ++ "\r\n" ++
                                                      showdes ++
                                                      showsum ++
                                                      showloc ++
                                                      "END:VEVENT\r\n"
                                                      where
                                                      showdes = case des of
                                                                     Nothing -> ""
                                                                     Just x -> "DESCRIPTION:" ++ x ++ "\r\n"
                                                      showsum = case sum of
                                                                     Nothing -> ""
                                                                     Just x -> "SUMMARY:" ++ x ++ "\r\n"
                                                      showloc = case loc of
                                                                     Nothing -> ""
                                                                     Just x -> "LOCATION:" ++ x ++ "\r\n"
                                                      

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

-- Calendar "asfd" [(VEvent (DateTime (Date (Year 1) (Month 3) (Day 2)) (Time (Hour 23) (Minute 59) (Second 59)) True) "asd" (DateTime (Date (Year 1) (Month 3) (Day 2)) (Time (Hour 22) (Minute 59) (Second 59)) True) (DateTime (Date (Year 1) (Month 3) (Day 3)) (Time (Hour 0) (Minute 0) (Second 59)) True) Nothing Nothing Nothing),(VEvent (DateTime (Date (Year 1) (Month 3) (Day 2)) (Time (Hour 23) (Minute 59) (Second 59)) True) "asd" (DateTime (Date (Year 1) (Month 3) (Day 3)) (Time (Hour 0) (Minute 0) (Second 0)) True) (DateTime (Date (Year 1) (Month 3) (Day 3)) (Time (Hour 22) (Minute 59) (Second 59)) True) Nothing Nothing Nothing)]
-- Exercise 4
countEvents :: Calendar -> Int
countEvents = length . events

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents dt (Calendar _ e) = filter (inBetween dt) e

inBetween :: DateTime -> VEvent -> Bool
inBetween dt VEvent{dtStart,dtEnd} = totalDiff dtStart dt >= 0 && totalDiff dt dtEnd >= 0

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ e) = overlap e e > length e

overlap :: [VEvent] -> [VEvent] -> Int
overlap [] _ = 0
overlap (VEvent{dtStart}:xs) ys = length (filter id (map (inBetween dtStart) ys)) + overlap xs ys

timeSpent :: String -> Calendar -> Int
timeSpent s (Calendar _ e) = sum (map eventTime (filter (filterEvent s) e)) `div` 60

filterEvent :: String -> VEvent -> Bool
filterEvent s VEvent{summary} = case summary of
                                     Nothing -> False
                                     Just x  -> x == s

eventTime :: VEvent -> Int
eventTime VEvent{dtStart,dtEnd} = totalDiff dtStart dtEnd

totalDiff :: DateTime -> DateTime -> Int
totalDiff (DateTime bd bt _) (DateTime ed et _) = dateDiff bd ed + timeDiff bt et

dateDiff :: Date -> Date -> Int
dateDiff bd@(Date y1 _ d1) ed@(Date y2 _ d2) = yearDiff y1 y2 + monthDiff  bd ed + dayDiff d1 d2

yearDiff :: Year -> Year -> Int
yearDiff y1@(Year y) y2 | y1 == y2    = 0
                        | leapYear y1 = 366 * 24 * 3600 + yearDiff (Year (y+1)) y2
                        | otherwise   = 365 * 24 * 3600 + yearDiff (Year (y+1)) y2

monthDiff :: Date -> Date -> Int
monthDiff d@(Date y1 (Month m1) d1) (Date y2 (Month m2) d2) | m1 == m2  = 0
                                                            | m1 < m2   =  days d + monthDiff (Date y1 (Month (m1 + 1)) d1) (Date y2 (Month m2) d2)
                                                            | otherwise = -days d + monthDiff (Date y1 (Month (m1 - 1)) d1) (Date y2 (Month m2) d2)

dayDiff :: Day -> Day -> Int
dayDiff (Day d1) (Day d2) | d1 == d2  = 0
                          | d1 < d2   = 24 * 3600 + dayDiff (Day (d1 + 1)) (Day d2)
                          | otherwise = -(24 * 3600) + dayDiff (Day (d1 - 1)) (Day d2)

timeDiff :: Time -> Time -> Int
timeDiff (Time h1 m1 (Second s1)) (Time h2 m2 (Second s2)) = hourDiff h1 h2 + minuteDiff m1 m2 + (s2 - s1)
                                
hourDiff :: Hour -> Hour -> Int
hourDiff (Hour h1) (Hour h2) | h1 == h2  = 0
                             | h1 < h2   = 3600 + hourDiff (Hour (h1 + 1)) (Hour h2)
                             | otherwise = -3600 + hourDiff (Hour (h1 - 1)) (Hour h2)

minuteDiff :: Minute -> Minute -> Int
minuteDiff (Minute m1) (Minute m2) | m1 == m2  = 0
                                   | m1 < m2   = 60 + minuteDiff (Minute (m1 + 1)) (Minute m2)
                                   | otherwise = -60 + minuteDiff (Minute (m1 - 1)) (Minute m2)                                         
                                   
leapYear :: Year -> Bool
leapYear (Year y) | y `mod` 400 == 0 = True
                  | y `mod` 100 == 0 = False
                  | y `mod` 4   == 0 = True
                  | otherwise        = False

days :: Date -> Int
days (Date y (Month m) _) | m == 2 && leapYear y = 29
                          | m == 2 = 28
                          | m == 4 || m == 6 || m == 9 || m == 11 = 30
                          | otherwise = 31

-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> PP.Doc
ppMonth y m c = PP.text (ppMonth2 (days (Date y m (Day 1))) (eventsMonth y m c))

ppMonth2 :: Int -> [VEvent] -> String
ppMonth2 m xs = intercalate ppLine (zipLists (map (ppDayLine m) [7 * k - 6 | k <- [1..5]]) (op3 m 1 xs))

op3 :: Int -> Int -> [VEvent] -> [String]
op3 m d xs | d > m = []
           | otherwise = ppEvent d (sortOnWeek m (toTuples xs) !! (d `div` 7)) : op3 m (d + 7) xs 

zipLists :: [String] -> [String] -> [String] 
zipLists [] [] = []
zipLists (x:xs) (y:ys) = (x ++ y) : zipLists xs ys
--zipLists  _ [] = []

ppLine :: String
ppLine = tail (concat (replicate 7 ("+" ++ replicate 14 '-')))

ppDayLine :: Int -> Int -> String
ppDayLine m n | m < n     = ' ' : tail (concat (replicate y ppEmptyDay))
              | otherwise = tail (concatMap ppDay [n .. x]) ++ concat (replicate y ppEmptyDay)
                       where
                        x = min (n+6) m
                        y = n + 6 - x

ppDay :: Int -> String
ppDay n = "| " ++ show n ++ replicate (14 - length (show n)) ' '

ppEmptyDay :: String
ppEmptyDay = "|" ++ replicate 14 ' '
                  
{-ppEvent :: Int -> [(Int, String)] -> String
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
                    Nothing -> delete e es
                    Just _  -> es
           z | n `mod` 7 == 1 = "\r\n"
             | otherwise = ""
           m | n < 7 = (n `mod` 7) + 1
             | n `mod` 7 == 0 && null es = n `mod` 7 + ((n `div` 7) * 7) + 1
             | n `mod` 7 == 0 && fst (head es) <= n = n - 7 + 1
             | otherwise = n `mod` 7 + ((n `div` 7) * 7) + 1-}

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
               | n `mod` 7 == 0 && null es = n `mod` 7 + ((n `div` 7) * 7) + 1
               | n `mod` 7 == 0 && fst (head es) <= n = n - 7 + 1
               | otherwise = n `mod` 7 + ((n `div` 7) * 7) + 1

sortOnWeek :: Int -> [(Int, String)] -> [[(Int, String)]]
sortOnWeek n xs = map (op n xs) [7*k-6 | k <- [1..5]]

op :: Int -> [(Int, String)] -> Int -> [(Int, String)]
op n xs week = filter f xs
        where f (a,_) = a >= week && a < week + 7 && a <= n

toTuples :: [VEvent] -> [(Int, String)]
toTuples = foldr ((++) . toTuple) []

toTuple :: VEvent -> [(Int, String)]
toTuple v | (unDay.day.date.dtEnd) v == (unDay.day.date.dtStart) v = [((unDay.day.date.dtStart) v,timeToString ((time.dtStart) v) ((time.dtEnd) v))]
          | otherwise                        = ((unDay.day.date.dtStart) v, timeToString ((time.dtStart) v) (Time (Hour 23) (Minute 59) (Second 0))) : (toTuple . toBeginOfNextDay) v

toBeginOfNextDay :: VEvent -> VEvent
toBeginOfNextDay v@VEvent{dtStart=dt@DateTime{date=d@Date{day},time=t}} = v{dtStart = dt{date=d{day = Day (unDay day + 1)}, time=t{hour = Hour 0, minute = Minute 0}}}
                                                                                      
timeToString :: Time -> Time -> String
timeToString (Time h1 m1 _) (Time h2 m2 _) = (addZeros 2.show.unHour) h1++":"++ (addZeros 2.show.unMinute) m1 ++ " - " ++ (addZeros 2.show.unHour) h2 ++ ":" ++ (addZeros 2.show.unMinute) m2
             
eventsMonth :: Year -> Month -> Calendar -> [VEvent]
eventsMonth y m (Calendar _ e) = filter (eventMonth y m) e

eventMonth :: Year -> Month -> VEvent -> Bool
eventMonth y m VEvent{dtStart = DateTime{date = Date y1 m1 _}, dtEnd = DateTime{date = Date y2 m2 _}} = (y1 == y && m1 == m) || (y2 == y && m2 == m)