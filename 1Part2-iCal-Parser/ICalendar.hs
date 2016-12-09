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

instance Show DateTime where
    show = printDateTime
 
data Props = DtStamp DateTime
           | Uid String
           | DtStart DateTime
           | DtEnd DateTime
           | Description String
           | Summary String
           | Location String
           deriving (Ord, Show, Eq)
           
run :: Parser a b -> [a] -> Maybe b
run p s = listToMaybe [p | (p, []) <- parse p s]

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run parseCalendar s


-- "Main" block, DO NOT EDIT.
-- If you want to run the parser + pretty-printing, rename this module (first line) to "Main".
-- DO NOT forget to rename the module back to "ICalendar" before submitting to DomJudge.
main = do
    res <- readCalendar "examples/rooster_infotc.ics"
    putStrLn . PP.render $ maybe (PP.text "Calendar parsing error") (ppMonth (Year 2012) (Month 11)) res


-- Exercise 1
data Token = Token
    deriving (Eq, Ord, Show)

parseCalendar :: Parser Char Calendar
parseCalendar = (\r xs -> (Calendar r xs)) <$ token "BEGIN:VCALENDAR\n" <*> parseCalProp <*> many parseEvent <* token "END:VCALENDAR\n" <* eof

{-toString :: String -> String
toString a = show (parse parseCalendar a)-}

parseCalProp :: Parser Char String
parseCalProp = parseVersion *> parseProdId <|> parseProdId <* parseVersion

parseVersion :: Parser Char ()
parseVersion = const () <$> token "VERSION:2.0\n"

parseProdId :: Parser Char String
parseProdId = token "PRODID:" *> parseToEnd

parseEvent :: Parser Char VEvent
parseEvent = (token "BEGIN:VEVENT\n" *> greedy parseProp <* token "END:VEVENT\n") >>= (f.sort)

f :: [Props] -> Parser Char VEvent
f xs =  case g xs of
            Nothing -> empty
            Just x -> case h x (drop 4 xs) of 
                        Nothing -> empty
                        Just y  -> succeed y

g :: [Props] -> Maybe VEvent
g ((DtStamp dm):(Uid u):(DtStart ds):(DtEnd de):xs) = Just (VEvent dm u ds de Nothing Nothing Nothing)
g _ = Nothing

h :: VEvent -> [Props] -> Maybe VEvent
h v@(VEvent a b c d e f g) xs = case xs of
                                   [] -> Just v
                                   (Description de:xss) -> h1 (VEvent a b c d (Just de) f g) xss
                                   _ -> h1 v xs

h1 :: VEvent -> [Props] -> Maybe VEvent
h1 v@(VEvent a b c d e f g) xs = case xs of
                                   [] -> Just v
                                   (Summary su:xss) -> h2 (VEvent a b c d e (Just su) g) xss
                                   _ -> h2 v xs
                                   
h2 :: VEvent -> [Props] -> Maybe VEvent
h2 v@(VEvent a b c d e f g) xs = case xs of
                                   [] -> Just v
                                   (Location lo:[]) -> Just (VEvent a b c d e f (Just lo))
                                   _ -> Nothing

parseProp :: Parser Char Props
parseProp = DtStamp     <$ token "DTSTAMP:"     <*> parseDTToEnd  <|>
            Uid         <$ token "UID:"         <*> parseToEnd    <|>
            DtStart     <$ token "DTSTART:"     <*> parseDTToEnd  <|>
            DtEnd       <$ token "DTEND:"       <*> parseDTToEnd  <|>
            Description <$ token "DESCRIPTION:" <*> parseToEnd    <|>
            Summary     <$ token "SUMMARY:"     <*> parseToEnd    <|>
            Location    <$ token "LOCATION:"    <*> parseToEnd
            where parseDTToEnd = parseDateTime <* symbol '\n'
            

parseToEnd :: Parser Char String
parseToEnd = greedy (satisfy (/='\n')) <* symbol '\n'

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
{-data Calendar = Calendar { prodId :: String
                         , events :: [VEvent] }
    deriving Eq

data VEvent = VEvent { dtStamp     :: DateTime
                     , uid         :: String
                     , dtStart     :: DateTime
                     , dtEnd       :: DateTime
                     , description :: Maybe String
                     , summary     :: Maybe String
                     , location    :: Maybe String }
    deriving Eq-}

printCalendar :: Calendar -> String
printCalendar (Calendar p e) = "BEGIN:VCALENDAR\r\nPRODID:" ++ p ++ "\r\nVERSION:2.0\r\n" ++ concat (map printEvent e) ++ "END:VCALENDAR\r\n"

printEvent :: VEvent -> String
printEvent (VEvent stamp uid start end des sum loc) = "BEGIN:VEVENT\r\nDTSTAMP:" ++ show stamp ++
                                                      "\r\nUID:" ++ uid ++ 
                                                      "\r\nDTSTART:" ++ show start ++ 
                                                      "\r\nDTEND:" ++ show end ++ "\r\n" ++
                                                      showdes ++
                                                      showsum ++
                                                      showloc ++
                                                      "END:VEVENT\r\n"
                                                      where
                                                      showdes | isNothing des = ""
                                                              | otherwise = "DESCRIPTION:" ++ fromJust des ++ "\r\n"
                                                      showsum | isNothing sum = ""
                                                              | otherwise = "SUMMARY:" ++ fromJust sum ++ "\r\n"
                                                      showloc | isNothing loc = ""
                                                              | otherwise = "LOCATION:" ++ fromJust loc ++ "\r\n"
                                                      

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
countEvents :: Calendar -> Int
countEvents = length . events

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents dt (Calendar _ e) = filter (inBetween dt) e

inBetween :: DateTime -> VEvent -> Bool
inBetween dt (VEvent _ _ start end _ _ _) = totalDiff start dt >= 0 && totalDiff dt end > 0

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ e) = overlap e e > length e

overlap :: [VEvent] -> [VEvent] -> Int
overlap [] xs = 0
overlap ((VEvent _ _ start _ _ _ _):xs) ys = length (filter id (map (inBetween start) ys)) + overlap xs ys

timeSpent :: String -> Calendar -> Int
timeSpent s (Calendar _ e) = (foldr (+) 0 (map eventTime (filter (filterEvent s) e))) `div` 60

filterEvent :: String -> VEvent -> Bool
filterEvent s (VEvent _ _ _ _ _ summ _) = s == fromJust summ

eventTime :: VEvent -> Int
eventTime (VEvent _ _ start end _ _ _) = totalDiff start end

totalDiff :: DateTime -> DateTime -> Int
totalDiff (DateTime bd bt _) (DateTime ed et _) = dateDiff bd ed + timeDiff bt et

dateDiff :: Date -> Date -> Int
dateDiff bd@(Date y1 m1 d1) ed@(Date y2 m2 d2) = yearDiff y1 y2 + monthDiff  bd ed + dayDiff d1 d2

yearDiff :: Year -> Year -> Int
yearDiff y1@(Year y) y2 | y1 == y2    = 0
                              | leapYear y1 = 366 * 24 * 3600 + yearDiff (Year (y+1)) y2
                              | otherwise   = 365 * 24 * 3600 + yearDiff (Year (y+1)) y2

monthDiff :: Date -> Date -> Int
monthDiff d@(Date y1 (Month m1) d1) (Date y2 (Month m2) d2) | m1 == m2 = 0
                                                                  | m1 < m2  = days d + monthDiff (Date y1 (Month (m1 + 1)) d1) (Date y2 (Month m2) d2)
                                                                  | m1 > m2  = -days d + monthDiff (Date y1 (Month (m1 - 1)) d1) (Date y2 (Month m2) d2)

dayDiff :: Day -> Day -> Int
dayDiff (Day d1) (Day d2) | d1 == d2 = 0
                                | d1 < d2  = 24 * 3600 + dayDiff (Day (d1 + 1)) (Day d2)
                                | d1 > d2  = -(24 * 3600) + dayDiff (Day (d1 - 1)) (Day d2)

timeDiff :: Time -> Time -> Int
timeDiff (Time h1 m1 (Second s1)) (Time h2 m2 (Second s2)) = hourDiff h1 h2 + minuteDiff m1 m2 + (s2 - s1)
                                
hourDiff :: Hour -> Hour -> Int
hourDiff (Hour h1) (Hour h2) | h1 == h2 = 0
                                   | h1 < h2  = 3600 + hourDiff (Hour (h1 + 1)) (Hour h2)
                                   | h1 > h2  = -3600 + hourDiff (Hour (h1 - 1)) (Hour h2)

minuteDiff :: Minute -> Minute -> Int
minuteDiff (Minute m1) (Minute m2) | m1 == m2 = 0
                                         | m1 < m2  = 60 + minuteDiff (Minute (m1 + 1)) (Minute m2)
                                         | m1 > m2  = -60 + minuteDiff (Minute (m1 - 1)) (Minute m2)                                         
                                   
leapYear :: Year -> Bool
leapYear (Year y) | y `mod` 400 == 0 = True
                  | y `mod` 100 == 0 = False
                  | y `mod` 4   == 0 = True
                  | otherwise        = False

days :: Date -> Int
days (Date y (Month m) d) | m == 2 && leapYear y = 29
                          | m == 2 = 28
                          | m == 4 || m == 6 || m == 9 || m == 11 = 30
                          | otherwise = 31

-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> PP.Doc
ppMonth y m c = undefined

ppLine :: String
ppLine = tail (concat (replicate 7 ("+" ++ replicate 14 '-')))

ppDayLine :: Int -> Int -> String
ppDayLine m n = tail (concat (map ppDay [n..x])) ++ concat (replicate y ppEmptyDay)
              where
              x = min (n+6) m
              y = n + 6 - x

ppDay :: Int -> String
ppDay n = "| " ++ show n ++ replicate (14 - length (show n)) ' '

ppEmptyDay :: String
ppEmptyDay = "|" ++ replicate (14) ' '

ppEmptyCalendar :: Int -> PP.Doc
ppEmptyCalendar n = undefined
                  where 
                  x = n `div` 7
                  f y = y * 7 + 1
                  d = map f [0..x]
                  dayLines = map (ppDayLine n) d
                  lineBreaks = replicate (x - 1) ppLine
                  
ppEvent :: Int -> [(Int, String)] -> String
ppEvent n es | n `mod` 7 == 1 && es == [] = ""
             | otherwise = z ++ d ++ ppEvent m y
           where
           x = findIndex (\x -> n == fst x) es
           e | x /= Nothing = es !! (fromJust x)
             | otherwise = (0,"")
           d | e == (0,"") && n `mod` 7 == 1 = "              "
             | e == (0,"") = "|              "
             | n `mod` 7 == 1 = " " ++ snd e ++ " "
             | otherwise = "| " ++ snd e ++ " "
           y | x /= Nothing = delete e es  
             | otherwise = es
           z | n `mod` 7 == 1 = "\r\n"
             | otherwise = ""
           m | n < 7 = (n `mod` 7) + 1
             | n `mod` 7 == 0 && es == [] = n `mod` 7 + ((n `div` 7) * 7) + 1
             | n `mod` 7 == 0 && fst (head es) <= n = n - 7 + 1
             | otherwise = n `mod` 7 + ((n `div` 7) * 7) + 1


eventsMonth :: Year -> Month -> Calendar -> [VEvent]
eventsMonth y m (Calendar _ e) = filter (eventMonth y m) e

eventMonth :: Year -> Month -> VEvent -> Bool
eventMonth y m (VEvent _ _ (DateTime (Date y1 m1 d1) t1 u1) (DateTime (Date y2 m2 d2) t2 u2) _ _ _) = (y1 == y && m1 == m) || (y2 == y && m2 == m)