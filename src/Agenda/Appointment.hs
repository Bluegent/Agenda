{-# LANGUAGE DeriveGeneric #-}
module Agenda.Appointment where

-- import as B to avoid name clash between Prelude's ByteString and Data.ByteString
import qualified Data.ByteString.Lazy as B
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import GHC.Generics
import Data.Time
import Agenda.Utils


localToUtc :: LocalTime -> IO UTCTime
localToUtc local = do
    timeZone <- getCurrentTimeZone
    return $ localTimeToUTC timeZone local

zonedToUtc :: ZonedTime -> UTCTime
zonedToUtc zoned = localTimeToUTC (zonedTimeZone zoned) (zonedTimeToLocalTime zoned)

utcToLocal :: UTCTime -> IO LocalTime
utcToLocal utc = do
    timeZone <- getCurrentTimeZone
    return $ utcToLocalTime timeZone utc 

utcToZoned :: UTCTime -> IO ZonedTime
utcToZoned utc = do
    timeZone <- getCurrentTimeZone
    localTime <- utcToLocal utc
    return ZonedTime {zonedTimeToLocalTime = localTime, zonedTimeZone = timeZone}

dateIsInRange :: UTCTime -> UTCTime -> UTCTime -> Bool
dateIsInRange date start end = date >= start && date <= end 


data Appointment =
  Appointment { name :: String
        , startDate :: UTCTime
        , endDate :: UTCTime
        , details :: String
        } deriving (Show,Generic)

instance FromJSON Appointment
instance ToJSON Appointment

writeAppointmentList :: FilePath  -> V.Vector Appointment -> IO()
writeAppointmentList path appts = B.writeFile path (encode appts)

parseDate :: String -> LocalTime
parseDate str = parseTimeOrError True defaultTimeLocale "%d/%m/%0Y %R" str :: LocalTime


printAppointment :: Appointment -> IO()
printAppointment appt = do
    localStart <- utcToLocal (startDate appt)
    localEnd <- utcToLocal (endDate appt) 
    putStrLn $ name appt ++ " (" ++ show localStart ++" - "++  show localEnd ++ ") - \"" ++ details appt ++ "\""  


printAppointmentList :: V.Vector Appointment -> IO()
printAppointmentList list =  do 
    putStrLn "Your appointments are:"
    forM_ list $ \s -> do
        printAppointment s
        
        
parseAppointments :: FilePath -> IO([Appointment])
parseAppointments path = do
    res <- (eitherDecode <$> pathToString path) :: IO (Either String [Appointment])
    case res of
        Left err -> do 
            putStrLn $ "Error while parsing appointments:\"" ++ err ++"\""
            return []
        Right appts -> return appts
        
        
printMatch :: (Appointment -> String) -> Int-> Appointment  ->  String -> IO()
printMatch func index appt term = do
    let termLower = lowerString term
    let funcLower = lowerString (func appt)
    if L.isInfixOf termLower funcLower
        then do 
            putStr $ "[id:" ++ show index ++ "]"
            printAppointment appt
    else return ()



searchApptByString :: V.Vector Appointment -> (Appointment -> String) -> String -> IO()
searchApptByString list func term = do
    let map = \ index appt -> printMatch func index appt term
    V.imapM_ map list

printRangeMatch :: Appointment -> Int -> UTCTime -> UTCTime -> IO()
printRangeMatch appt index start end = do
    if dateIsInRange (startDate appt) start end
        then do 
            putStr $ "[id:" ++ show index ++ "]"
            printAppointment appt
    else return ()


searchApptByDateRange :: V.Vector Appointment -> UTCTime -> UTCTime -> IO()
searchApptByDateRange list start end = do
    let map = \ index appt -> printRangeMatch appt index start end
    V.imapM_ map list



printDateMatch :: Appointment -> Int -> UTCTime -> IO()
printDateMatch appt index time = do
    if startDate appt == time || endDate appt == time
        then do 
            putStr $ "[id:" ++ show index ++ "]"
            printAppointment appt
    else return ()

searchApptByExactDate :: V.Vector Appointment -> UTCTime -> IO()
searchApptByExactDate list time = do 
    let map = \ index appt -> printDateMatch appt index time
    V.imapM_ map list
        
getFirst (a,_,_) = a
getSecond (_,a,_) = a
getThird (_,_,a) = a

parseDateMaybe ::  String -> LocalTime -> Maybe LocalTime
parseDateMaybe str baseDate = do
    case ( parseTimeM True defaultTimeLocale "%d/%m/%0Y %R" str) :: Maybe LocalTime of 
        Just x -> return x
        Nothing -> do
            case ( parseTimeM True defaultTimeLocale "%d/%m/%0Y" str) :: Maybe LocalTime of 
                Just x -> return x
                Nothing -> do
                    case ( parseTimeM True defaultTimeLocale "%R" str) :: Maybe TimeOfDay of 
                        Just x -> do
                            let day = localDay baseDate
                            let localWithHour = LocalTime {localDay = day, localTimeOfDay = x}
                            return localWithHour
                        Nothing -> do
                            case ( parseTimeM True defaultTimeLocale "%d/%m" str) :: Maybe Day of 
                                Just x -> do
                                    let yearGreg = getFirst (toGregorian (localDay baseDate))
                                    let dayGreg = toGregorian x
                                    let day = fromGregorian yearGreg (getSecond dayGreg) (getThird dayGreg)
                                    let localWithYear = LocalTime {localDay = day, localTimeOfDay = midnight}
                                    return localWithYear
                                Nothing -> Nothing
   
readDate :: IO UTCTime
readDate = do
    line <- getLine
    current <- getCurrentTime
    baseDate <- utcToLocal current
    case (parseDateMaybe line baseDate) :: Maybe LocalTime of 
        Just time -> localToUtc time
        Nothing -> do
            putStrLn "Invalid date"
            readDate
   
readAppointment :: IO Appointment
readAppointment = do
    putStr "Input name:"
    nameStr <- getLine
    putStrLn "Accepted date formats are \"dd/mm/yy hh:mm\" or \"dd/mm/yyyy\" or \"dd/mm\"(will use current year) or \"hh:mm\"(will use current date)."
    putStr "Enter start date:"
    utcStart <- readDate
    putStr "Enter end date:"
    utcEnd <- readDate
    putStr "Input details:"
    detailsStr <- getLine
    return  Appointment { name = nameStr, details = detailsStr, startDate = utcStart, endDate = utcEnd}
    