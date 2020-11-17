{-# LANGUAGE DeriveGeneric #-}
module Agenda.Appointment where

-- import as B to avoid name clash between Prelude's ByteString and Data.ByteString
import qualified Data.ByteString.Lazy as B
import qualified Data.List as L
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
dateIsInRange date start end = date > start && date < end 


data Appointment =
  Appointment { name :: String
        , startDate :: UTCTime
        , endDate :: UTCTime
        , details :: String
        } deriving (Show,Generic)

instance FromJSON Appointment
instance ToJSON Appointment

writeAppointmentList :: FilePath  -> [Appointment] -> IO()
writeAppointmentList path appts = B.writeFile path (encode appts)

parseDate :: String -> LocalTime
parseDate str = parseTimeOrError True defaultTimeLocale "%d/%m/%0Y %R" str :: LocalTime


printAppointment :: Appointment -> IO()
printAppointment appt = do
    localStart <- utcToLocal (startDate appt)
    localEnd <- utcToLocal (endDate appt) 
    putStrLn $ name appt ++ " (" ++ show localStart ++" - "++  show localEnd ++ ") - \"" ++ details appt ++ "\""  


printAppointmentList :: [Appointment] -> IO()
printAppointmentList list =  do 
    putStrLn "Your appointments are:"
    forM_ list $ \s -> do
        printAppointment s
        
        
parseAppointments :: FilePath -> IO([Appointment])
parseAppointments path = do
    res <- (eitherDecode <$> pathToString path) :: IO (Either String [Appointment])
    case res of
        Left err -> do 
            putStrLn err
            return []
        Right appts -> return appts
        
        
printMatch :: (Appointment -> String) -> Appointment  ->  String -> IO()
printMatch func appt term = do
    let termLower = lowerString term
    let funcLower = lowerString (func appt)
    if L.isInfixOf termLower funcLower
        then printAppointment appt
    else return ()



searchApptByString :: [Appointment] -> (Appointment -> String) -> String -> IO()
searchApptByString list func term = do 
    forM_ list $ \appt -> do
        printMatch func appt term


printDateMatch :: Appointment -> UTCTime -> IO()
printDateMatch appt time = do
    if startDate appt == time || endDate appt == time
        then printAppointment appt
    else return ()

searchApptByExactDate :: [Appointment] -> UTCTime -> IO()
searchApptByExactDate list time = do 
    forM_ list $ \appt -> do
        printDateMatch appt time


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
                        Nothing -> Nothing
    

    