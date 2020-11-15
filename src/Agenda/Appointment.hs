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

data Appointment =
  Appointment { name :: String
        , startDate :: String
        , endDate :: String
        , details :: String
        } deriving (Show,Generic)

instance FromJSON Appointment
instance ToJSON Appointment

writeAppointmentList :: FilePath  -> [Appointment] -> IO()
writeAppointmentList path appts = B.writeFile path (encode appts)

parseDate :: String -> LocalTime
parseDate str = parseTimeOrError True defaultTimeLocale "%d/%m/%0Y %R" str :: LocalTime


printAppointment :: Appointment -> IO()
printAppointment appt = putStrLn $ name appt ++ " (" ++ show (parseDate (startDate appt)) ++" - "++ show (parseDate (endDate appt)) ++ ") - \"" ++ details appt ++ "\""  


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
    forM_ list $ \contact -> do
        printMatch func contact term



parseDateMaybe ::  String -> Maybe LocalTime
parseDateMaybe str = do
    case ( parseTimeM True defaultTimeLocale "%d/%m/%0Y %R" str) :: Maybe LocalTime of 
        Just x -> return x
        Nothing -> do
            case ( parseTimeM True defaultTimeLocale "%d/%m/%0Y" str) :: Maybe LocalTime of 
                Just x -> return x
                Nothing -> do
                    case ( parseTimeM True defaultTimeLocale "%R" str) :: Maybe LocalTime of 
                        Just x -> return x
                        Nothing -> Nothing
    
        
readDateFromKeyBoard :: IO ()
readDateFromKeyBoard = do
    putStrLn "Accepted date formats are \"dd/mm/yy hh:mm\" or \"mm/dd/yyyy\" or \"hh:mm\"."
    putStr "Enter date:"
    line <- getLine
    case (parseDateMaybe line) :: Maybe LocalTime of 
        Just time -> putStrLn $ show time
        Nothing -> putStrLn "Invalid input."

    