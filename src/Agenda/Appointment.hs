{-# LANGUAGE DeriveGeneric #-}
module Agenda.Appointment where

-- import as B to avoid name clash between Prelude's ByteString and Data.ByteString
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import GHC.Generics
import Data.Time

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


printAppointmentList :: [Appointment] -> IO()
printAppointmentList list =  do 
    putStrLn "Your appointments are:"
    forM_ list $ \s -> do
        putStrLn $ name s ++ " (" ++ show (parseDate (startDate s)) ++ ")"
