import Data.ConfigFile
import Data.Either.Utils
import Agenda.Contact
import Agenda.Appointment
import Agenda.Utils
import Data.Aeson
import Data.Time
import Misc.Config
import Misc.Menu
import Control.Monad
import Control.Exception
import System.IO




localToUtc :: LocalTime -> IO UTCTime
localToUtc local = do
    timeZone <- getCurrentTimeZone
    return $ localTimeToUTC timeZone local

zonedToUtc :: ZonedTime -> UTCTime
zonedToUtc zoned = localTimeToUTC (zonedTimeZone zoned) (zonedTimeToLocalTime zoned)
    
utcToZoned :: UTCTime -> IO ZonedTime
utcToZoned utc = do
    timeZone <- getCurrentTimeZone
    let localTime = utcToLocalTime timeZone utc 
    return ZonedTime {zonedTimeToLocalTime = localTime, zonedTimeZone = timeZone}
    
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let defaultCfgPath = "docs/config.cfg" 
    
    case ( parseTimeM True defaultTimeLocale "%R" "13:20") :: Maybe LocalTime of 
        Just x -> do
            localTime <- localToUtc x
            putStrLn $ show localTime
            
        Nothing -> return ()
    
    
    cfgParser <- getCfgFromPath defaultCfgPath
    greetUser cfgParser

    contacts <- parseContacts $ getConfigPath  cfgParser contactsPath
    
    appts <- parseAppointments $ getConfigPath  cfgParser appointmentsPath
    
    let db = Database {ctcs = contacts , appointments = appts}
    
--    menuLoop db
    putStrLn "Goodbye"