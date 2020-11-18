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
import Data.Typeable

parseContactsWithEx :: FilePath -> IO [Contact]
parseContactsWithEx path = do
    res <- try ( parseContacts path) :: IO (Either SomeException [Contact])
    case res of 
        Left ex -> do
            putStrLn "A problem occurred while parsing contacts. Either the file does not exist or the program does not have proper access to it. Starting with an empty contact list."
            return []
        Right ctc -> return ctc
            
            
parseAppointmentsWithEx :: FilePath -> IO [Appointment]
parseAppointmentsWithEx path = do
    res <- try ( parseAppointments path) :: IO (Either SomeException [Appointment])
    case res of 
        Left ex -> do
            putStrLn "A problem occurred while parsing appointments. Either the file does not exist or the program does not have proper access to it. Starting with an empty appointment list."
            return []
        Right appts -> return appts
 
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let defaultCfgPath = "docs/config.cfg" 
         
    cfgParser <- getCfgFromPath defaultCfgPath
    greetUser cfgParser    
    
    parsedContacts <- parseContactsWithEx $ getConfigPath  cfgParser contactsPath
    parsedAppts <- parseAppointmentsWithEx $ getConfigPath  cfgParser appointmentsPath
    
    let db = Database {contacts = parsedContacts , appointments = parsedAppts , cfg = cfgParser}
    
    menuLoop db
    putStrLn "Goodbye"