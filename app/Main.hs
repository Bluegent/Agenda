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




main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let defaultCfgPath = "docs/config.cfg" 
    
    readDateFromKeyBoard

    
    cfgParser <- getCfgFromPath defaultCfgPath
    greetUser cfgParser

    contacts <- parseContacts $ getConfigPath  cfgParser contactsPath
    
    appts <- parseAppointments $ getConfigPath  cfgParser appointmentsPath
    
    let db = Database {ctcs = contacts , appointments = appts}
    
    menuLoop db