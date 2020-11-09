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



main :: IO ()
main = do
    let defaultCfgPath = "docs/config.cfg" 
    
    cfgParser <- getCfgFromPath defaultCfgPath
    greetUser cfgParser

    contacts <- parseContacts $ getConfigPath  cfgParser "contacts_file"
    printContactList contacts
    
    appts <- parseAppointments $ getConfigPath  cfgParser "appointments_file"
    printAppointmentList appts
    
    let loop = do 
            eval <- welcomeMenu
            when (eval == 0) loop
    loop