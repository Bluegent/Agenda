import Data.ConfigFile
import Data.Either.Utils
import Agenda.Contact
import Agenda.Appointment
import Agenda.Utils
import Data.Aeson
import Data.Time
import Misc.Config
import Misc.Menu

main :: IO ()
main = do
    val <- readfile emptyCP "docs/config.cfg"
    let configParser = forceEither val
    greetUser configParser

    d <- (eitherDecode <$> pathToString (getConfigPath  configParser "contacts_file")) :: IO (Either String [Contact])
    case d of
        Left err -> putStrLn err
        Right ps -> printContactList ps
        
    r <- (eitherDecode <$> pathToString (getConfigPath  configParser "appointments_file")) :: IO (Either String [Appointment])
    case r of
        Left err -> putStrLn err
        Right ps -> printAppointmentList ps
    dummyMenu