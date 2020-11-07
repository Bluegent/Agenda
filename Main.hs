import Data.ConfigFile
import Data.Either.Utils
import Contact
import AgendaUtils
import Data.Aeson

getConfigPath :: ConfigParser -> String -> String
getConfigPath cp opt = forceEither $ get cp "PATHS" opt

main :: IO ()
main = do
    putStrLn "Starting up." 
    val <- readfile emptyCP "docs/config.cfg"
    let configParser = forceEither val
    
    d <- (eitherDecode <$> pathToString (getConfigPath  configParser "contacts_file")) :: IO (Either String [Contact])
    case d of
        Left err -> putStrLn err
        Right ps -> printContactList ps

  