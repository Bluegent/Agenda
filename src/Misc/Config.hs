module Misc.Config where

import System.Directory
import Data.Either.Utils
import Data.ConfigFile



contactsPath = "contacts_file"
appointmentsPath = "appointments_file"
pathsSection = "PATHS"
userCallout = "callout"


defaultCfg :: ConfigParser
defaultCfg = forceEither $ do
    let cfg = emptyCP
    cfg <- add_section cfg pathsSection
    cfg <- set cfg pathsSection contactsPath "docs/contacts.json"
    cfg <- set cfg pathsSection appointmentsPath "docs/appointments.json"
    cfg <- add_section cfg "MISC"
    cfg <- set cfg "MISC" userCallout "User"
    return cfg


readCfg :: FilePath -> IO ConfigParser
readCfg path = do
    res <- readfile emptyCP path
    let cp = forceEither res
    return cp


getCfgFromPath :: FilePath -> IO ConfigParser
getCfgFromPath path = do
    res <- doesFileExist path
    if res 
    then do
        cfg <- readCfg path
        return cfg
    else return defaultCfg


greetUser configParser = putStrLn $ "Hello, " ++ (forceEither $ get configParser "MISC" "callout") ++"!"

getConfigPath :: ConfigParser -> String -> String
getConfigPath cp opt = forceEither $ get cp "PATHS" opt