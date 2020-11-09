module Misc.Config where

import System.Directory
import Data.Either.Utils
import Data.ConfigFile

defaultCfg :: ConfigParser
defaultCfg = forceEither $ do
    let cfg = emptyCP
    cfg <- add_section cfg "PATHS"
    cfg <- set cfg "PATHS" "contacts_file" "docs/contacts.json"
    cfg <- set cfg "PATHS" "appointments_file" "docs/appointments.json"
    cfg <- add_section cfg "MISC"
    cfg <- set cfg "MISC" "callout" "User"
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