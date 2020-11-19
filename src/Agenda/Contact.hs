{-# LANGUAGE DeriveGeneric #-}
module Agenda.Contact where

-- import as B to avoid name clash between Prelude's ByteString and Data.ByteString
import qualified Data.ByteString.Lazy as B
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import GHC.Generics
import Agenda.Utils
import Data.Char
import Text.Regex.TDFA


data Contact =
  Contact { name     :: String
          , phone    :: String
          , email    :: String
          } deriving (Show,Generic)

instance FromJSON Contact
instance ToJSON Contact

writeContactList :: FilePath -> V.Vector Contact -> IO()
writeContactList path people = B.writeFile path (encode people)


printContact :: Contact -> IO()
printContact c =  putStrLn $ name c ++ "(tel:"++phone c ++", mail:"++ email c ++")" 

printContactWithIndex :: Int -> Contact -> IO()
printContactWithIndex index contact = do
    putStr $ "[id:" ++ show index ++ "]"
    printContact contact


parseContacts :: FilePath -> IO([Contact]) 
parseContacts path = do
    res <- (eitherDecode <$> pathToString path) :: IO (Either String [Contact])
    case res of
        Left err -> do
            putStrLn $ "Error while parsing contacts:\"" ++ err ++"\""
            return []
        Right contacts -> return contacts


printMatch :: (Contact -> String) -> Int -> Contact  ->  String -> IO()
printMatch func index contact term = do
    let termLower = lowerString term
    let funcLower = lowerString (func contact)
    if L.isInfixOf termLower funcLower
        then do
           printContactWithIndex index contact           
    else return ()


searchContactByString :: V.Vector Contact -> (Contact -> String) -> String -> IO()
searchContactByString list func term = do
    let map = \ index contact -> printMatch func index contact term
    V.imapM_ map list

emailRegex = "^[a-zA-Z0-9+._-]+@[a-zA-Z0-9.-]+[a-zA-Z0-9]{2,4}$"
phoneRegex = "^[+]{0,1}[0-9]{0,5}[\\ \\./0-9]*$"

readPhone :: IO String
readPhone = do
    putStr "Input phone:"
    line <- getLine
    if line =~ phoneRegex :: Bool then return line
    else do
        putStrLn "Invalid phone."
        readPhone

readEmail :: IO String
readEmail = do
    putStr "Input e-mail:"
    line <- getLine
    if line =~ emailRegex :: Bool then return line
    else do
        putStrLn "Invalid e-mail."
        readEmail

readContact :: IO Contact
readContact = do
    putStr "Input name:"
    nameStr <- getLine
    phoneStr <- readPhone
    mailStr <- readEmail
    return  Contact { name = nameStr, phone = phoneStr, email = mailStr}

printContacts :: V.Vector Contact -> IO()
printContacts list = do
    putStrLn "\nContact list:"
    V.imapM_ printContactWithIndex list