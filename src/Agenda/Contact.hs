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


printContactList :: [Contact] -> IO()
printContactList list = do 
    putStrLn "Your Loaded contacts are:"
    forM_ list $ \contact -> do
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
            putStr $ "[id:" ++ show index ++ "]"
            printContact contact
            
    else return ()


searchContactByString :: V.Vector Contact -> (Contact -> String) -> String -> IO()
searchContactByString list func term = do
    let map = \ index contact -> printMatch func index contact term
    V.imapM_ map list
        
        
readContact :: IO Contact
readContact = do
    putStr "Input name:"
    nameStr <- getLine
    putStr "Input phone number:"
    phoneStr <- getLine
    putStr "Input e-mail:"
    mailStr <- getLine
    return  Contact { name = nameStr, phone = phoneStr, email = mailStr}