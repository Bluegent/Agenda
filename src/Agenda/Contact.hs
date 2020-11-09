{-# LANGUAGE DeriveGeneric #-}
module Agenda.Contact where

-- import as B to avoid name clash between Prelude's ByteString and Data.ByteString
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import GHC.Generics
import Agenda.Utils

data Contact =
  Contact { name     :: String
          , surname  :: String
          , phone    :: String
          } deriving (Show,Generic)

instance FromJSON Contact
instance ToJSON Contact

writeContactList :: FilePath -> [Contact] -> IO()
writeContactList path people = B.writeFile path (encode people)

printContactList :: [Contact] -> IO()
printContactList list = do 
    putStrLn "Your Loaded contacts are:"
    forM_ list $ \s -> do
        putStrLn $ name s ++" "++ surname s


parseContacts :: FilePath -> IO([Contact]) 
parseContacts path = do
    res <- (eitherDecode <$> pathToString path) :: IO (Either String [Contact])
    case res of
        Left err -> do
            putStrLn err
            return []
        Right contacts -> return contacts