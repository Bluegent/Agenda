module AgendaUtils where

-- import as B to avoid name clash between Prelude's ByteString and Data.ByteString
import qualified Data.ByteString.Lazy as B

-- reads the contents of a file indicated by path to a bytestring
pathToString :: FilePath -> IO B.ByteString
pathToString path = B.readFile path