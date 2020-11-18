{-# LANGUAGE ForeignFunctionInterface #-}
module Agenda.Utils where

import Data.Char
import Foreign.C.Types
import Text.Read
import Data.Maybe

-- import as B to avoid name clash between Prelude's ByteString and Data.ByteString
import qualified Data.ByteString.Lazy as B


readChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt




-- reads the contents of a file indicated by path to a bytestring
pathToString :: FilePath -> IO B.ByteString
pathToString path = B.readFile path


lowerString = Prelude.map Data.Char.toLower


isValidInt :: String -> Bool
isValidInt strInt = isJust (readMaybe strInt :: Maybe Int)

stringToInt :: String -> Int
stringToInt = read 