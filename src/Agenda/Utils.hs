{-# LANGUAGE ForeignFunctionInterface #-}
module Agenda.Utils where

import Data.Char
import Foreign.C.Types
import Text.Read
import Data.Maybe
import System.Console.ANSI

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


menuCol = 20
recordCol = 10

putStrCursor:: Int->String->IO()
putStrCursor col str = do
    setCursorColumn col
    putStr str

putStrLnCursor:: Int->String->IO()
putStrLnCursor col str = do
    setCursorColumn col
    putStrLn str

putRecordStr:: String -> IO()
putRecordStr str = do
    putStrCursor recordCol str

putRecordStrLn:: String -> IO()
putRecordStrLn str = do
    putStrLnCursor recordCol str

putMenuStr:: String -> IO()
putMenuStr str = do
    putStrCursor menuCol str

putMenuStrLn:: String -> IO()
putMenuStrLn str = do
    putStrLnCursor menuCol str
    
resetScreen = do
    clearScreen
    setCursorPosition 0 menuCol

waitForUserRead:: IO() 
waitForUserRead = do
    putMenuStrLn "Press any key to continue"
    line <- readChar
    return()