{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import FingerD
import System.Environment (getArgs)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types

getTextField :: String -> IO Text
getTextField msg = do
  putStrLn msg
  answer <- getLine
  return $ T.pack answer

getUserRow :: IO UserRow
getUserRow = do
  putStrLn "Please enter user information"
  username <- getTextField "User: "
  shell <- getTextField "Shell: "
  homeDir <- getTextField "Home Directory: "
  fullName <- getTextField "Full Name: "
  phone <- getTextField "Phone: "
  return (Null, username, shell, homeDir, fullName, phone)

addUser :: IO ()
addUser = do
  putStrLn "ADDING USER"
  user@(n,u,s,h,f,p) <- getUserRow
  conn <- open "finger.db"
  alreadyInDb <- getUser conn u
  case alreadyInDb of
    Just _ -> do
      putStrLn $ "User " ++ (show u) ++ " already in DB"
      SQLite.close conn
    Nothing -> do
      execute conn insertUser user
      putStrLn $ "ADDED NEW USER: " ++ show u
      SQLite.close conn

-- UPDATE USER
updateUserQ :: Query
updateUserQ =
  "UPDATE users SET ? = ? \
  \ WHERE username = ?"

updateUsername :: Query
updateUsername =
  "UPDATE users SET username = ? WHERE username = ?"

updateShell :: Query
updateShell = "UPDATE users SET shell = ? WHERE username = ?"

updateHomeDir :: Query
updateHomeDir =
  "UPDATE users SET homeDirectory = ? WHERE username = ?"

updateRealName :: Query
updateRealName =
  "UPDATE users SET realName = ? WHERE username = ?"

updatePhone :: Query
updatePhone =
  "UPDATE users SET phone = ? WHERE username = ?"

pickQuery :: IO Query
pickQuery = do
  putStrLn "Which field do you wish to edit?"
  putStrLn "1) username, 2) shell, 3) homeDirectory"
  putStrLn "4) realName, 5) phone"
  choice <- getLine
  case choice of
    '1':_ -> do
      putStrLn "Selected 'username'"
      return updateUsername
    '2':_ -> do
      putStrLn "Selected 'shell'"
      return updateShell
    '3':_ -> do
      putStrLn "Selected 'homeDirectory'"
      return updateHomeDir
    '4':_ -> do
      putStrLn "Selected 'realName'"
      return updateRealName
    '5':_ -> do
      putStrLn "Selected 'phone'"
      return updatePhone
    _ -> do
      putStrLn "Invalid selection. Defaulting to shell"
      return updateShell

updateUser :: String -> IO ()
updateUser username = do
  let username' = T.pack username
  putStrLn "UPDATING USER"
  conn <- open "finger.db"
  alreadyInDb <- getUser conn username'
  case alreadyInDb of
    Nothing -> do
      putStrLn $ "No user found with username: " ++ show username
      SQLite.close conn
    Just _ -> do
      updateQ <- pickQuery
      putStrLn "What is the new value?"
      newValue <- getLine
      execute conn updateQ (T.pack newValue, username')
      putStrLn $ "UPDATING TO " ++ show newValue
      SQLite.close conn

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-add"] -> addUser
    ["-update", username] -> updateUser username
    _ -> do
      putStrLn "Command not supported. Supported commands:"
      putStrLn "ADD USER: -add"
      putStrLn "UPDATE USER: -update username"
