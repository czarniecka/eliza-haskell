module Main where

import System.IO
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Regex.TDFA ((=~))
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing)
import Data.Char (toLower)


import Rules

main :: IO ()
main = do
    putStrLn "Welcome to the chatbot you can talk to about your mental health. Type 'bye' to end the conversation."
    putStrLn ""
    putStrLn "Hi! How are you?"
    loop emptyUserData

loop :: UserData -> IO ()
loop userData = do
    putStr "> "
    hFlush stdout
    input <- getLine
    if map toLower input =~ ("\\b(bye|goodbye)\\b" :: String)
        then do
            saveSession userData
            putStrLn "Thanks for this conversation. I hope you will feel better. Bye!"
        else do
            let updatedData = updateUserData input userData
                response = generateResponse input
            putStrLn response
            loop updatedData

saveSession :: UserData -> IO ()
saveSession ud = do
    currentTime <- getZonedTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
        folder = "../data/"
        fileName = folder ++ "session-" ++ formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" currentTime ++ ".txt"
        content = unlines
            [ "Conversation ended at: " ++ timestamp
            , "Name: " ++ fromMaybe "unknown" (userName ud)
            , "Mood: " ++ if null (userMood ud) then "unknown" else intercalate ", " (userMood ud)
            , "Problem: " ++ if null (userProblem ud) then "unknown" else intercalate ", " (userProblem ud)
            , "Stressor: " ++ if null (userStressor ud) then "unknown" else intercalate ", " (userStressor ud)
            , "\nWhole conversation:"
            ] ++ unlines (rawMessages ud)

    createDirectoryIfMissing True folder
    writeFile fileName content
    putStrLn $ "Conversation saved to: " ++ fileName
