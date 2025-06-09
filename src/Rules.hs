module Rules where

import Text.Regex.TDFA ((=~))
import Data.Char (toLower, toUpper)
import Data.List (words)

-- User data

data UserData = UserData
    { userName     :: Maybe String
    , userMood     :: Maybe String
    , userProblem  :: Maybe String
    , userStressor :: Maybe String
    , rawMessages  :: [String]
    } deriving Show

emptyUserData :: UserData
emptyUserData = UserData Nothing Nothing Nothing Nothing []

-- Convert input to lowercase
toLowerString :: String -> String
toLowerString = map toLower

-- Capitalize first letter of a string
capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs

-- Count tokens in a string
countTokens :: String -> Int
countTokens = length . words

-- Check if token count is 2 or 3
validNameTokenCount :: String -> Bool
validNameTokenCount s = let c = countTokens s in c == 1

-- Helper: extract single group from regex match
matchRegex :: String -> String -> Maybe String
matchRegex regex input =
    let (_, _, _, matches) = input =~ regex :: (String, String, String, [String])
    in case matches of
         [x] -> Just x
         _   -> Nothing

-- Update user data based on input
updateUserData :: String -> UserData -> UserData
updateUserData input ud =
  let inputLower = toLowerString input
      raw = rawMessages ud ++ [input]
  in case () of
    _ | Just name <- matchRegex "my name is ([a-z ]+)" inputLower ->
          ud { userName = Just (capitalize name), rawMessages = raw }
      | Just name <- matchRegex "i am ([a-z ]+)" inputLower
            , validNameTokenCount name ->
          ud { userName = Just (capitalize name), rawMessages = raw }
      | Just name <- matchRegex "i['’`]?m ([a-z ]+)" inputLower
            , validNameTokenCount name ->
          ud { userName = Just (capitalize name), rawMessages = raw }
      | Just name <- matchRegex "iam ([a-z ]+)" inputLower
            , validNameTokenCount name -> 
          ud { userName = Just (capitalize name), rawMessages = raw }
      | Just moodPhrase <- matchRegex "i feel (.+)" inputLower ->
        let wordsFiltered = filter (`notElem` ["a", "an", "the", "bit", "very"]) (words moodPhrase)
            mood = unwords wordsFiltered
        in ud { userMood = Just mood, rawMessages = raw }
      | Just problem <- matchRegex "i have a problem with (.+)" inputLower ->
          ud { userProblem = Just problem, rawMessages = raw }
      | Just stressor <- matchRegex "i am stressed about (.+)" inputLower ->
          ud { userStressor = Just stressor, rawMessages = raw }
      | otherwise -> ud { rawMessages = raw }

-- Generate response based on input
generateResponse :: String -> String
generateResponse input =
  let inputLower = toLowerString input
  in case () of
    _ | Just name <- matchRegex "my name is ([a-z ]+)" inputLower ->
          "Nice to meet you, " ++ capitalize name ++ "."
      | Just name <- matchRegex "i am ([a-z ]+)" inputLower
            , validNameTokenCount name ->
          "Nice to meet you, " ++ capitalize name ++ "."
      | Just name <- matchRegex "i['’`]?m ([a-z ]+)" inputLower
            , validNameTokenCount name ->
          "Nice to meet you, " ++ capitalize name ++ "."
      | Just name <- matchRegex "iam ([a-z ]+)" inputLower
            , validNameTokenCount name ->
          "Nice to meet you, " ++ capitalize name ++ "."
      | Just moodPhrase <- matchRegex "i feel (.+)" inputLower ->
        let wordsFiltered = filter (`notElem` ["a", "an", "the", "bit", "very"]) (words moodPhrase)
            mood = unwords wordsFiltered
        in "What makes you feel " ++ mood ++ "?"
      | Just _ <- matchRegex "i have a problem with (.+)" inputLower ->
          "Have you tried talking to someone about it?"
      | Just _ <- matchRegex "i am stressed about (.+)" inputLower ->
          "That sounds tough. How are you coping with it?"
      | inputLower =~ "thank you" -> "You’re welcome. I’m here for you."
      | inputLower =~ "i can't sleep" ->
          "Lack of sleep can be really hard. Have you tried any techniques to relax before bed?"
      | inputLower =~ "i feel lonely" ->
          "You’re not alone in feeling that way. Would you like to talk more about it?"
      | inputLower =~ "i feel anxious" ->
          "Anxiety can be overwhelming. What do you think is triggering it?"
      | inputLower =~ "i have no motivation" ->
          "It's okay to feel unmotivated sometimes. What would help you feel more energized?"
      | inputLower =~ "can you help me" ->
          "I’ll do my best to support you. What’s on your mind?"
      | inputLower =~ "i'm afraid of (.*)" ->
          "That sounds scary. What makes you afraid of that?"
      | inputLower =~ "i feel overwhelmed" ->
          "That must be difficult. Have you tried breaking things down into smaller steps?"
      | inputLower =~ "i need someone to talk to" ->
          "I'm here to listen. What would you like to share?"
      | inputLower =~ "i feel depressed" ->
          "I'm really sorry you're feeling this way. You're not alone, and it's okay to talk about it."
      | inputLower =~ "i hate myself" ->
          "I'm really sorry you feel that way. You matter, and your feelings are important."
      | inputLower =~ "no one understands me" ->
          "That sounds very isolating. I’m here, and I want to understand."
      | inputLower =~ "i can't focus" ->
          "Concentration can be hard when your mind is busy. What's been on your mind lately?"
      | inputLower =~ "i'm tired of everything" ->
          "It’s okay to feel exhausted. Would you like to talk about what’s been wearing you down?"
      | inputLower =~ "i want to give up" ->
          "That sounds really tough. What’s been making you feel this way?"
      | inputLower =~ "i cried today" ->
          "Crying is a natural way to release emotions. What made you feel like that?"
      | inputLower =~ "i'm lost" ->
          "When you say you're lost, what do you mean? Do you want to talk about what’s confusing?"
      | inputLower =~ "i don't see the point" ->
          "It’s okay to question things. Would you like to explore what’s been making you feel that way?"
      | inputLower =~ "what should i do" ->
          "Let’s try to understand what you’re going through first. Can you tell me more?"
      | inputLower =~ "i need advice" ->
          "I'm here to listen and help as much as I can. What’s going on?"
      | otherwise -> "I understand. Please tell me more."
