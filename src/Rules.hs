module Rules where

import Text.Regex.TDFA ((=~))
import Data.Char (toLower, toUpper)
import Data.List (isInfixOf)

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

-- Helper: extract single group from regex match
matchRegex :: String -> String -> Maybe String
matchRegex regex input =
    let (_, _, _, matches) = input =~ regex :: (String, String, String, [String])
    in case matches of
         [x] -> Just x
         _   -> Nothing

-- Levenshtein distance
levenshtein :: String -> String -> Int
levenshtein s t = last (foldl transform [0..length t] s)
  where
    transform xs@(x:xs') c = scanl compute (x + 1) (zip3 t xs xs')
      where
        compute z (c', x', y) = minimum [y + 1, z + 1, x' + fromEnum (c /= c')]

-- Check if input is similar (at most 2 edits away)
isSimilar :: String -> String -> Bool
isSimilar s1 s2 = levenshtein s1 s2 <= 3

-- Helper to check similarity with phrases
matchesApprox :: [String] -> String -> Bool
matchesApprox phrases input = any (`isInfixOf` input) phrases || any (`isSimilar` input) phrases

-- Update user data based on input
updateUserData :: String -> UserData -> UserData
updateUserData input ud =
  let inputLower = toLowerString input
      raw = rawMessages ud ++ [input]
  in case () of
    _ | Just name <- matchRegex "my name is ([a-z]+)" inputLower ->
          ud { userName = Just (capitalize name), rawMessages = raw }
      | Just name <- matchRegex "i am ([a-z]+)" inputLower ->
          ud { userName = Just (capitalize name), rawMessages = raw }
      | Just name <- matchRegex "i['’`]?m ([a-z]+)" inputLower ->
          ud { userName = Just (capitalize name), rawMessages = raw }
      | Just name <- matchRegex "iam ([a-z]+)" inputLower -> 
          ud { userName = Just (capitalize name), rawMessages = raw }
      | Just mood <- matchRegex "i feel ([a-z]+)" inputLower ->
          ud { userMood = Just mood, rawMessages = raw }
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
    _ | Just name <- matchRegex "my name is ([a-z]+)" inputLower ->
          "Nice to meet you, " ++ capitalize name ++ "."
      | Just name <- matchRegex "i am ([a-z]+)" inputLower ->
          "Nice to meet you, " ++ capitalize name ++ "."
      | Just name <- matchRegex "i['’`]?m ([a-z]+)" inputLower ->
          "Nice to meet you, " ++ capitalize name ++ "."
      | Just name <- matchRegex "iam ([a-z]+)" inputLower ->
          "Nice to meet you, " ++ capitalize name ++ "."
      | Just _ <- matchRegex "i feel ([a-z]+)" inputLower ->
          "What makes you feel that way?"
      | Just _ <- matchRegex "i have a problem with (.+)" inputLower ->
          "Have you tried talking to someone about it?"
      | Just _ <- matchRegex "i am stressed about (.+)" inputLower ->
          "That sounds tough. How are you coping with it?"
      | matchesApprox ["thank you"] inputLower ->
          "You’re welcome. I’m here for you."
      | matchesApprox ["i can't sleep"] inputLower ->
          "Lack of sleep can be really hard. Have you tried any techniques to relax before bed?"
      | matchesApprox ["i feel lonely"] inputLower ->
          "You’re not alone in feeling that way. Would you like to talk more about it?"
      | matchesApprox ["i feel anxious"] inputLower ->
          "Anxiety can be overwhelming. What do you think is triggering it?"
      | matchesApprox ["i have no motivation"] inputLower ->
          "It's okay to feel unmotivated sometimes. What would help you feel more energized?"
      | matchesApprox ["can you help me"] inputLower ->
          "I’ll do my best to support you. What’s on your mind?"
      | matchesApprox ["i'm afraid of"] inputLower ->
          "That sounds scary. What makes you afraid of that?"
      | matchesApprox ["i feel overwhelmed"] inputLower ->
          "That must be difficult. Have you tried breaking things down into smaller steps?"
      | matchesApprox ["i need someone to talk to"] inputLower ->
          "I'm here to listen. What would you like to share?"
      | matchesApprox ["i feel depressed"] inputLower ->
          "I'm really sorry you're feeling this way. You're not alone, and it's okay to talk about it."
      | matchesApprox ["i hate myself"] inputLower ->
          "I'm really sorry you feel that way. You matter, and your feelings are important."
      | matchesApprox ["no one understands me"] inputLower ->
          "That sounds very isolating. I’m here, and I want to understand."
      | matchesApprox ["i can't focus"] inputLower ->
          "Concentration can be hard when your mind is busy. What's been on your mind lately?"
      | matchesApprox ["i'm tired of everything"] inputLower ->
          "It’s okay to feel exhausted. Would you like to talk about what’s been wearing you down?"
      | matchesApprox ["i want to give up"] inputLower ->
          "That sounds really tough. What’s been making you feel this way?"
      | matchesApprox ["i cried today"] inputLower ->
          "Crying is a natural way to release emotions. What made you feel like that?"
      | matchesApprox ["i'm lost"] inputLower ->
          "When you say you're lost, what do you mean? Do you want to talk about what’s confusing?"
      | matchesApprox ["i don't see the point"] inputLower ->
          "It’s okay to question things. Would you like to explore what’s been making you feel that way?"
      | matchesApprox ["what should i do"] inputLower ->
          "Let’s try to understand what you’re going through first. Can you tell me more?"
      | matchesApprox ["i need advice"] inputLower ->
          "I'm here to listen and help as much as I can. What’s going on?"
      | otherwise -> "I understand. Please tell me more."
