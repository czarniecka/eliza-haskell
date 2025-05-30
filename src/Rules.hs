module Rules where

import Text.Regex.TDFA ((=~))
import Data.Maybe (fromMaybe)
import Data.Char (toLower)

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

-- Update user data based on input
updateUserData :: String -> UserData -> UserData
updateUserData input ud =
  let inputLower = toLowerString input
  in if inputLower =~ "my name is ([a-z]+)"
       then let (_, _, _, [name]) = inputLower =~ "my name is ([a-z]+)" :: (String, String, String, [String])
            in ud { userName = Just name, rawMessages = rawMessages ud ++ [input] }
  else if inputLower =~ "i feel ([a-z]+)"
       then let (_, _, _, [mood]) = inputLower =~ "i feel ([a-z]+)" :: (String, String, String, [String])
            in ud { userMood = Just mood, rawMessages = rawMessages ud ++ [input] }
  else if inputLower =~ "i have a problem with (.*)"
       then let (_, _, _, [problem]) = inputLower =~ "i have a problem with (.*)" :: (String, String, String, [String])
            in ud { userProblem = Just problem, rawMessages = rawMessages ud ++ [input] }
  else if inputLower =~ "i am stressed about (.*)"
       then let (_, _, _, [stressor]) = inputLower =~ "i am stressed about (.*)" :: (String, String, String, [String])
            in ud { userStressor = Just stressor, rawMessages = rawMessages ud ++ [input] }
  else
       ud { rawMessages = rawMessages ud ++ [input] }

-- Generate response based on input
generateResponse :: String -> String
generateResponse input =
  let inputLower = toLowerString input
  in if inputLower =~ "i feel ([a-z]+)"
       then "What makes you feel that way?"
  else if inputLower =~ "i have a problem with (.*)"
       then "Have you tried talking to someone about it?"
  else if inputLower =~ "i am stressed about (.*)"
       then "That sounds tough. How are you coping with it?"
  else if inputLower =~ "my name is ([a-z]+)"
       then let (_, _, _, [name]) = inputLower =~ "my name is ([a-z]+)" :: (String, String, String, [String])
            in "Nice to meet you, " ++ name ++ "."
  else if inputLower =~ "i can't sleep"
       then "Lack of sleep can be really hard. Have you tried any techniques to relax before bed?"
  else if inputLower =~ "i feel lonely"
       then "You’re not alone in feeling that way. Would you like to talk more about it?"
  else if inputLower =~ "i feel anxious"
       then "Anxiety can be overwhelming. What do you think is triggering it?"
  else if inputLower =~ "i have no motivation"
       then "It's okay to feel unmotivated sometimes. What would help you feel more energized?"
  else if inputLower =~ "can you help me"
       then "I’ll do my best to support you. What’s on your mind?"
  else if inputLower =~ "thank you"
       then "You’re welcome. I’m here for you."
  else if inputLower =~ "i'm afraid of (.*)"
       then "That sounds scary. What makes you afraid of that?"
  else if inputLower =~ "i feel overwhelmed"
       then "That must be difficult. Have you tried breaking things down into smaller steps?"
  else if inputLower =~ "i need someone to talk to"
       then "I'm here to listen. What would you like to share?"
  else if inputLower =~ "i feel depressed"
       then "I'm really sorry you're feeling this way. You're not alone, and it's okay to talk about it."
  else if inputLower =~ "i hate myself"
       then "I'm really sorry you feel that way. You matter, and your feelings are important."
  else if inputLower =~ "no one understands me"
       then "That sounds very isolating. I’m here, and I want to understand."
  else if inputLower =~ "i can't focus"
       then "Concentration can be hard when your mind is busy. What's been on your mind lately?"
  else if inputLower =~ "i'm tired of everything"
       then "It’s okay to feel exhausted. Would you like to talk about what’s been wearing you down?"
  else if inputLower =~ "i want to give up"
       then "That sounds really tough. What’s been making you feel this way?"
  else if inputLower =~ "i cried today"
       then "Crying is a natural way to release emotions. What made you feel like that?"
  else if inputLower =~ "i'm lost"
       then "When you say you're lost, what do you mean? Do you want to talk about what’s confusing?"
  else if inputLower =~ "i don't see the point"
       then "It’s okay to question things. Would you like to explore what’s been making you feel that way?"
  else if inputLower =~ "what should i do"
       then "Let’s try to understand what you’re going through first. Can you tell me more?"
  else if inputLower =~ "i need advice"
       then "I'm here to listen and help as much as I can. What’s going on?"
  else
       "I understand. Please tell me more."


