module Rules where

import Text.Regex.TDFA ((=~))
import Data.Char (toLower, toUpper)
import Data.List (isInfixOf)
import Data.List (words)
import Data.Char (ord, isAlpha)

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

notNames :: [String]
notNames =
  [ "tired", "sad", "happy", "depressed", "angry", "lost"
  , "lonely", "anxious", "ok", "fine", "good", "bad", "great"
  , "stressed", "smart", "hopeless", "useless", "worthless"
  , "scared", "afraid", "frustrated", "numb", "empty"
  , "overwhelmed", "worried", "broken", "down", "low"
  , "ashamed", "confused", "nervous", "ignored", "unseen"
  , "helpless", "invisible", "sad", "guilty", "exhausted"
  , "angsty", "moody", "weak", "defeated", "hurt"
  , "shy", "quiet", "isolated", "misunderstood", "apathetic"
  , "bored", "unmotivated", "rejected", "insecure", "overhelmed"
  ]


cleanMoodWords :: String -> String
cleanMoodWords = unwords . filter (`notElem` ["a", "bit", "very", "just", "kind of"]) . words

moodPhrases :: [String]
moodPhrases =
  [ "not smart enough", "not good enough", "not worthy", "not okay"
  , "not fine", "not happy", "not capable", "not loved", "not understood", "not confident"
  , "not motivated", "not important", "not enough", "not strong enough"
  , "not successful", "not interesting", "not needed", "not safe"
  , "not stable", "not useful", "not appreciated", "not wanted"
  , "not heard", "not respected", "not brave", "not whole"
  , "not calm", "not relaxed", "not in control", "not good with people"
  ]

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

-- Deterministic choice from a list based on input length
deterministicChoice :: [String] -> String -> String
deterministicChoice options input =
  let asciiSum = sum (map ord input)
      wordCount = length (words input)
      alphaCount = length (filter isAlpha input)
      mix = asciiSum + wordCount * 3 + alphaCount * 2
      index = mix `mod` length options
  in options !! index

-- Update user data based on input
updateUserData :: String -> UserData -> UserData
updateUserData input ud =
    let inputLower = toLowerString input
        raw = rawMessages ud ++ [input]
    in case () of
      _ | Just name <- matchRegex "my name is ([a-z ]+)" inputLower ->
            ud { userName = Just (capitalize name), rawMessages = raw }
        | Just name <- matchRegex "i am ([a-z ]+)" inputLower ->
            let cleaned = cleanMoodWords name
                tokenCount = length (words name)
            in if (cleaned `elem` notNames && tokenCount <= 3) || name `elem` moodPhrases
                  then ud { userMood = Just cleaned, rawMessages = raw }
                  else if validNameTokenCount name
                       then ud { userName = Just (capitalize name), rawMessages = raw }
                       else ud { rawMessages = raw }
        | Just name <- matchRegex "i['’`]?m ([a-z ]+)" inputLower ->
            let cleaned = cleanMoodWords name
                tokenCount = length (words name)
            in if (cleaned `elem` notNames && tokenCount <= 3) || name `elem` moodPhrases
                  then ud { userMood = Just cleaned, rawMessages = raw }
                  else if validNameTokenCount name
                       then ud { userName = Just (capitalize name), rawMessages = raw }
                       else ud { rawMessages = raw }
        | Just name <- matchRegex "iam ([a-z ]+)" inputLower ->
            let cleaned = cleanMoodWords name
                tokenCount = length (words name)
            in if (cleaned `elem` notNames && tokenCount <= 3) || name `elem` moodPhrases
                  then ud { userMood = Just cleaned, rawMessages = raw }
                  else if validNameTokenCount name
                       then ud { userName = Just (capitalize name), rawMessages = raw }
                       else ud { rawMessages = raw }
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
        respondNameOrMood :: String -> Maybe String
        respondNameOrMood word =
          let tokens = words word
              tokenCount = length tokens
              cleaned = cleanMoodWords word
          in if tokenCount <= 3
               then if cleaned `elem` notNames || word `elem` moodPhrases
                      then Nothing
                      else Just ("Nice to meet you, " ++ capitalize word ++ ".")
               else Nothing

    in case () of
      _ | Just name <- matchRegex "my name is ([a-z ]+)" inputLower ->
            "Nice to meet you, " ++ capitalize name ++ "."
        | Just name <- matchRegex "i am ([a-z ]+)" inputLower
        , Just resp <- respondNameOrMood name -> resp
        | Just name <- matchRegex "i['’`]?m ([a-z ]+)" inputLower
        , Just resp <- respondNameOrMood name -> resp
        | Just name <- matchRegex "iam ([a-z ]+)" inputLower
        , Just resp <- respondNameOrMood name -> resp
        | Just moodPhrase <- matchRegex "i feel (.+)" inputLower ->
            let wordsFiltered = filter (`notElem` ["a", "an", "the", "bit", "very"]) (words moodPhrase)
                mood = unwords wordsFiltered
            in "What makes you feel " ++ mood ++ "?"
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
        | matchesApprox ["i feel worthless"] inputLower ->
            "I'm really sorry you're feeling this way. You're valuable, even if it doesn't feel like it right now."
        | matchesApprox ["i hate my life"] inputLower ->
            "That sounds incredibly painful. I'm here for you — would you like to talk about what’s been happening?"
        | matchesApprox ["i feel empty"] inputLower ->
            "That emptiness can be so heavy. Do you want to share what's been making you feel this way?"
        | matchesApprox ["i feel stuck"] inputLower ->
            "Feeling stuck is hard. What do you think is holding you back?"
        | matchesApprox ["i don't know what to do"] inputLower ->
            "It’s okay to feel uncertain. Maybe talking it through can help. What’s going on?"
        | matchesApprox ["everything hurts"] inputLower ->
            "That sounds really tough. Do you want to talk about what’s been hurting you?"
        | matchesApprox ["i feel like giving up"] inputLower ->
            "I hear you. You’re not alone in this — do you want to tell me more about what’s going on?"
        | matchesApprox ["i can't do this anymore"] inputLower ->
            "That sounds overwhelming. You’re not alone — I'm here to listen."
        | matchesApprox ["i feel hopeless"] inputLower ->
            "Hopelessness is such a heavy feeling. Let’s talk about what’s been making you feel that way."
        | matchesApprox ["i miss someone"] inputLower ->
            "Missing someone can be really painful. Do you want to talk about them?"
        | matchesApprox ["why does it hurt so much"] inputLower ->
            "Emotional pain can be deep. I’m here with you — do you want to share more about what’s hurting?"
        | matchesApprox ["no one cares about me"] inputLower ->
            "I'm really sorry you feel this way. I care, and I'm here to listen to you."
        | matchesApprox ["i feel scared"] inputLower ->
            "It's okay to feel scared. Do you want to talk about what's been making you feel this way?"
        | matchesApprox ["i want to disappear"] inputLower ->
            "I'm really sorry you’re feeling this way. You matter, and I’m here for you if you want to talk."
        | matchesApprox ["i feel like a failure"] inputLower ->
            "I’m sorry you feel that way. Everyone struggles sometimes — would you like to talk about it?"
        | matchesApprox ["i feel like nobody listens"] inputLower ->
            "I’m here, and I’m listening. What would you like to share?"
        | matchesApprox ["i can't focus on studying"] inputLower ->
            "It can be really hard to concentrate sometimes. Do you know what might be distracting you?"
        | matchesApprox ["i'm behind on my assignments"] inputLower ->
            "Falling behind can feel overwhelming. Maybe we can figure out how to start tackling them?"
        | matchesApprox ["i'm afraid of failing exams"] inputLower ->
            "Exams can be really stressful. Have you tried any techniques that help you feel more prepared?"
        | matchesApprox ["i don't understand the material"] inputLower ->
            "That can be so frustrating. Would talking it through help you find a way forward?"
        | matchesApprox ["i'm failing my classes"] inputLower ->
            "That sounds really tough. Do you want to talk about what’s been making it hard to keep up?"
        | matchesApprox ["i'm not good enough for this degree"] inputLower ->
            "Many students feel this way sometimes. You're not alone — do you want to talk about what’s making you feel that way?"
        | matchesApprox ["i feel like i chose the wrong major"] inputLower ->
            "That’s a big feeling to sit with. What’s been making you question your major?"
        | matchesApprox ["i can't balance work and study"] inputLower ->
            "Balancing work and study is really challenging. Want to talk through your schedule?"
        | matchesApprox ["i have too much to do"] inputLower ->
            "That sounds overwhelming. Maybe we can break things down into smaller, more manageable steps?"
        | matchesApprox ["i feel burnt out"] inputLower ->
            "Burnout is real, especially in university. What’s been draining your energy the most?"
        | matchesApprox ["i don't have any friends here"] inputLower ->
            "Feeling lonely at university is more common than you think. Would you like to talk about how it’s been for you?"
        | matchesApprox ["i'm homesick"] inputLower ->
            "Being away from home can be really tough. What do you miss the most?"
        | matchesApprox ["i can't afford rent"] inputLower ->
            "Financial stress can be so hard to carry. Would you like to talk about your situation?"
        | matchesApprox ["i feel lost in lectures"] inputLower ->
            "It’s okay not to understand everything right away. Want to talk about which parts confuse you the most?"
        | matchesApprox ["i'm scared of speaking in class"] inputLower ->
            "Public speaking can be really intimidating. Would practicing or preparing more help ease the anxiety?"
        | matchesApprox ["i failed my exam"] inputLower ->
            "That’s really tough to deal with. Would you like to talk about what happened and what’s next?"
        | matchesApprox ["i feel unmotivated to study"] inputLower ->
            "It’s okay to feel that way. What do you think is blocking your motivation right now?"
        | matchesApprox ["i'm not learning anything"] inputLower ->
            "That must be frustrating. Do you want to explore ways to feel more engaged with your classes?"
        | matchesApprox ["i regret coming to university"] inputLower ->
            "That’s a heavy feeling. Want to share what’s been making you feel this way?"
        | matchesApprox ["i feel pressure to succeed"] inputLower ->
            "That pressure can be intense. Let’s talk about where it’s coming from and how you’re coping."
        | matchesApprox ["i can't take it anymore"] inputLower ->
            "That sounds really overwhelming. You’re not alone — I’m here for you. Want to talk more about it?"
        | matchesApprox ["i feel emotionally drained"] inputLower ->
            "Emotional exhaustion is really hard. What do you think has been wearing you down the most?"
        | matchesApprox ["everything feels meaningless"] inputLower ->
            "That’s such a heavy feeling. Do you want to talk about where that sense of emptiness is coming from?"
        | matchesApprox ["i'm tired all the time"] inputLower ->
            "Constant fatigue can be a sign you need a break. How have you been resting lately?"
        | matchesApprox ["i just want to sleep forever"] inputLower ->
            "That sounds like you’re really struggling. You matter, and I’d like to hear what’s been going on."
        | matchesApprox ["i feel like a burden"] inputLower ->
            "I'm really sorry you feel this way. You are not a burden — your feelings matter."
        | matchesApprox ["i keep disappointing everyone"] inputLower ->
            "It’s okay to feel like you’re falling short, but you’re human. Want to talk about where that pressure is coming from?"
        | matchesApprox ["i'm scared i won't graduate"] inputLower ->
            "That’s a very real fear for many students. Want to talk about what’s been getting in the way?"
        | matchesApprox ["i don't know what to do after graduation"] inputLower ->
            "That uncertainty is tough. Many students feel that way — do you want to talk through your thoughts?"
        | matchesApprox ["my parents expect too much from me"] inputLower ->
            "That kind of pressure can feel really intense. Want to share more about their expectations?"
        | matchesApprox ["i'm falling apart during exam season"] inputLower ->
            "Exam season can be brutal. What’s been hardest for you lately?"
        | matchesApprox ["i have no time for myself"] inputLower ->
            "It’s important to take breaks, even when things are busy. How can you carve out a bit of time for yourself?"
        | matchesApprox ["i feel like everyone else is doing better than me"] inputLower ->
            "It's easy to feel that way, especially when you’re under pressure. Want to talk about how you’re comparing yourself?"
        | matchesApprox ["i'm just not smart enough"] inputLower ->
            "Feeling that way doesn’t mean it’s true. Let’s talk about what’s been making you doubt yourself."
        | matchesApprox ["i feel stuck in this degree"] inputLower ->
            "That’s frustrating. Want to talk about what’s been making it feel like a trap?"
        | matchesApprox ["i can't handle group work"] inputLower ->
            "Working in groups can be really difficult. What’s been frustrating you the most about it?"
        | matchesApprox ["i hate online classes"] inputLower ->
            "Online learning isn’t for everyone. Want to talk about what’s been bothering you?"
        | matchesApprox ["i don't belong here"] inputLower ->
            "That’s a really hard feeling. You’re not alone — would you like to talk about what’s making you feel that way?"
        | matchesApprox ["i feel invisible"] inputLower ->
            "Feeling unseen can really hurt. I see you, and I’m listening. Want to tell me more?"
        | matchesApprox ["i'm afraid to ask for help"] inputLower ->
            "It’s brave to even say that. Asking for help is hard — I’m here if you want to try."
        | matchesApprox ["i eat alone every day"] inputLower ->
            "That sounds really lonely. Would you like to talk about how it's been for you?"
        | matchesApprox ["i have no one to talk to"] inputLower ->
            "You’re not alone right now — I’m here and ready to listen."
        | matchesApprox ["no one would notice if i disappeared"] inputLower ->
            "I'm really sorry you feel this way. You matter, and your presence is important."
        | matchesApprox ["i don't know who i am anymore"] inputLower ->
            "That’s a really deep feeling. Do you want to talk about what’s been making you feel disconnected from yourself?"
        | matchesApprox ["i feel like i'm wasting my youth"] inputLower ->
            "Many students feel that way, especially during hard times. What makes you feel that?"
        | matchesApprox ["i never have time to relax"] inputLower ->
            "That sounds exhausting. What would you do if you had a free day just for yourself?"
        | matchesApprox ["i'm scared of the future"] inputLower ->
            "The future can feel really uncertain. Want to talk about what worries you the most?"
        | matchesApprox ["really sad", "so sad", "very sad"] inputLower ->
            "I’m sorry you’re feeling that way. Do you want to talk more about it?"
        | matchesApprox ["really tired", "so tired", "very tired"] inputLower ->
            "That sounds exhausting. Want to talk about what’s been draining your energy?"
        | matchesApprox ["really angry", "so angry", "very angry"] inputLower ->
            "Anger can be powerful. What’s been making you feel this way?"
        | matchesApprox ["really anxious", "so anxious", "very anxious"] inputLower ->
            "Anxiety can feel overwhelming. Do you want to share what’s causing it?"
        | matchesApprox ["really lonely", "so lonely", "very lonely"] inputLower ->
            "That must feel very isolating. I’m here to listen if you want to talk."
        | matchesApprox ["really depressed", "so depressed", "very depressed"] inputLower ->
            "Depression is really hard. You’re not alone. Would talking help a bit?"
        | matchesApprox ["really empty", "so empty", "very empty"] inputLower ->
            "Feeling empty can be difficult. What do you think might be behind that feeling?"
        | matchesApprox ["really lost", "so lost", "very lost"] inputLower ->
            "When everything feels unclear, it helps to talk. What’s making you feel lost?"
        | matchesApprox ["really stressed", "so stressed", "very stressed"] inputLower ->
            "Stress can pile up fast. Want to tell me more about what’s going on?"
        | matchesApprox ["really overwhelmed", "so overwhelmed", "very overwhelmed"] inputLower ->
            "It’s okay to feel overwhelmed. Maybe sharing what’s on your plate could help."
        | matchesApprox ["really frustrated", "so frustrated", "very frustrated"] inputLower ->
            "Frustration can be really tough to deal with. What’s been bothering you the most?"
        | matchesApprox ["really confused", "so confused", "very confused"] inputLower ->
            "Confusion can be disorienting. What’s been making things unclear for you?"
        | matchesApprox ["really scared", "so scared", "very scared"] inputLower ->
            "Fear can be really paralyzing. What’s been making you feel scared?"
        | matchesApprox ["really hurt", "so hurt", "very hurt"] inputLower ->
            "I’m sorry you’re feeling hurt. Would you like to talk about what’s causing that pain?"
        | matchesApprox ["really hopeless", "so hopeless", "very hopeless"] inputLower ->
            "Hopelessness can feel really heavy. I’m here to listen if you want to share more."
        | matchesApprox ["really guilty", "so guilty", "very guilty"] inputLower ->
            "Guilt can be a hard emotion to carry. Do you want to talk about what’s making you feel that way?"
        | matchesApprox ["really ashamed", "so ashamed", "very ashamed"] inputLower ->
            "Shame can be really isolating. I’m here to listen if you want to share what’s making you feel that way."
        | matchesApprox ["really numb", "so numb", "very numb"] inputLower ->   
            "Feeling numb can be a way to cope with overwhelming emotions. Do you want to talk about what’s been going on?"
        | matchesApprox ["really bored", "so bored", "very bored"] inputLower ->
            "Boredom can be frustrating. What do you usually enjoy doing that you haven’t had time for?"
        | matchesApprox ["really unmotivated", "so unmotivated", "very unmotivated"] inputLower ->
            "It’s okay to feel unmotivated sometimes. What do you think would help you feel more energized?"
        | matchesApprox ["really anxious about exams", "so anxious about exams", "very anxious about exams"] inputLower ->
            "Exams can be really stressful. Have you tried any techniques to help manage your anxiety?"
        | matchesApprox ["really stressed about exams", "so stressed about exams", "very stressed about exams"] inputLower ->
            "Exam stress is common. Have you found any strategies that help you cope with it?"
        | matchesApprox ["really overwhelmed with assignments", "so overwhelmed with assignments", "very overwhelmed with assignments"] inputLower ->
            "Assignments can pile up quickly. Have you tried breaking them down into smaller tasks?"
        | matchesApprox ["really frustrated with group work", "so frustrated with group work", "very frustrated with group work"] inputLower ->
            "Group work can be really challenging. What’s been the hardest part for you?"
        | matchesApprox ["really confused about my major", "so confused about my major", "very confused about my major"] inputLower ->
            "It’s okay to feel uncertain about your major. Many students go through this. Want to talk about what’s making you question it?"
        | matchesApprox ["really scared about the future", "so scared about the future", "very scared about the future"] inputLower ->
            "The future can feel really uncertain. It’s okay to be scared. Want to talk about what’s worrying you the most?"        
        | matchesApprox ["problem with speaking"] inputLower ->
            "It’s okay to have trouble with speaking. You can practice your speeches in front of a mirror."
        | matchesApprox ["problem with talking", "i do not like talking", "i don't like talking", "I don't want to talk", "I do not want to talk", "I won't talk", "I will not talk"] inputLower ->
            "That's okay. I'm here whenever you're ready to share."  
        | otherwise -> 
        deterministicChoice
            [ "I understand. Please tell me more."
            , "I'm listening. Would you like to explain further?"
            , "Go on, I'm here for you."
            , "Feel free to share more if you'd like."
            , "That’s okay. I’m here to listen."
            , "I'm listening. Say whatever’s on your mind."
            , "Take your time — I’m here when you’re ready."
            , "I hear you. Would you like to continue?"
            , "Thanks for sharing that. Do you want to go deeper?"
            , "I appreciate you opening up. What else would you like to share?"
            , "Whenever you're ready, I'm here to talk."
            , "It's okay to feel that way. Want to tell me more?"
            , "Let’s take it one step at a time. I’m listening."
            , "This space is for you. Say as much or as little as you like."
            , "I'm here to support you. Would talking more help?"
            , "Whatever you're going through, I'm glad you shared that."
            , "What you’re saying matters. Go on if you’d like."
            , "I'm still with you. Keep going if you want to."
          ] input
