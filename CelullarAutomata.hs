import Euterpea
import Data.List (mapAccumL)
import System.Random

-- Rule definitions
rule90, rule30, rule110, rule54, rule184 :: Bool -> Bool -> Bool -> Bool

rule90 left current right = left /= right

rule30 left current right =
    case (left, current, right) of
        (False, False, True) -> True
        (False, True, True) -> True
        (True, False, False) -> True
        (True, False, True) -> True
        _ -> False

rule110 left current right =
    case (left, current, right) of
        (True, True, True)   -> False
        (True, True, False)  -> True
        (True, False, True)  -> True
        (True, False, False) -> False
        (False, True, True)  -> True
        (False, True, False) -> True
        (False, False, True) -> True
        (False, False, False) -> False

rule54 left current right =
    case (left, current, right) of
        (True, False, True) -> True
        (True, False, False) -> True
        (False, True, False) -> True
        (False, False, True) -> True
        _ -> False

rule184 left current right =
    case (left, current, right) of
        (True, True, True) -> True
        (True, True, False) -> False
        (True, False, True) -> True
        (True, False, False) -> True
        (False, True, True) -> True
        (False, True, False) -> False
        (False, False, True) -> False
        (False, False, False) -> False
        _ -> False

-- Scale definition
eMinorScale :: [Pitch]
eMinorScale = [(E,4), (Fs,4), (G,4), (A,4), (B,4), (C,5), (D,5), (E,5)]

boolsToMusic :: [Bool] -> [Pitch] -> Music Pitch
boolsToMusic bools scale = line . concat $ zipWith (\b p -> if b then [note en p] else [rest sn]) bools (cycle scale)

minorTriad :: Pitch -> Music Pitch
minorTriad p = chord [note en p, note en (trans 3 p), note en (trans 7 p)]
  where
    trans n p = pitch (absPitch p + n)

-- Generate the next state of the automaton based on a rule
nextState :: (Bool -> Bool -> Bool -> Bool) -> [Bool] -> [Bool]
nextState rule state = zipWith3 rule (False : init state) state (tail state ++ [False])

-- Generate the automaton states using a selected rule
generateCAWithRule :: (Bool -> Bool -> Bool -> Bool) -> Int -> [[Bool]]
generateCAWithRule rule width = take n $ iterate (nextState rule) initial
  where
    n = 32  -- Number of generations to generate
    initial = [False, False] ++ replicate (width - 4) True ++ [False, False]  -- Initial state

generateCA :: (Bool -> Bool -> Bool -> Bool) -> Int -> [[Bool]]
generateCA rule n = take n $ iterate (nextState rule) initial
  where
    initial = replicate (n `div` 2) False ++ [True] ++ replicate (n `div` 2) False

-- Visualize a single generation of the CA
visualizeGeneration :: [Bool] -> String
visualizeGeneration = map (\b -> if b then '#' else '.')

-- Visualize the entire CA
visualizeCA :: [[Bool]] -> IO ()
visualizeCA = mapM_ (putStrLn . visualizeGeneration)

-- Convert CA states to music
caToMusic :: [[Bool]] -> [Pitch] -> Music Pitch
caToMusic caStates scale = foldr ((:+:) . (`boolsToMusic` scale)) (rest 0) caStates

-- Main function with rule selection
main :: IO ()
main = do
    putStrLn "Select a rule for generating the cellular automaton:"
    putStrLn "1. Rule 30 - Complex patterns"
    putStrLn "2. Rule 90 - Sierpinski triangle"
    putStrLn "3. Rule 110 - Supports universal computation"
    putStrLn "4. Rule 54 - Like rule 90, but more compact"
    putStrLn "5. Rule 184 - Traffic flow simulation"
    ruleNum <- getLine

    let rule = case ruleNum of
                  "1" -> rule30
                  "2" -> rule90
                  "3" -> rule110
                  "4" -> rule54
                  "5" -> rule184
                  _   -> rule90  -- Default to rule90 if invalid input

    putStrLn "Choose CA generation method:"
    putStrLn "1. Simple CA generation (centered single True)"
    putStrLn "2. Advanced CA generation (custom initial state and rule)"
    genMethod <- getLine

    -- Both generateCA and generateCAWithRule now correctly take a rule function as an argument.
    let caStates = case genMethod of
                      "1" -> generateCA rule 32  -- Now correctly applies `rule` as an argument
                      "2" -> generateCAWithRule rule 32
                      _   -> generateCA rule 32  -- Default to simple CA generation if invalid input

    visualizeCA caStates
    let music = caToMusic caStates eMinorScale
    play music
