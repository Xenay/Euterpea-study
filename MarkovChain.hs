import Euterpea
import System.Random
import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Monad (when)


type MarkovChain = [(Pitch, [(Pitch, Double)])]

-- Define a simple Markov chain for a E minor scale within one octave
markovChainEminor  :: MarkovChain
markovChainEminor  = 
    [((E, 4), [((Fs, 4), 0.8), ((G, 4), 0.2)]),
     ((Fs, 4), [((E, 4), 0.3), ((G, 4), 0.7)]),
     ((G, 4), [((Fs, 4), 0.2), ((A, 4), 0.8)]),
     ((A, 4), [((G, 4), 0.3), ((B, 4), 0.7)]),
     ((B, 4), [((A, 4), 0.4), ((C, 5), 0.6)]),
     ((C, 5), [((B, 4), 0.5), ((D, 5), 0.5)]),
     ((D, 5), [((C, 5), 0.6), ((E, 5), 0.4)]),
     ((E, 5), [((D, 5), 0.5), ((Fs, 5), 0.5)]),
     ((Fs, 5), [((E, 5), 0.7), ((G, 5), 0.3)]),
     ((G, 5), [((Fs, 5), 0.8), ((A, 5), 0.2)])]

-- List of possible durations
possibleDurations :: [Dur]
possibleDurations = [hn, qn, en]

-- Function to randomly select a duration
randomDuration :: [Dur] -> StdGen -> (Dur, StdGen)
randomDuration durs gen =
    let (index, newGen) = randomR (0, length durs - 1) gen
    in (durs !! index, newGen)

-- Function to find the next pitch based on the current pitch and the Markov chain
findNextPitch :: Pitch -> MarkovChain -> StdGen -> (Pitch, StdGen)
findNextPitch currentPitch chain gen = 
    let possibilities = fromMaybe [] (lookup currentPitch chain)
        (randValue, newGen) = randomR (0.0, 1.0 :: Double) gen
        nextPitch = fromMaybe currentPitch (selectPitch randValue possibilities)
    in (nextPitch, newGen)

-- Function to select the next pitch based on a random value and accumulated probabilities
selectPitch :: Double -> [(Pitch, Double)] -> Maybe Pitch
selectPitch randValue possibilities =
    let accumPossibilities = scanl1 (\(_, accProb) (p, prob) -> (p, accProb + prob)) possibilities
    in fmap fst . find ((randValue <=) . snd) $ accumPossibilities

-- Generate a melody using the Markov chain, starting from a given pitch
generateMelody :: MarkovChain -> Int -> Pitch -> StdGen -> Music Pitch
generateMelody _ 0 _ _ = rest 0
generateMelody chain numNotes currentPitch gen =
    let (nextPitch, gen1) = findNextPitch currentPitch chain gen
        (dur, gen2) = randomDuration possibleDurations gen1
        noteWithDur = note dur nextPitch
    in noteWithDur :+: generateMelody chain (numNotes - 1) nextPitch gen2

--bass
bassChords :: [Pitch]
bassChords = [(E, 2), (G, 2), (B, 2)]

generateBassLine :: Int -> Music Pitch
generateBassLine n = line . take n . cycle $ map (\p -> note wn p) bassChords

generateMusic :: MarkovChain -> Int -> Pitch -> StdGen -> Music Pitch
generateMusic chain numNotes startingPitch gen = 
    let melody = generateMelody chain numNotes startingPitch gen
        bassLine = generateBassLine numNotes
    in melody :=: bassLine



main :: IO ()
main = do
    gen <- newStdGen
    let music = generateMusic markovChainEminor 32 (E, 4) gen
    play music