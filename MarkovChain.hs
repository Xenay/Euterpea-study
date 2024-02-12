import Euterpea
import System.Random
import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Monad (when)


type MarkovChain = [(Pitch, [(Pitch, Double)])]

-- Define a simple Markov chain for a E minor scale within one octave
markovChainEminor  :: MarkovChain
markovChainEminor = 
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

--octave shenanigans
octUp :: Music Pitch -> Music Pitch
octUp (Prim (Note d (p,o))) = note d (p, o+1)
octUp (Prim (Rest d)) = rest d
octUp (m1 :+: m2) = octUp m1 :+: octUp m2
octUp (m1 :=: m2) = octUp m1 :=: octUp m2
octUp (Modify c m) = Modify c (octUp m)

octDown :: Music Pitch -> Music Pitch
octDown (Prim (Note d (p,o))) = note d (p, o-1)
octDown (Prim (Rest d)) = rest d
octDown (m1 :+: m2) = octDown m1 :+: octDown m2
octDown (m1 :=: m2) = octDown m1 :=: octDown m2
octDown (Modify c m) = Modify c (octDown m)

adjustOctave :: Music Pitch -> Int -> Music Pitch
adjustOctave music n
    | n > 0 = iterate octUp music !! n
    | n < 0 = iterate octDown music !! abs n
    | otherwise = music

--tempo shenanigans
adjustTempo :: Double -> Music Pitch -> Music Pitch
adjustTempo bpm music = tempo (fromRational . toRational $ bpm / 120) music

main :: IO ()
main = do
    putStrLn "Enter octave change (positive for up, negative for down): "
    octaveChange <- readLn
    putStrLn "Enter desired tempo in BPM: "
    bpm <- readLn

    gen <- newStdGen
    let melody = generateMelody markovChainEminor 32 (E, 4) gen
        bassLine = generateBassLine 16
        music = adjustOctave (melody :=: bassLine) octaveChange
        finalMusic = adjustTempo bpm music

    play finalMusic