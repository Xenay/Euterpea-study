module BossaNovaRandom where

import Euterpea
import System.Random
import Euterpea.Music (InstrumentName(AcousticGuitarNylon))

type Scale = [AbsPitch] -- Scale is a list of AbsPitch values
-- Define Scale as a type synonym for [Int]
choose :: [a] -> StdGen -> (a, StdGen)
choose [] g = error "Nothing to choose from!"
choose xs g =
    let (i,g') = next g
    in  (xs !! (i `mod` length xs), g')


randomMel :: [Pitch] -> [Dur] -> StdGen -> Music Pitch
randomMel pitches durs g0 = randomMelody (choose pitches g0) g0
  where
    -- Choose a starting note and then build the melody
    randomMelody :: (Pitch, StdGen) -> StdGen -> Music Pitch
    randomMelody (currentPitch, g) gMain =
      let (nextPitch, g1) = nextNote currentPitch pitches g
          (dur, g2) = choose durs g1
          mel = note dur nextPitch
      in mel :+: randomMelody (nextPitch, g2) gMain
    
   
    -- Decide on the next note to play based on the current note
nextNote :: Pitch -> [Pitch] -> StdGen -> (Pitch, StdGen)
nextNote currentP ps g =
  let (step, g1) = randomR (-2, 2) g -- Limit step size to create stepwise motion
      nextP = transAbsPitch step currentP -- Ensure currentP is a Pitch
  in if nextP `elem` ps
     then (nextP, g1)
     else nextNote currentP ps g1 -- Recursively call until a valid note is found

-- Transpose a pitch by a given number of semitones
transAbsPitch :: Int -> Pitch -> Pitch
transAbsPitch n (pc, oct) = pitch $ (absPitch (pc, oct)) + n

    
-- Convert an AbsPitch to a Pitch
toPitch :: AbsPitch -> Pitch
toPitch ap = pitch ap

-- The 'makeBossa' function generates Bossa Nova style music
makeBossa :: [(Dur, Scale)] -> StdGen -> Music Pitch
makeBossa [] _ = rest 0
makeBossa ((d, scale):ss) g0 =
    let sl = map toPitch (map (+ 60) scale) -- Range for melody
        br = toPitch (36 + scale !! 0) -- Root for bass
        bf = toPitch (36 + scale !! 4) -- Fifth for bass
        (g1, g2) = split g0 -- Split the generator
        mel = cut d (randomMel sl [qn, en, en, en] g1) -- Generate melody using randomMel
        bpat = note dqn br :+: note en bf :+: note dqn bf :+: note en br -- Bass pattern
        bass = cut d (forever bpat) -- Generate bass line
    in instrument AcousticGuitarNylon mel :=: instrument AcousticBass bass :+: makeBossa ss g2

-- The scales are defined here
efMaj, fMin, cMin :: Scale
efMaj = [3, 5, 7, 8, 10, 12, 14]
fMin = [5, 7, 8, 10, 12, 13, 15]
cMin = [0, 2, 3, 5, 7, 8, 10]

-- The sequence of durations and scales
myKeys :: [(Dur, Scale)]
myKeys = [(1, efMaj), (1, fMin), (2, cMin)] ++ repeat (2, cMin)

-- The main Bossa Nova piece
bossa :: Music Pitch
bossa = tempo 1.25 $ makeBossa myKeys (mkStdGen 123)
