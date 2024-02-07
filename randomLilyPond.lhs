
> module RandomLilyPond where
> import Euterpea
> import System.Random


> choose :: [a] -> StdGen -> (a, StdGen)
> choose [] g = error "Nothing to choose from!"
> choose xs g = 
>     let (i,g') = next g
>     in  (xs !! (i `mod` length xs), g')

Random melody which has been altered for simplicity

> randomMel :: [AbsPitch] -> [Dur] -> Volume -> Int -> StdGen -> Music (AbsPitch, Volume)
> randomMel _ _ _ 0 _ = rest qn -- Use a rest with a valid, non-zero duration
> randomMel pitches durs thresh n g0 = 
>    let (p, g1) = choose pitches g0 
>        (d, g2) = choose durs g1 
>        (v, g3) = randomR (0, 127) g2
>        x = if v < thresh then rest d else note d (p, v)
>    in  x :+: randomMel pitches durs thresh (n - 1) g3


now lets make it needlesly complicated!

> musicToLilyPond :: Music (AbsPitch, Volume) -> String
> musicToLilyPond m = case m of
>    Prim (Note d (p, _)) -> pitchToLilyPond p ++ durationToLilyPond d ++ " "
>    Prim (Rest d) -> "r" ++ durationToLilyPond d ++ " "
>    m1 :+: m2 -> musicToLilyPond m1 ++ musicToLilyPond m2
>    _ -> ""

> writeToLilyPondFile :: Music (AbsPitch, Volume) -> IO ()
> writeToLilyPondFile music = do
>    let melody = musicToLilyPond music
>    let header = "\\version \"2.18.2\" \n\\header { title = \"Algorithmic Melody\" }\n"
>    let content = header ++ "\\score {\n \\relative c { " ++ melody ++ "}\n \\midi {}\n \\layout {} \n}"
>    writeFile "algorithmicMelody.ly" content
>    putStrLn "LilyPond file written."

> durationToLilyPond :: Dur -> String
> durationToLilyPond d
>    | d == wn = "1"
>    | d == hn = "2"
>    | d == qn = "4"
>    | d == en = "8"
>    | d == sn = "16"
>    | d == tn = "32"  
>    | d == dwn = "1." 
>    | d == dhn = "2." 
>    | d == dqn = "4." 
>    | d == den = "8."
>    | d == dsn = "16."
>    | otherwise = error $ "Unsupported duration: " ++ show d

> pitchToLilyPond :: AbsPitch -> String
> pitchToLilyPond p = case p of
>    60 -> "c"
>    62 -> "d"
>    63 -> "ees"  
>    65 -> "f"
>    67 -> "g"
>    68 -> "aes"  
>    70 -> "bes"  
>    72 -> "c'"
>    74 -> "d'"
>    75 -> "ees'" 
>    77 -> "f'"
>    79 -> "g'"
>    _  -> error "Note out of scale" 


> main = do
>    let pitches = [60, 62, 63, 65, 67, 68, 70, 72, 74, 75, 77, 79] -- C-minor scale pitches
>    let g = mkStdGen 42 -- Seed for randomness
>    let melody = randomMel pitches [qn, en] 80 20 g -- Generate a melody with 20 notes/rests
>    play melody
>    writeToLilyPondFile melody