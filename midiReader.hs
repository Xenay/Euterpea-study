import Euterpea
import Codec.Midi (importFile)

-- Function to read a MIDI file and convert it to a Music1 structure
readMidi :: FilePath -> IO (Maybe (Music1))
readMidi path = do
    result <- importFile path
    case result of
        Right midi -> return $ Just (fromMidi midi)
        Left _ -> return Nothing

main :: IO ()
main = do
    music <- readMidi "Driftveil.mid"  
    case music of
        Just m -> play m  -- Play the music
        Nothing -> putStrLn "Error reading MIDI file."
