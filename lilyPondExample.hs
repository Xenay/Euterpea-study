import System.IO

-- Function to generate the LilyPond content
generateLilyPondContent :: String
generateLilyPondContent = unlines [
    "\\version \"2.18.2\"",
    "\\header {",
    "  title = \"A Simple Melody\"",
    "  composer = \"Composer Name\"",
    "}",
    "\\score {",
    "  \\relative c' {",
    "    \\key c \\major",
    "    \\time 4/4",
    "    c4 d e f | g a b c |",
    "  }",
    "  \\layout { }",
    "  \\midi { }",
    "}"
  ]

-- Function to write the content to a file
writeToFile :: FilePath -> IO ()
writeToFile filePath = do
    let content = generateLilyPondContent
    writeFile filePath content
    putStrLn $ "LilyPond file written to " ++ filePath

main :: IO ()
main = writeToFile "simpleMelody.ly"