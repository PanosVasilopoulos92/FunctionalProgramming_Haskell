import System.IO
                                              -- Hangman Game

-- The action "getCh" reads a single character from the keyboard, without echoing it in the screen.
getCh :: IO Char
getCh = do hSetEcho stdin False  -- Makes echo Off.
           x <- getChar
           hSetEcho stdin True   -- Turns echo back on.
           return x


-- The action "sgetLine" reads a line of text from the keybord, echoing each
-- character as a dash.
sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

-- The helper function "match".
match :: String -> String -> String
match xs ys =
  [if elem x ys then x else '-' | x <- xs]


-- The function "play" is the main loop, which requests and processes guesses until the game ends.
play :: String -> IO ()
play word = 
  do putStr "? "
     guess <- getLine
     if guess == word then
        putStrLn "You got it!"
     else
        do putStrLn (match word guess)
           play word


hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it"
             play word

