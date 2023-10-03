import Data.Char

-- Board utilities

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
-- "all" is a Higher Order function that checks if all the elements of the list have the value indicated in the property.
finished b = all (== 0) b

valid :: Board -> Int -> Int -> Bool
valid b row number = b !! (row - 1) >= number

move :: Board -> Int -> Int -> Board
move b row number = [adjust r n | (r,n) <- zip [1..5] b]   -- The "zip" function pairs the board with the corresponding number.
                    where
                        adjust r n = if r == row then n - number else n  -- Difining the "adjust" function.


-- IO utilities

newLine :: IO ()
newLine = putChar '\n'

stars :: Int -> String
stars n = concat (replicate n "* ")   -- "replicate" function will create list of lists, which "concat" will concatinate together into a single list.

-- A helper function to print the rows of the Board.
putRow :: Int -> Int -> IO ()
putRow row number = do putStr (show row)
                       putStr ": "
                       putStrLn (stars number)

putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] = do putRow 1 a
                              putRow 2 b
                              putRow 3 c
                              putRow 4 d
                              putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newLine
                     if isDigit x then
                        return (digitToInt x)  -- "digitToInt" function casts a Char into an Integer.
                     else
                        do newLine
                           putStrLn "Wrong input, try again."
                           getDigit prompt  -- Recursively call the function "getDigit".


-- Nim game Implementation

nim :: IO ()
nim = play initial 1

next :: Int -> Int
next 1 = 2
next 2 = 1

play :: Board -> Int -> IO ()
play board player =
    do newLine
       putBoard board
       if finished board then
          do newLine
             putStr "Player "
             putStr (show (next player))
             putStrLn " wins!"
        else
          do newLine
             putStr "Player "
             putStrLn (show player)
             r <- getDigit "Enter a row number: "
             n <- getDigit "Enter stars to remove: "
             if valid board r n then
                play (move board r n) (next player)  -- If a valid move it updates the board, the player and calls the "play" function with the updated values.
             else
                do newLine
                   putStrLn "Not a valid move. Try again."
                   play board player   -- Calls the "play" function without any update in the board or the player.