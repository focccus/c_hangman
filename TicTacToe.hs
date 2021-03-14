
import Data.Char (toLower)
import Control.Monad ( when )
import Data.List (intercalate, intersperse)

data Player = X | O | E deriving (Eq)

size = 3

instance Show Player where
    show X = "X"
    show O = "O"
    show E = " "

type Field = [Player]

empty = replicate (size ^ 2) E

isEmpty E = True
isEmpty _ = False

rows [] = []
rows xs = take size xs : rows (drop size xs)

cols = helper . rows
    where
        helper xs | any null xs = []
        helper xs = map head xs : helper (map tail xs)

diagonalFromRows [] = []
diagonalFromRows (x:xs) = head x : diagonalFromRows (map tail xs)

diagonal = diagonalFromRows . rows
counterDiagonal = diagonalFromRows. map reverse . rows

isSame [] = True
isSame (x:xs) = not (isEmpty x) && all (==x) xs

isLegal field i
    | i < 1 || i > size * size = "Selected Field is not in range"
    | isEmpty $ field !! (i - 1) = ""
    | otherwise = "Selected Field already has a value"

hasWon field = any isSame (
        rows field ++
        cols field ++
        [diagonal field] ++
        [counterDiagonal field]
    )



replaceAt field p i = [if i == j then p else x | (x,j) <- zip field [1..]]

switchPlayer X = O
switchPlayer O = X

showRow :: [Player] -> String
showRow xs = ' ' : intercalate " | " (map show xs) ++ " "

showDivider = intercalate "+" $ replicate size "---"

showField = unlines . intersperse showDivider . map showRow . rows

clear = putStr "\ESC[2J"
play player field = do
                   
                    putStrLn $ "It's your turn, " ++ show player

                    putStrLn "Field: "
                    input <- getLine
                    let i = read input :: Int
                    
                    let legal = isLegal field i
                    if null legal then do
                        let f = replaceAt field player i
                        putStr $ showField f
                        if hasWon f then
                            putStrLn ("Player " ++ show player ++ " has won!")
                        else if not (any isEmpty f) then
                            putStrLn "It's a tie!"
                        else
                            play (switchPlayer player) f
                    else do
                        putStrLn legal
                        play player field

main = do
        play X empty
        putStrLn "Do you want to play again? [Y/N] "
        ch <- getChar
        getChar
        when (toLower ch == 'y') main