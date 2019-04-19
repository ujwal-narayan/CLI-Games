import Control.Monad
import Text.Read
import Data.Maybe

printSquares :: Board -> IO ()
printSquares board =
   forM_ [0..2] $ \x ->
      forM_ [0..2] $ \y ->
                       do
                         if y `mod` 3 == 0 then putStrLn "" else putStr ""
                         if y `mod` 3 == 0 then putStr "             " else putStr ""
                         putChar (((board !! x!! )y) )
                         if x `mod`3 == 0 then putStr "|" else putStr "|"
                         if x == 2 && y == 2 then do{putStrLn "\n\n\n"} else putStr ""

                      --  if turn == 'x' then printSquares $ updateMatrix board 'o' (row,col) else printSquares $ updateMatrix board 'x' (row,col)

type Board = [[Char]]

emptyBoard :: Board 
emptyBoard = ["---", "---", "---"]

updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m


loop :: Board -> Char -> IO ()
loop board turn = do
  g<-getLine
  f <- getLine
  let rowf = readMaybe g :: Maybe Int
  let colf = readMaybe f :: Maybe Int
  if isNothing rowf || isNothing colf then errorhandle board turn else
    do
     let row = fromJust rowf
     let col = fromJust colf
     if row < 0 || row > 2 then do {putStrLn "Value has to be between 0 and 2"; loop board turn} else
       do
         if col  < 0 || col > 2 then do {putStrLn "Value has to be between 0 and 2"; loop board turn} else
           do
             if checkOccupied board row col then occupiedRetry board turn else if turn == 'x' then updateMover board 'x' (row,col) 'o' else updateMover board 'o' (row,col) 'x'




updateMover board turn (row,col) next = do
  let x = updateMatrix board turn (row,col) 
  if checkWin x then winLoop else continueGame x next 


continueGame x next = do
  printSquares x
  loop x next 
checkOccupied board row col = if board !! row !! col == '-' then False else True

occupiedRetry board turn = do
  putStrLn "You cannot move there, as a piece is already there. Please try again"
  loop board turn

errorhandle board turn = do
  putStrLn "Please enter numbers only"
  loop board turn

checkWin :: Board -> Bool

checkWin board = if checkHorizontal board || checkVertical board || checkDiagonal board then True else False

checkHorizontal board = if board !! 0 == "xxx" || board !! 0 == "poo" || board !! 1 == "xxx" || board !! 1 == "ooo" || board !! 2 == "xxx" || board !! 2 == "ooo" then True else False


formVerticalPaterns board x y = if x==3 then [] else ( board !! x !! y): (formVerticalPaterns board (x+1) y) 

checkVertical board = if formVerticalPaterns board 0 0 == "xxx" || formVerticalPaterns board 0 0 == "poo" || formVerticalPaterns board 0 1 == "xxx" || formVerticalPaterns board 0 1 == "ooo"|| formVerticalPaterns board 0 2 == "xxx" || formVerticalPaterns board 0 2 == "ooo" then True else False

checkDiagonal board = if board !! 1 !! 1 /= '-' then if (board !!0 !!0 == board !! 1 !! 1 && board !! 1 !! 1 == board !! 2 !! 2 ) || (board !! 0 !! 2 == board !! 1 !! 1 && board !! 1 !! 1 == board !! 2 !! 0) then True else False else False


winLoop = do
  putStrLn "Congrats! Game has been won"
  putStrLn "Press y to replay, q to quit"
  ans <- getLine
  if ans == "y" then loop emptyBoard 'x' else gameover

gameover = putStrLn "gameover"


main = do
  printSquares emptyBoard
  putStrLn "Enter row and then Enter Col"
  loop emptyBoard 'x'
