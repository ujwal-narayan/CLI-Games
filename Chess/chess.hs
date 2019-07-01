--Chess Engine Ujwal Narayan -- 



----- Imports------

import Control.Monad
import Data.Maybe
import Data.Char
import Data.List


----- Data Types and Constructors ----------
data ChessPiece  = ChessPiece Color Piece Number | Blank deriving (Show,Read,Eq)
data Color = Black | White deriving (Show, Read,Eq)
data Piece = Bishop | Pawn | Rook | King | Queen| Knight deriving (Show, Read,Eq)
type Number = Int
type Board = [[ChessPiece]]

--------------------Function Definitions -------------------------------
convertToChessPiece :: Color -> Piece -> Number -> ChessPiece 
getColor :: ChessPiece -> Maybe Color
getPiece :: ChessPiece  -> Maybe Piece
getNumber :: ChessPiece -> Maybe Number
getBlackSquare :: String
getWhiteSquare :: String
blackOrWhiteSquare :: [[Color]]
printBoardSquare :: Int -> Int -> String
printSquares ::Board ->  IO()
printPiece :: Board -> Int -> Int -> String
printColor :: Board -> Int -> Int -> String
initBlackConfig :: Board
initWhiteConfig :: Board
printBoard :: Board -> IO()
initBoard :: Board
populateBoardInit  :: Board -> Board
printNewLines :: Int -> IO()
validMoves :: Board -> ChessPiece -> [(Int, Int)]
referBoard :: IO()
positionOccupied :: Board -> Int -> Int -> Bool
emptyBoardMoves :: (ChessPiece ) -> Board -> Int -> Int -> [(Int,Int)]
emptyBoardMovesKing ::   Board -> Int -> Int -> [(Int,Int)]
emptyBoardMovesQueen ::  Board -> Int -> Int -> [(Int,Int)]
emptyBoardMovesRook ::   Board -> Int -> Int -> [(Int,Int)]
emptyBoardMovesKnight :: Board -> Int -> Int ->[(Int,Int)]
emptyBoardMovesBishop :: Board -> Int -> Int -> [(Int,Int)]
emptyBoardMovesPawn ::   Board -> Int -> Int -> [(Int,Int)]
remainingPieces :: Board -> [ChessPiece]
changeOutputToBoardRep :: (Int,Int) -> (Char,Int)
changeInputToBoardRep :: (Char,Int) -> (Int,Int)




convertToChessPiece x y z = ChessPiece x y z

getColor (ChessPiece x y z ) = Just x
getColor _ = Nothing

getPiece (ChessPiece x y z ) = Just y
getPiece _ = Nothing

getNumber (ChessPiece x y z ) = Just z
getNumber _ = Nothing




getWhiteSquare= "\x1b[42m" ++ "x" ++ "\x1b[0m"

getBlackSquare = "\x1b[44m" ++ "o" ++ "\x1b[0m"

blackOrWhiteSquare = [[White,Black,White,Black,White,Black,White,Black],[Black,White,Black,White,Black,White,Black,White],[White,Black,White,Black,White,Black,White,Black],[Black,White,Black,White,Black,White,Black,White],[White,Black,White,Black,White,Black,White,Black],[Black,White,Black,White,Black,White,Black,White],[White,Black,White,Black,White,Black,White,Black],[Black,White,Black,White,Black,White,Black,White]]

printBoardSquare x y = if blackOrWhiteSquare !! x !! y == Black then getBlackSquare else getWhiteSquare

printSquares board = forM_ [0..7] $ \x ->
    do
         if (x > 0 ) then putStr "" else do
           putStrLn "   "
           putStr "          "
           putStr " "
           putStr "  "
           putStr $ unwords $ map (\x-> " " ++ [x] ++ " ") ['a'..'h']
           putStrLn ""
           putStrLn ""

         putStr "          "
         putStr " "
         putStr " "
         forM_ [0..7] $ \y ->
                            do
                                putStr $ printBoardSquare x y
                                putStr $ printBoardSquare x y
                                putStr $ printBoardSquare x y
                                putStr $ printBoardSquare x y
         putStrLn ""
         putStr "          "
         putStr $ show (8-x)
         putStr " "
         forM_ [0..7] $ \y ->
                            do
                              putStr $ printBoardSquare x y
                              putStr $ printColor board x y ++ printPiece board x y
                              putStr $ printBoardSquare x y
                              if (y < 7) then putStr "" else do
                                putStr " "
                                putStr $ show (8-x)

         putStrLn "   "
         putStr "          "
         putStr " "
         putStr " "
         forM_ [0..7] $ \y ->
                            do
                                putStr $ printBoardSquare x y
                                putStr $ printBoardSquare x y
                                putStr $ printBoardSquare x y
                                putStr $ printBoardSquare x y
         if (x < 7 ) then putStrLn "   " else do
           putStrLn "   "
           putStrLn ""
           putStr "          "
           putStr " "
           putStr "  "
           putStr $ unwords $ map (\x-> " " ++ [x] ++ " ") ['a'..'h']


printPiece board x y
  | getPiece sq == Just Pawn = "P"
  | getPiece sq == Just Rook = "R"
  | getPiece sq == Just Knight = "N"
  | getPiece sq == Just Bishop = "B"
  | getPiece sq == Just Queen = "Q"
  | getPiece sq == Just King = "K"
  | otherwise = (printBoardSquare x y)
  where sq = board !! x !! y

printColor board x y
  | getColor sq == Just Black = "B"
  | getColor sq == Just White = "W"
  | otherwise = (printBoardSquare x y)
  where sq = board !! x !! y

initBlackConfig = [[(ChessPiece Black Rook 1),(ChessPiece Black Knight 1), (ChessPiece Black Bishop 1), (ChessPiece Black Queen 1), (ChessPiece Black King 1),(ChessPiece Black Bishop 2), (ChessPiece Black Knight 2), (ChessPiece Black Rook 2)],(map (\x -> (ChessPiece Black Pawn x)) [1..8])]

initWhiteConfig = [(map (\x -> (ChessPiece White Pawn x)) [1..8]),[(ChessPiece White Rook 1),(ChessPiece White Knight 1), (ChessPiece White Bishop 1), (ChessPiece White Queen 1), (ChessPiece White King 1),(ChessPiece White Bishop 2), (ChessPiece White Knight 2), (ChessPiece White Rook 2)]]


initBoard = populateBoardInit (map (\x -> (map (\x->Blank) (take 8 [1..]))) (take 8 [1..]))

populateBoardInit board = initBlackConfig ++ (take 4 board) ++ initWhiteConfig

printNewLines x = putStrLn $ unwords $ replicate x "\n"

printBoard board = do
  printNewLines 30
  printSquares board
  printNewLines 10

referBoard = do
  let b = initBoard
  printBoard b

positionOccupied board x y = if board !! x !! y == Blank then False else True

emptyBoardMoves p board x y
  | getPiece p == Just Rook = emptyBoardMovesRook board x y
  | getPiece p == Just Bishop = emptyBoardMovesBishop board x y
  | getPiece p == Just Knight = emptyBoardMovesKnight board x y
  | getPiece p == Just Queen = emptyBoardMovesQueen board x y
  | getPiece p == Just King = emptyBoardMovesKing board x y
  | getPiece p == Just Pawn = emptyBoardMovesPawn board x y
  | otherwise = []

  


emptyBoardMovesBishop  board x y =  zip (takeWhile (<8) $ map (\y -> y + x) [1..]) (takeWhile (<8) $ map (\x -> x + y) [1..]) ++  zip (takeWhile (>=0) $ map (\y -> -1*y + x) [1..]) (takeWhile (>=0) $ map (\x -> -1*x + y) [1..]) ++   zip (takeWhile (<8) $ map (\y -> y + x) [1..]) (takeWhile (>=0) $ map (\x -> -1*x + y) [1..]) ++  zip (takeWhile (>=0) $ map (\y -> -1*y + x) [1..]) (takeWhile (<8) $ map (\x -> x + y) [1..])

emptyBoardMovesRook board x y =  zip (takeWhile (<8) $ map (\y -> y + x) [1..]) (map (\x -> y) [1..]) ++  zip (takeWhile (>=0) $ map (\y -> -1*y + x) [1..]) ((map (\x -> y) [1..])) ++   zip (map (\y -> x) [1..]) (takeWhile (>=0) $ map (\x -> -1*x + y) [1..]) ++  zip (map (\y -> x) [1..]) (takeWhile (<8) $ map (\x -> x + y) [1..])

emptyBoardMovesQueen board x y = emptyBoardMovesBishop board x y ++ emptyBoardMovesRook board x y

emptyBoardMovesKing board x y = filter (\(u,v) -> ((u-x) == 1 || (u-x) == -1 || (v-y) ==1 || (v-y) == -1)) $ emptyBoardMovesQueen board x y

emptyBoardMovesKnight board x y = filter (\(u,v) -> (0<=u && u<=9 && 0<=v && v<=7)) [(x+2,y+1),(x-2,y+1),(x+2,y-1),(x-2,y-1),(x+1,y+2),(x-1,y+2),(x+1,y-2),(x-1,y-2)]

emptyBoardMovesPawn board x y = if ((getColor $ initBoard  !! x !! y) == Just Black) then [(x+1,y),(x+2,y)] else [(x-1,y),(x-2,y)]


changeInputToBoardRep (x,y)= (8-y,fromJust $ elemIndex x ['a'..'z'] )

changeOutputToBoardRep (x,y) = ((['a'..'z'] !! y) ,8-x )

remainingPieces board = filter (/=Blank) $ concat board 

-- validMoves board p 
--   | p == Queen = 
validMoves board p = [(0,0)]
--Placeholder text 