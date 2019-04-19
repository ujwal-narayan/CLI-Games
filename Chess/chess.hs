import Control.Monad
import Data.Maybe


data ChessPiece x y z = ChessPiece x y z  | Blank deriving (Show,Read,Eq)
data Color = Black | White deriving (Show, Read,Eq)
data Piece = Bishop | Pawn | Rook | King | Queen| Knight deriving (Show, Read,Eq)
type Number = Int

convertToChessPiece :: Color -> Piece -> Int -> ChessPiece Color Piece Int
convertToChessPiece x y z = ChessPiece x y z

getColor :: ChessPiece Color Piece Number -> Maybe Color
getColor (ChessPiece x y z ) = Just x
getColor _ = Nothing

getPiece :: ChessPiece Color Piece Number -> Maybe Piece
getPiece (ChessPiece x y z ) = Just y
getPiece _ = Nothing

getNumber :: ChessPiece Color Piece Number -> Maybe Number
getNumber (ChessPiece x y z ) = Just z
getNumber _ = Nothing

type Board = [[ChessPiece Color Piece Number ]]


getBlackSquare :: String
getBlackSquare = "\x1b[42m" ++ "x" ++ "\x1b[0m"

getWhiteSquare :: String
getWhiteSquare = "\x1b[44m" ++ "o" ++ "\x1b[0m"

blackOrWhiteSquare :: [[Color]]
blackOrWhiteSquare = [[White,Black,White,Black,White,Black,White,Black],[Black,White,Black,White,Black,White,Black,White],[White,Black,White,Black,White,Black,White,Black],[Black,White,Black,White,Black,White,Black,White],[White,Black,White,Black,White,Black,White,Black],[Black,White,Black,White,Black,White,Black,White],[White,Black,White,Black,White,Black,White,Black],[Black,White,Black,White,Black,White,Black,White]]

printBoardSquare :: Int -> Int -> String
printBoardSquare x y = if blackOrWhiteSquare !! x !! y == Black then getBlackSquare else getWhiteSquare

printSquares ::Board ->  IO()
printSquares board = forM_ [0..7] $ \x -> 
    do
         putStr "          "
         forM_ [0..7] $ \y ->
                            do
                                putStr $ printBoardSquare x y
                                putStr $ printBoardSquare x y
                                putStr $ printBoardSquare x y
                                putStr $ printBoardSquare x y
         putStrLn ""
         putStr "          "
         forM_ [0..7] $ \y ->
                            do
                              putStr $ printBoardSquare x y
                              putStr $ printColor board x y ++ printPiece board x y
                              putStr $ printBoardSquare x y
         putStrLn "   "
         putStr "          "
         forM_ [0..7] $ \y ->
                            do
                                putStr $ printBoardSquare x y
                                putStr $ printBoardSquare x y
                                putStr $ printBoardSquare x y
                                putStr $ printBoardSquare x y
         putStrLn "   "

printPiece :: Board -> Int -> Int -> String
printPiece board x y
  | getPiece sq == Just Pawn = "P"
  | getPiece sq == Just Rook = "R"
  | getPiece sq == Just Knight = "N"
  | getPiece sq == Just Bishop = "B"
  | getPiece sq == Just Queen = "Q"
  | getPiece sq == Just King = "K"
  | otherwise = (printBoardSquare x y)
  where sq = board !! x !! y

printColor :: Board -> Int -> Int -> String
printColor board x y
  | getColor sq == Just Black = "B"
  | getColor sq == Just White = "W"
  | otherwise = (printBoardSquare x y)
  where sq = board !! x !! y

initBlackConfig :: Board
initBlackConfig = [[(ChessPiece Black Rook 1),(ChessPiece Black Knight 1), (ChessPiece Black Bishop 1), (ChessPiece Black Queen 1), (ChessPiece Black King 1),(ChessPiece Black Bishop 2), (ChessPiece Black Knight 2), (ChessPiece Black Rook 2)],(map (\x -> (ChessPiece Black Pawn x)) [1..8])]

initWhiteConfig :: Board
initWhiteConfig = [(map (\x -> (ChessPiece White Pawn x)) [1..8]),[(ChessPiece White Rook 1),(ChessPiece White Knight 1), (ChessPiece White Bishop 1), (ChessPiece White Queen 1), (ChessPiece White King 1),(ChessPiece White Bishop 2), (ChessPiece White Knight 2), (ChessPiece White Rook 2)]]


initBoard :: Board
initBoard = populateBoardInit (map (\x -> (map (\x->Blank) (take 8 [1..]))) (take 8 [1..]))

populateBoardInit  :: Board -> Board
populateBoardInit board = initBlackConfig ++ (take 4 board) ++ initWhiteConfig
