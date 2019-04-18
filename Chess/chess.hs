data ChessPiece x y z = ChessPiece x y z  | Blank deriving (Show,Read)
data Color = Black | White deriving (Show, Read)
data Piece = Bishop | Pawn | Rook | King | Queen| Knight deriving (Show, Read)
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
