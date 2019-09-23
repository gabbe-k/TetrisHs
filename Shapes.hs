-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Data.List(transpose)
import Data.Maybe(isNothing)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

data Shape = S [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]
    
    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r


-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of connected 4 blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes] 
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes = 
              [["I",
               "I",
               "I",
               "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions

--HUR MAN GÖR EN SHAPE: 
-- S [[Just Black],[Just Red]]
-- S [[Nothing]]
-- S 

-- ** A01
--Prints out an empty shape
--x represents the num of column for each row and y the num of rows
emptyShape :: (Int,Int) -> Shape
emptyShape (x,y) = S (replicate y (replicate x Nothing) )

-- ** A02

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int,Int)
shapeSize (S rows) = (length(rows !! 0) ,length rows)

-- ** A03

-- | Count how many non-empty squares a shape contains
aS = S [[Just Black, Nothing],[Just Red]]

blockCount :: Shape -> Int
blockCount (S rows) = (x*y) - sum [ 1 | s <- rows, s == [Nothing]]
        where (x,y) = shapeSize (S rows)

-- * The Shape invariant

-- ** A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
--prop_Shape :: Shape -> Bool
prop_Shape (S rows) | S rows == 0 = True
                    | firstCol == (lengthRows !! 0) = prop_Shape(S (tail rows))
                    | otherwise = False
        where 
        firstCol = (rows !! 0)
        lengthRows = [ length col | col <- rows ]

-- * Test data generators

-- ** A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = error "A05 rColour undefined"

instance Arbitrary Colour where
  arbitrary = rColour

-- ** A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = error "A06 rShape undefined"


instance Arbitrary Shape where
  arbitrary = rShape

-- * Transforming shapes

-- ** A07
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape = error "A07 rotateShape undefined"

-- ** A08
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape = error "A08 shiftShape undefined"

-- ** A09
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int,Int) -> Shape -> Shape
padShape = error "A09 padShape undefined"

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo = error "A10 padShapeTo undefined"

-- * Comparing and combining shapes

-- ** B01

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = error "A11 overlaps undefined"

-- ** B02
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith = error "A12 zipShapeWith undefined"

-- ** B03
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = error "A13 zipShapeWith undefined"
