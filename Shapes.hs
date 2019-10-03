-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Data.List
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
blockCount :: Shape -> Int
blockCount (S rows) = (x*y) - sum [ 1 | s <- rows, i <- s, i == Nothing]
        where (x,y) = shapeSize (S rows)

-- * The Shape invariant

-- ** A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape (S rows) | length rows > 0 && firstRowLen > 0 = 
                      length rows == numSameCol 
  where 
  firstRowLen   = length (head rows)
  numSameCol = sum [ 1 | row <- rows, length row == firstRowLen]

-- * Test data generators

-- ** A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = elements [Black ..]

instance Arbitrary Colour where
  arbitrary = rColour

-- ** A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = elements allShapes


instance Arbitrary Shape where
  arbitrary = rShape

-- * Transforming shapes

-- ** A07
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (S r) = S [x | x <- rs]
    where 
      rs = reverse(transpose r) 
  
-- ** A08
-- | shiftShape adds empty squares above and to the left of the shape
shiftY :: Int -> Shape -> Shape
shiftY y (S rows) = S ((replicate y (replicate x Nothing)) ++ rows)
  where 
  x = fst(shapeSize(S rows))

shiftX :: Int -> Shape -> Shape
shiftX x (S rows) = S [(replicate x Nothing) ++ row | row <- rows]

shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (x,y) sh = shiftX x (shiftY y sh)

-- ** A09
-- | padShape adds empty square below and to the right of the shape
padShape :: (Int,Int) -> Shape -> Shape
padShape (x,y) sh = rotateTwice ( shiftShape (x,y) (rotateTwice sh) )
  where
  rotateTwice(x) = (rotateShape . rotateShape) x

-- ** A10
-- -- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (xdim,ydim) sh = padShape (x,y) sh
    where 
    (w,h) = shapeSize(sh) 
    x     = xdim - w
    y     = ydim - h

-- * Comparing and combining shapes

-- ** B01

-- | Test if two shapes overlap
rowsOverlap :: Row -> Row -> Bool
rowsOverlap [] _        = False
rowsOverlap (x:xs) (y:ys) | (x == Nothing) || (y == Nothing) = rowsOverlap xs ys
                          | otherwise = True

--does not work
-- rowsOverlap [] _        = False
-- rowsOverlap (Nothing:xs) _   = False
-- rowsOverlap s s2        = True

overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = or [rowsOverlap r1 r2 | r1 <- rows(s1), r2 <- rows(s2)]

-- data Shape = S [Row] deriving (Eq)
-- type Row = [Square]

-- -- ** B02
-- -- -- | zipShapeWith, like 'zipWith' for lists
-- zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
-- zipShapeWith f (S rows1) (S rows2) = S ([zipRowWith f r1 r2 | i <- [0..(len - 1)]
--                                         , r1 <- (rows1 !! i), r2 <- (rows2 !! i)])
--   where len = (length rows1) `min` (length rows2)

zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith f (S r1) (S r2) = S (zipRows f r1 r2)

zipRows :: (Square -> Square -> Square) -> [Row] -> [Row] -> [Row]
zipRows f (x:xs) (y:ys) = (zipRowWith f x y) : zipRows f xs ys
zipRows _ _      _      = []

zipRowWith :: (Square -> Square -> Square) -> Row -> Row -> Row
zipRowWith f (x:xs) (y:ys) = (f x y) : zipRowWith f xs ys
zipRowWith _ _      _      = []

blackClashes :: Shape -> Shape -> Shape
blackClashes s1 s2 = zipShapeWith clash s1 s2  
  where clash :: Square -> Square -> Square 
        clash Nothing Nothing = Nothing
        clash Nothing s       = s
        clash s       Nothing = s
        clash (Just c1) (Just c2) = Just Black

-- ** B03
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
(S (x:xs)) `combine` (S (y:ys)) | rowsOverlap x y = (S (xs)) `combine` (add1Col (y:ys))
                                | otherwise = (S (xs)) `combine` (S (ys))
  where add1Col r1 = shiftShape (1,0) (S r1)
 

-- combine :: Shape -> Shape -> Shape
-- (S (x:xs)) `combine` (S (y:ys)) | rowsOverlap x y = zipRowWith (++) x y
--                                 | otherwise = (S xs) `combine` (S ys)
--   where colCheck :: Square -> Square -> Square
--         colCheck Nothing s       = s
--         colCheck s       Nothing = s
--         colCheck (Just c1) (Just c2) = Just Black
--         add1Col r1 = shiftShape (1,0) (S r1)


r1 = rows(allShapes !! 5)
r2 = rows(allShapes !! 4)
  