-- | The Tetris game (main module)
module Main where
import ConsoleGUI       -- cabal install ansi-terminal 
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation


-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int,Int)

-- | The size of the well
wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)
wellWidth = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s


-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (_,s) w _) = (prop_Shape s) && (shapeSize w == wellSize)

-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls (S rows) = S $ br ++ [[w] ++ r ++ [w] | r <- rows] ++ br
  where 
    w   = Just Black
    x   = 2 + (fst $ shapeSize (S rows))
    br  = [(replicate x w)]

-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v,p) w _) = addWalls $ w `combine` s
                      where s = place (v,p)

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,nwShp) (emptyShape wellSize) (supply)
  where
    index n = floor (n * 7)
    nwShp = head shList
    supply = tail shList
    shList = [ allShapes !! (index n) | n <- rs]
    
-- | Moves the falling piece to a new relative x,y position
move :: Vector -> Tetris -> Tetris
move v1 (Tetris (v2,s) w sList) = Tetris (v,s) w sList
  where v = vAdd v1 v2

-- | Rotates the falling piece 90 degrees counter clockwise
rotate :: Tetris -> Tetris
rotate (Tetris (v,s0) w sList) = (Tetris (v,s) w sList)
  where s = rotateShape s0
  
-- | Checks if a piece has collided with the well / other shapes
collision' :: Tetris -> [Bool]
collision' (Tetris (v,s0) w sList) = colList
  where 
    (x,y)   = v
    s       = place (v,s0)
    (sw,sh) = shapeSize s
    (ww,wh) = shapeSize w
    cRgtLft = sw > ww || x < 0
    cDwn    = sh > wh
    cSha    = s `overlaps` w 
    colList = [cDwn,cSha,cRgtLft]

-- | Checks if part of piece is outside of well, or overlaps anything in the well
collision :: Tetris -> Bool
collision t = or (collision' t) 

-- | Updates the game, makes the falling shape move, or stops it
tick :: Tetris -> Maybe (Int,Tetris)
tick t0 | (pieceIsDown) && (collision t) = dropNewPiece t0
        | collision t                    = Just(0,t0)
        | otherwise                      = Just (0,t)
    where 
    t           = move (0,1) t0
    pieceIsDown = ((collision' t) !! 0) || ((collision' t) !! 1)
    
-- | Moves piece x,y coordinates
movePiece :: (Int, Int) -> Tetris -> Tetris
movePiece (mX, mY) t0 | collision t = t0
                      | otherwise   = t
  where t = move (mX,mY) t0

-- | Rotates piece if it doesnt collide after the rotate
rotatePiece :: Tetris -> Tetris
rotatePiece t0 | collision t = t0
               | otherwise   = t
  where t = rotate t0

-- | Drops a new piece. If the new piece is blocked = Game Over
dropNewPiece :: Tetris -> Maybe (Int,Tetris)
dropNewPiece (Tetris (v,s0) w0 sList0) 
             | gOver     = Nothing
             | otherwise = Just (p, (Tetris (startPosition,s) clw sList))
  where 
  w = w0 `combine` (place (v,s0))
  (p,clw) = clearRow w
  s = head sList0
  sList = tail sList0
  gOver = place(startPosition,s) `overlaps` w

-- | Checks if a row is full. Filter only returns nothing
-- | If the length of the returned list is 0, then row is full
rowFull :: Row -> Bool
rowFull r = length(filter (\x -> (x == Nothing)) r) == 0

-- | Returns all rows that arent full, then adds 
-- | Just as many empty rows on top of the rowlist
clearRows :: [Row] -> (Int,[Row])
clearRows rows = (deltaRows, newWell)
    where deltaRows = wellHeight - (length newRows)
          newRows = [ r | r <- rows, not (rowFull r)]
          newWell = (replicate deltaRows emptyRow) ++ newRows
          emptyRow = replicate wellWidth (Nothing)

-- | Clears all full rows in a well
clearRow :: Shape -> (Int,Shape)
clearRow (S wr) = (p,(S r))
    where (p,r) = clearRows wr

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris MoveLeft  t = Just (0, (movePiece (-1, 0) t))
stepTetris MoveRight t = Just (0, (movePiece (1, 0) t))
stepTetris MoveDown  t = Just (0, (movePiece (0, 1) t))
stepTetris Rotate    t = Just (0, (rotatePiece t))
stepTetris _ t         = tick t

