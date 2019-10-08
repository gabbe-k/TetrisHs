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
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = repeat (allShapes!!1) -- incomplete !!!
    
-- | Moves the falling piece to a new relative x,y position
move :: Vector -> Tetris -> Tetris
move v1 (Tetris (v2,s) w sList) = Tetris (v,s) w sList
  where v = vAdd v1 v2

rotate :: Tetris -> Tetris
rotate (Tetris (v,s0) w sList) = (Tetris (v,s) w sList)
  where s = rotateShape s0
  
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
tick t0 | (pieceIsDown) && (collision t)  = dropNewPiece t0
        | collision t = Just(0,t0)
        | otherwise   = Just (0,t)
    where 
    t   = move (0,1) t0
    pieceIsDown = ((collision' t) !! 0) || ((collision' t) !! 1)
     
movePiece :: Int -> Tetris -> Tetris
movePiece m t0 | collision t = t0
               | otherwise   = t
  where t = move (m,0) t0

rotatePiece :: Tetris -> Tetris
rotatePiece t0 | collision t = t0
               | otherwise   = t
  where t = rotate t0

dropNewPiece :: Tetris -> Maybe (Int,Tetris)
dropNewPiece (Tetris (v,s0) w0 sList0) 
             | gOver = Nothing
             | otherwise = Just (0, (Tetris (startPosition,s) w sList))
  where 
  w = w0 `combine` (place (v,s0))
  s = head sList0
  sList = tail sList0
  gOver = place(startPosition,s) `overlaps` w

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris MoveLeft  t = Just (0, (movePiece (-1) t))
stepTetris MoveRight t = Just (0, (movePiece (1) t))
stepTetris MoveDown  t = tick (move (0,0) t)
stepTetris Rotate    t = Just (0, (rotatePiece t))
stepTetris _ t         = tick t

s1  = addWalls(emptyShape(10,20))
s2  = shiftShape (4,18) (allShapes !! 2)
h = s1 `combine` s2
h2 = shiftShape (4,1) (allShapes !! 2)

