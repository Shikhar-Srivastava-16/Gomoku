module Board where
import Debug.Trace

data Col = Black | White
  deriving Show

other :: Col -> Col
other Black = White
other White = Black

type Position = (Float, Float)

-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list 
-- of pairs of position and the colour at that position.  So a 10x10 board 
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]     NOTE: NO

data Board = Board { tileSize :: Int,
                     size :: Int,
                     target :: Int,
                     buttonLoci :: [Position],
                     wPieces :: [Position],
                     bPieces :: [Position] }
  deriving Show

btloci :: Float -> Float -> [Position]
btloci bDims tSize = do
  let a = (bDims / 2) * tSize
  let bs = [(-1 * a), (tSize - a)..a] where a = bDims * 0.5 * tSize
  [ (x, y) | x <- bs, y <- bs ]

-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard = do
  let bDimension = 6            -- 1 less than the actual dimension on the board
  let tileSize = 50
  let target = 3
  Board tileSize bDimension target (btloci (fromIntegral bDimension) (fromIntegral tileSize)) [] []

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col }

initWorld = World initBoard Black

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Col -> Position -> Maybe Board
-- TODO: Validate :
--                Invalid if position not in buttonLoci               :: trying to place something off the board
--                else Invalid if position in wPieces                 :: trying to place something where there is already a piece
--                else Invalid if position in bPieces                 :: trying to place something where there is already a piece
makeMove oldBoard curTurn newPosition = do
  case curTurn of
    Black -> Just $ Board (tileSize oldBoard) (size oldBoard) (target oldBoard) (buttonLoci oldBoard) (wPieces oldBoard) ((bPieces oldBoard) ++ [newPosition])
    White -> Just $ Board (tileSize oldBoard) (size oldBoard) (target oldBoard) (buttonLoci oldBoard) ((wPieces oldBoard) ++ [newPosition]) (bPieces oldBoard)

-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Col
checkWon board = trace "checking" (Just Black)

{- Hint: One way to implement 'checkWon' would be to write functions 
which specifically check for lines in all 8 possible directions
(NW, N, NE, E, W, SE, SW)

In these functions:
To check for a line of n in a row in a direction D:
For every position ((x, y), col) in the 'pieces' list:
- if n == 1, the colour 'col' has won
- if n > 1, move one step in direction D, and check for a line of
  n-1 in a row.
-}

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate = undefined



