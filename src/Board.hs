{-# LANGUAGE DeriveGeneric #-}
module Board where

import Debug.Trace
import Control.Monad (when)

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics

data Col = Black | White
  deriving (Show, Generic)

instance FromJSON Col
instance ToJSON Col

other :: Col -> Col
other Black = White
other White = Black

type Position = (Float, Float)
  -- deriving (Show, Generic)

-- instance FromJSON Position
-- instance ToJSON Position

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
  deriving (Show, Generic)

instance FromJSON Board
instance ToJSON Board

btloci :: Float -> Float -> [Position]
btloci bDims tSize = do
  let a = (bDims / 2) * tSize
  let bs = [(-1 * a), (tSize - a)..a] where a = bDims * 0.5 * tSize
  [ (x, y) | x <- bs, y <- bs ]

-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard bDim bTarg = do
  let bDimension = (bDim - 1)            -- 1 less than the actual dimension on the board
  let tileSize = 50
  let target = bTarg
  let loci = btloci (fromIntegral bDimension) (fromIntegral tileSize)
  Board tileSize bDimension target loci [] []

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col,
                     filePath :: String }      -- Just if file exists, otherwise Nothing
  deriving (Show, Generic)

instance FromJSON World
instance ToJSON World

initWorld :: Int -> Int -> String -> World -> World
-- initWorld bDim bTarg filePath "" = World (initBoard bDim bTarg) Black filePath
initWorld bDim bTarg filePath spec = spec


{--
 - pTurn
 - parse (string "Board")
 - parse space
 - parse {
 - parse tileSize; size; target; each have a number
 - parse buttonLoci; wPieces; bPieces; each have lists
 --}

-- pAny = parse (string "Black" <|> string "White") -- will always have one of these

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Col -> Position -> Maybe Board
-- TODO: Validate :
--                Invalid if position not in buttonLoci               :: trying to place something off the board
--                else Invalid if position in wPieces                 :: trying to place something where there is already a piece
--                else Invalid if position in bPieces                 :: trying to place something where there is already a piece
makeMove oldBoard curTurn newPosition = do
  if not (newPosition `elem` (trace ("buttons: " ++ show (buttonLoci oldBoard)) (buttonLoci oldBoard)) ) then
    Nothing -- Position is not a valid board spot
  else if newPosition `elem` wPieces oldBoard || newPosition `elem` bPieces oldBoard then
    Nothing -- Position already taken by another piece
  else
    case curTurn of
      Black -> Just $ oldBoard { bPieces = (bPieces oldBoard) ++ [newPosition] }
      White -> Just $ oldBoard { wPieces = (wPieces oldBoard) ++ [newPosition] }
-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Col
checkWon board =
  if hasWon board White then ( trace ("Wh Win") Just White )
  else if hasWon board Black then ( trace ("Bl Win") Just Black )
  else Nothing

{- Hint: One way to implement 'checkWon' would be to write functions
which specifically check for lines in all 8 possible directions
(NW, N, NE, E, W, SE, SW) -}

hasWon :: Board -> Col -> Bool
hasWon board col =
  let
    pieces = case col of
              White -> wPieces $ board
              Black -> bPieces board
    targetCount = target board

    directions = [(-50,-50), (-50,0), (-50,50), (0,-50), (0,50), (50,-50), (50,0), (50,50)]
    shouldCheckLine position directionToCheck = countLine position directionToCheck 1 >= targetCount

    countLine (x, y) (xoffset, yoffset) count =
      let checkPos = (x + xoffset, y + yoffset)
      in if checkPos `elem` pieces
         then countLine checkPos (xoffset, yoffset) (count + 1)
         else count
  in Prelude.any (\pos -> Prelude.any (shouldCheckLine pos) directions) (pieces)

{-- 
 - Check world turn
 - black => change black piece set
 - white => change white piece set
--}
undoTurn :: World -> World
undoTurn w = do
  let curBoard = board w
  case (turn w) of
    White -> do
      let oBs = bPieces curBoard    
      let nBs = if (oBs == [])
                then oBs
                else Prelude.init oBs
      let nWs = wPieces curBoard    
      World ( Board (tileSize curBoard) (size curBoard) (target curBoard) (buttonLoci curBoard) (nWs) (nBs) ) (other $ turn w) (filePath w)

    Black -> do 
      let oWs = wPieces curBoard    
      let nWs = if (oWs == [])
                then oWs
                else Prelude.init oWs
      let nBs = bPieces curBoard    
      World ( Board (tileSize curBoard) (size curBoard) (target curBoard) (buttonLoci curBoard) (nWs) (nBs) ) (other $ turn w) (filePath w)
undoRound :: World -> World
undoRound w = do 
  {-- 
  - remove latest from both
  - so, newBs = Prelude.init Bs
       newWs = Prelude.init Ws
  --}
  let curBoard = board w
  let oBs = bPieces curBoard
  let oWs = wPieces curBoard
  let nBs = if (oBs == [])
               then oBs
               else Prelude.init oBs 
   
  let nWs = if (oWs == [])
               then oWs
               else Prelude.init oWs 

  World ( Board (tileSize curBoard) (size curBoard) (target curBoard) (buttonLoci curBoard) (nWs) (nBs) ) (turn w) (filePath w)

{- In these functions
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

-- saveWorld :: World -> String -> IO ()
writeWorldToJSON :: FilePath -> World -> IO ()
writeWorldToJSON path world = B.writeFile path (encode world)

saveWorld w filePath = do
  writeWorldToJSON filePath w
  
    -- writeFile filePath $ (show $ turn w) ++ "\n"
    -- appendFile filePath $ show $ board w
