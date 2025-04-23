{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Board where

import Debug.Trace

import Control.Monad (when)
import Graphics.Gloss
import Data.Time.Clock (getCurrentTime, diffUTCTime, addUTCTime)
import qualified Data.Time.Clock as Clock
import System.IO.Unsafe
import Data.Aeson
import Data.Aeson.Key
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics

data Col = Black | White
  deriving (Show, Generic, Eq)

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
                     turnStartTime :: Clock.UTCTime,
                     turnPausedStartTime :: Clock.UTCTime,
                     paused :: Bool,
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
  let currentTime = unsafePerformIO $ getCurrentTime
  let target = bTarg
  let loci = btloci (fromIntegral bDimension) (fromIntegral tileSize)
  Board tileSize bDimension target currentTime currentTime False loci [] []

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).

data World = World { won :: Bool,
                     board :: Board,
                     turn :: Col,
                     filePath :: String,
                     hint :: Maybe Position }      -- Just if file exists, otherwise Nothing
  deriving (Show, Generic)

instance FromJSON World
instance ToJSON World

initWorld :: Int -> Int -> String -> Maybe World -> World
-- initWorld bDim bTarg filePath "" = 
initWorld bDim bTarg savePath spec = case spec of 
                                          Nothing -> World (False) (initBoard bDim bTarg) Black savePath Nothing
                                          Just a -> World (won a) (board a) (turn a) (filePath a) Nothing

data Bmps = Bmps { bl :: Picture,
                   wh :: Picture,
                   sq :: Picture,
                   wwin :: Picture,
                   bwin :: Picture,
                   hintPic :: Picture }
  deriving (Show, Generic)

-- instance FromJSON Bmps where
--   -- parseJSON (Object v) = Bmps <$> v .: "bl" <*> v .: "wh" <*> v .: "sq"
--   parseJSON _ = mzero

-- instance ToJSON Bmps where
--   toJSON (Bmps bl wh sq) = 
--     object [ (fromString "bl") .= (circleSolid 0.5), (fromString "wh") .= (circleSolid 0.5), (fromString "sq") .= (circleSolid 0.5)]
--   -- toJSON _ = mzero

-- deriving instance Generic Picture
-- instance FromJSON Picture
-- instance ToJSON Picture
-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove oldBoard curTurn newPosition = do
  if not (newPosition `elem` (trace ("buttons: " ++ show (buttonLoci oldBoard)) (buttonLoci oldBoard)) ) then
    Nothing -- Position is not a valid board spot
  else if newPosition `elem` wPieces oldBoard || newPosition `elem` bPieces oldBoard then
    Nothing -- Position already taken by another piece -- else if True && -- 3 and 3 rule, cannot make two open 3 long rows
  else if (hasFourAndFour oldBoard curTurn newPosition) then-- 4 and 4 rule, cannot make two 4 long rows
    Nothing
  else if (hasThreeAndThree oldBoard curTurn newPosition) then
    Nothing
  else do
    case curTurn of
      Black -> Just $ oldBoard { bPieces = (bPieces oldBoard) ++ [newPosition] }
      White -> Just $ oldBoard { wPieces = (wPieces oldBoard) ++ [newPosition] }
-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Col
checkWon board =
  if hasWon board White then ( trace ("Wh Win HERE") Just White )
  else if hasWon board Black then ( trace ("Bl Win HERE") Just Black )
  else Nothing

{- Hint: One way to implement 'checkWon' would be to write functions
which specifically check for lines in all 8 possible directions
(NW, N, NE, E, W, SE, SW) -}

hasThreeAndThree :: Board -> Col -> Position -> Bool
hasThreeAndThree :: Board -> Col -> Position -> Bool
  where
    positiveDirections = [(0, 50), (50,50), (50, 0), (50, -50)]
    -- take this position, count its line length in one direction
    -- count it in the opposite direction, add, + 1 for itself
    totalDirectionLengths = Prelude.map (countLineBothEnds board col pos) (positiveDirections)
    -- if equals 4, add to fourRowCount
    fourRowCount = Prelude.length (Prelude.filter (== 3) totalDirectionLengths)
    -- return fourRowCount >= 2
    a = (fourRowCount >= 2)

hasFourAndFour :: Board -> Col -> Position -> Bool
hasFourAndFour board col pos = a
  where
    positiveDirections = [(0, 50), (50,50), (50, 0), (50, -50)]
    -- take this position, count its line length in one direction
    -- count it in the opposite direction, add, + 1 for itself
    totalDirectionLengths = Prelude.map (countLineBothEnds board col pos) (positiveDirections)
    -- if equals 4, add to fourRowCount
    fourRowCount = Prelude.length (Prelude.filter (== 4) totalDirectionLengths)
    -- return fourRowCount >= 2
    a = (fourRowCount >= 2)

countLineBothEnds b c (x, y) (xoffset, yoffset) = countLine b c (x, y) (xoffset, yoffset) ( (countLine b c (x, y) ( -(xoffset), -(yoffset) ) 1) )

countLinePickyBothEnds b c (x, y) (xoffset, yoffset) = do
  let firstCount = countLinePickyWithEnds b c (x, y) ( -(xoffset), -(yoffset) ) 1
  if firstCount == 0 then 0
  else countLinePickyWithEnds b c (x, y) (xoffset, yoffset) ( firstCount )

countLine :: Board -> Col -> Position -> Position -> Int -> Int
countLine board col (x, y) (xoffset, yoffset) count =

    let checkPos = (x + xoffset, y + yoffset)
    in if checkPos `elem` pieces
       then countLine board col checkPos (xoffset, yoffset) (count + 1)
       else count
    where pieces = case col of
                White -> wPieces board
                Black -> bPieces board

countLinePickyWithEnds :: Board -> Col -> Position -> Position -> Int -> Int
countLinePickyWithEnds board col (x, y) (xoffset, yoffset) count =

    let
      checkPos = (x + xoffset, y + yoffset)
      pieces = case col of
                White -> wPieces board
                Black -> bPieces board
      oppPieces = case col if
                White -> bPieces board
                Black -> wPieces board
    in if checkPos `elem` pieces
       then countLine board col checkPos (xoffset, yoffset) (count + 1)
       else if not checkPos `elem` oppPieces
       then count
       else 0


hasWon :: Board -> Col -> Bool
hasWon board col =
  let
    pieces = case col of
              White -> wPieces $ board
              Black -> bPieces board
    targetCount = target board

    directions = [(-50,-50), (-50,0), (-50,50), (0,-50), (0,50), (50,-50), (50,0), (50,50)]
    shouldCheckLine position directionToCheck = countLine board col position directionToCheck 1 >= targetCount

    {-- countLine (x, y) (xoffset, yoffset) count =
      let checkPos = (x + xoffset, y + yoffset)
      in if checkPos `elem` pieces
         then countLine checkPos (xoffset, yoffset) (count + 1)
         else count --}
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
      World (won w) ( Board (tileSize curBoard) (size curBoard) (target curBoard) (turnStartTime curBoard) (turnStartTime curBoard) (paused curBoard) (buttonLoci curBoard) (nWs) (nBs) ) (other $ turn w) (filePath w) Nothing

    Black -> do 
      let oWs = wPieces curBoard    
      let nWs = if (oWs == [])
                then oWs
                else Prelude.init oWs
      let nBs = bPieces curBoard    

      World (won w) ( Board (tileSize curBoard) (size curBoard) (target curBoard) (turnStartTime curBoard) (turnStartTime curBoard) (paused curBoard) (buttonLoci curBoard) (nWs) (nBs) ) (other $ turn w) (filePath w) Nothing


togglePause :: World -> World
togglePause w = do
  let curBoard = board w
  let currentTime = unsafePerformIO $ getCurrentTime
  if paused (board w)
    then do
    let pauseDuration = diffUTCTime currentTime (turnPausedStartTime curBoard)
    let pauseOffsetStart = addUTCTime pauseDuration (turnStartTime curBoard)
    World (won w) (Board (tileSize curBoard) (size curBoard) (target curBoard) (pauseOffsetStart) (currentTime) (False) (buttonLoci curBoard) (wPieces curBoard) (bPieces curBoard) ) (turn w) (filePath w) Nothing
  else
    World (won w) ( Board (tileSize curBoard) (size curBoard) (target curBoard) (turnStartTime curBoard) (currentTime) (True) (buttonLoci curBoard) (wPieces curBoard) (bPieces curBoard) ) (turn w) (filePath w) Nothing

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

  World (won w) ( Board (tileSize curBoard) (size curBoard) (target curBoard) (turnStartTime curBoard) (turnStartTime curBoard) (paused curBoard) (buttonLoci curBoard) (nWs) (nBs) ) (turn w) (filePath w) Nothing-- TODO set current and paused time to current time - 10 for fair replay

{- In these functions:
To check for a line of n in a row in a direction D:
For every position ((x, y), col) in the 'pieces' list:
- if n == 1, the colour 'col' has won
- if n > 1, move one step in direction D, and check for a line of
  n-1 in a row.
-}

writeWorldToJSON :: FilePath -> World -> IO ()
writeWorldToJSON path world = B.writeFile path (encode world)

saveWorld w filePath = do
  if filePath == "!!none!!"
    then error "Malformed file path provided, please do not use reserved keyword '!!none!!'"
    else do
      writeWorldToJSON filePath w
  
-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate board _
  | hasWon board White = -1
  | hasWon board Black = 1
  | otherwise = 0

evalTie :: Board -> Col -> Int
evalTie board col = undefined
