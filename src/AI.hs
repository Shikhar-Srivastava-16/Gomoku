module AI where

import Board
import Debug.Trace
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Time.Clock as Clock
import System.IO.Unsafe

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)


data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

-- type generationFunction = Board -> Col -> [Position]

gen :: Board -> Col -> [Position]
gen board _ =
  let

    adjacent (x, y) =
     [(x+50,y+50),(x+50,y),(x+50,y-50),(x,y+50),(x,y-50),(x-50,y+50),(x-50,y),(x-50,y-50)]

    filledPositions = wPieces board ++ bPieces board
    unoccupied = filter (\p -> notElem p filledPositions) (buttonLoci board)
 
    isAdjacentToStone location =
      any (\p -> p `elem` filledPositions) (adjacent location)
  in
    if null filledPositions
      then [(-25.0,75.0)]
    else    
      filter isAdjacentToStone unoccupied

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from 
                             -- here for opposite player

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove depth tree = snd (minimax depth tree)

minimax :: Int -> GameTree -> (Int, Position)
minimax depth (GameTree board playerTurn possibleMoves)
  | depth <= 0 || null possibleMoves || hasWon board playerTurn || hasWon board (other playerTurn) = (evaluate board playerTurn, (-1,-1))
  |  playerTurn == Black = maximumBy (comparing fst) evalSubPositions
  | otherwise = minimumBy (comparing fst) evalSubPositions
  where 
    evalSubPositions = [(fst (minimax (depth-1) subtree), pos) | (pos, subtree) <- possibleMoves]

-- -- Update the world state after some time has passed
updateWorld :: Float -> World -> World
updateWorld t w = do
  if (won w) then w
    else do
      let retval | checkWon (board w) == Just Black = trace ("Bl Win " ++ (show $ won w)) (World (True) (Board 50 6 3 (turnStartTime $ board w) (turnPausedStartTime $ board w) False [] [] []) (turn w) (filePath w) Nothing (aiLevel w)) -- TODO exit here
                | checkWon (board w) == Just White = trace ("Wh Win " ++ (show $ won w)) (World (True) (Board 50 6 3 (turnStartTime $ board w) (turnPausedStartTime $ board w) False [] [] []) (turn w) (filePath w) Nothing (aiLevel w)) -- TODO exit here
                | null allPossibleMoves = trace "error generating moves or none valid" $ w
                | turn w == Black && aiLevel w > 0 = 
                  let bestMove = getBestMove (aiLevel w) (buildTree gen (board w) Black)
                  in case makeMove (board w) (turn w) bestMove of
                                    Just validBoard -> World { won = (won w), board = validBoard, turn = other (turn w), filePath = filePath w, hint = Nothing, aiLevel = (aiLevel w)}
                                    Nothing -> trace "ai error" w
                | otherwise = trace ("No Win, checking turn is in time limit") w
                where allPossibleMoves = gen (board w) (turn w)
      if (turn w /= turn retval) -- then AI has made a move and swapped turns to player, no need to check for timeout
        then trace ("returning retval") retval
        else do

          let emptyVar = trace ("entered branch") "example"
          let currentTime = unsafePerformIO $ getCurrentTime
          let turnDuration = realToFrac $ diffUTCTime currentTime (turnStartTime (board w))

          if (turnDuration > 10) && not (paused (board w)) -- todo change to modular
            then do
              let currentBoard = board w
              let newTimingBoard = Board (tileSize currentBoard) (size currentBoard) (target currentBoard) (currentTime) (currentTime) (False) (buttonLoci currentBoard) (wPieces currentBoard) (bPieces currentBoard)
              trace ("took too long for turn, handing it over!") World { aiLevel = (aiLevel w), won = (won w), board = (newTimingBoard), turn = other (turn w), filePath = filePath w, hint = Nothing }
            else do
              if won retval 
                then retval
                else w



{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateWorld' should also check if either 
 player has won and display a message if so.
-}


calcHint :: World -> Maybe Position
calcHint w =
    let tree = (buildTree gen (board w) (turn w))
    in Just (getBestMove 3 tree)
