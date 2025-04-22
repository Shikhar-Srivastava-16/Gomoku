module AI where

import Lists
import Board
import Debug.Trace

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

-- type generationFunction = Board -> Col -> [Position]

gen :: Board -> Col -> [Position] 
gen board _ =
  let
      filledPositions = wPieces board ++ bPieces board
  in filter (\p -> notElem p filledPositions) (buttonLoci board)

-- do
--   let list = buttonLoci board
--   let toRem1 = Set.toList $ wPieces board
--   let toRem2 = Set.toList $ bPieces board
--   -- remove blacks and whites from main list for all possible moves
--   removeAll (removeAll list toRem2) toRem1

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
getBestMove depth tree = (125.0, 125.0)

-- -- Update the world state after some time has passed
-- updateWorld :: Float -- ^ time since last update (you can ignore this)
--             -> World -- ^ current world state
--             -> World
-- updateWorld t w =
--   do
--   let won = checkWon $ board w
--   case won of
--     Nothing -> trace("No Win") w
--     Just Black -> trace("Bl Win") w
--     Just White -> trace("Wh Win") w 

updateWorld :: Float -> World -> World
updateWorld t w
  | checkWon (board w) == Just Black = trace "Bl Win" w
  | checkWon (board w) == Just White = trace "Wh Win" w
  | null allPossibleMoves = trace "error generating moves or none valid" w
  | turn w == Black = case makeMove (board w) (turn w) (head allPossibleMoves) of
                          Just validBoard -> World { bmps = bmps w, board = validBoard, turn = other (turn w)}
                          Nothing -> trace "ai error" w
  | otherwise = trace ("No Win") w
  where allPossibleMoves = gen (board w) (turn w)

 -- let newPos = getBestMove 0 (buildTree (gen) (board w) (turn w))
 -- -- now make new board
 -- let newBoard = makeMove (board w) (turn w) newPos
 -- case newBoard of
 --   Just b -> World b (other $ turn w)
 --   Nothing -> trace ("Invalid Gen") (World (board w) (turn w))

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


