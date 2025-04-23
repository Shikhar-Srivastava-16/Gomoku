module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI

import Debug.Trace
import Control.Concurrent
import Control.Exception
import System.IO.Unsafe

-- functions for timed turns - whichever thread of the timer finishing and the player making a move are done first, are returned
compete :: [IO a] -> IO a
compete actions = do
  mvar <- newEmptyMVar
  tids <- mapM (\action -> forkIO $ action >>= putMVar mvar) actions
  result <- takeMVar mvar
  mapM_ killThread tids
  return result

timeout :: Int -> IO a -> IO (Maybe a)
timeout usec action = compete [fmap Just action, threadDelay usec >> return Nothing]

-- functions for snapping
coordSnap w coord = rndAdv ( size $ board w ) ( toInteger $ tileSize $ board w ) coord

clickSnap :: World -> (Integer, Integer) -> (Integer, Integer)
clickSnap w (xCoord, yCoord) = ((coordSnap w (toInteger xCoord)), (coordSnap w (toInteger yCoord)))

rndAdv :: Int -> Integer -> Integer -> Integer
rndAdv size target input = do

    if (even size)
        then do
            if (input >= 0) 
                then rnd target input
                else ( (-1) * (rnd target ( input * (-1) ) ) )
        else do
            if (input >= 0) 
                then (rnd target (input + 25) -25 )
                else ( (-1) * ( rnd target ( ( input * (-1) ) + 25 ) -25 ) )

rnd :: Integer -> Integer -> Integer 
rnd target input = do
    let temp = rem input target
    if (temp < div target 2) 
        then (input - temp)
        else (input + 50 - temp)

first (a,b) = a
second (a,b) = b

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
-- handleInput (EventMotion (x, y)) b 
--     = trace ("Mouse moved to: " ++ show (x,y)) b
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w 
    = do
        let snapped = clickSnap w (round x, round y)
        --let newBoard = makeMove (board w) (turn w) (fromIntegral $ first snapped, fromIntegral $ second snapped)
        let selectedPos = (fromIntegral $ first snapped, fromIntegral $ second snapped)
        let timeoutResult = unsafePerformIO $ timeout 1000000 $ Control.Exception.evaluate $ makeMove (board w) (turn w) selectedPos
        let doesTurnInTime = case timeoutResult of
              Nothing -> trace ("turn timed out") False
              Just _ -> trace ("Returned in time!") True
        let newBoard = case timeoutResult of
                          Just (Just b) -> Just b
                          _ -> Nothing
        if doesTurnInTime
            then case newBoard of
                  Just b -> trace ("Left button press at " ++ show (x,y) ++ "snapped to: " ++ show snapped ++ "; " ++ show (turn w) ++ " moved here") World (bmps w) b (other $ turn w)
                  Nothing -> trace ("Left button press at " ++ show (x,y) ++ "snapped to: " ++ show snapped ++ "; " ++ " !!Invalid Move!!") w
        --else World (bmps w) (board w) (other $ turn w)
        else trace ("yikes, time's up!") w
-- handleInput (EventKey (Char k) Down _ _) b
--     = trace ("Key " ++ show k ++ " down") b
handleInput (EventKey (Char 'u') Up _ _) w
    = trace ("Key " ++ show 'u' ++ " up: Undoing one from both") $ undoRound w

handleInput (EventKey (Char 'p') Up _ _) w
    = trace ("Key " ++ show 'p' ++ " up: Toggling Pause") $ togglePause w
-- on paused, get current time, set as paused var
-- on unpaused, get current time, current - paused added to start
handleInput (EventKey (Char 'b') Up _ _) w
    = trace ("Key " ++ show 'b' ++ " up: Undoing one from current player") $ undoTurn w


-- other input events
handleInput e b = b
{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}

