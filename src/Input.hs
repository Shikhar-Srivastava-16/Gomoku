module Input(handleInputIO) where

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

handleInputIO :: Event -> World -> IO World
handleInputIO (EventKey (MouseButton LeftButton) Up m (x, y)) w 
    = do
        let snapped = clickSnap w (round x, round y)
        let selectedPos = (fromIntegral $ first snapped, fromIntegral $ second snapped)
        let newBoard = makeMove (board w) (turn w) selectedPos

        case newBoard of
           Just b -> trace ("Left click " ++ show (x,y) ++ " snapped to: " ++ show snapped ++ "; " ++ show (turn w) ++ " moved.") return $ World (won w) b (other $ turn w) (filePath w) Nothing (aiLevel w)
           Nothing -> trace ("Left click " ++ show (x,y) ++ " !!Invalid Move!!") $ return w

handleInputIO (EventKey (Char 'u') Up _ _) w
    = trace ("Undoing one from both") $ return $ undoRound w

handleInputIO (EventKey (Char '.') Up _ _) w
    = trace ("higher targ") $ return (World (won w) (initBoard ((size $ board w) + 1) ((target $ board w) + 1) (rules $ board w)) (turn w) (filePath w) (hint w) (aiLevel w))

handleInputIO (EventKey (Char ',') Up _ _) w
    = trace ("lower targ") $ return (World (won w) (initBoard ((size $ board w) + 1) ((target $ board w) - 1) (rules $ board w)) (turn w) (filePath w) (hint w) (aiLevel w))

handleInputIO (EventKey (Char '>') Up _ _) w
    = trace ("size higher") $ return (World (won w) (initBoard ((size $ board w) + 2) (target $ board w) (rules $ board w)) (turn w) (filePath w) (hint w) (aiLevel w))

handleInputIO (EventKey (Char '<') Up _ _) w
    = trace ("size lower") $ return (World (won w) (initBoard ((size $ board w)) (target $ board w) (rules $ board w)) (turn w) (filePath w) (hint w) (aiLevel w))
-- ( Board (tileSize $ board w ) ((size $ board w) - 1) (target $ board w) (buttonLoci $ board w) (nWs) (nBs) )
handleInputIO (EventKey (Char 'b') Up _ _) w
    = trace ("Undoing one from current player") $ return $ undoTurn w
    
handleInputIO (EventKey (Char 's') Up _ _) w
    = trace ("saving") $ do 
                                                     a <- saveWorld w (filePath w)
                                                     return w
handleInputIO (EventKey (Char 'p') Up _ _) w
    = trace ("Toggling Pause") $ return $ togglePause w

handleInputIO (EventKey (Char 'h') Up _ _) w
    = return w { hint = (calcHint w)} 

handleInputIO e b = return b

