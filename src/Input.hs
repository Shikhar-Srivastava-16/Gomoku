module Input(handleInputIO) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI

import Debug.Trace

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
        let newBoard = makeMove (board w) (turn w) (fromIntegral $ first snapped, fromIntegral $ second snapped)
        case newBoard of
            Just b -> trace ("Left button press at " ++ show (x,y) ++ "snapped to: " ++ show snapped ++ "; " ++ show (turn w) ++ " moved here") (return $ World b (other $ turn w) (filePath w) )
            Nothing -> trace ("Left button press at " ++ show (x,y) ++ "snapped to: " ++ show snapped ++ "; " ++ " !!Invalid Move!!") (return w)

handleInputIO (EventKey (Char 'u') Up _ _) w
    = trace ("Key " ++ show 'u' ++ " up: Undoing one from both") $ return $ undoRound w

-- handleInput (EventKey (Char 'u') Up _ _) w
--     = trace ("Key " ++ show 'u' ++ " up: Undoing one from both") $ undoRound w

-- handleInput (EventKey (Char 'b') Up _ _) w
--     = trace ("Key " ++ show 'b' ++ " up: Undoing one from current player") $ undoTurn w

handleInputIO (EventKey (Char '.') Up _ _) w
    = trace ("Key " ++ show '>' ++ " up: higher targ") $ return (World (initBoard ((size $ board w) + 1) ((target $ board w) + 1)) (turn w) (filePath w))

handleInputIO (EventKey (Char ',') Up _ _) w
    = trace ("Key " ++ show ',' ++ " up: lower targ") $ return (World (initBoard ((size $ board w) + 1) ((target $ board w) - 1)) (turn w) (filePath w))

handleInputIO (EventKey (Char '>') Up _ _) w
    = trace ("Key " ++ show '>' ++ " up: higher") $ return (World (initBoard ((size $ board w) + 2) (target $ board w)) (turn w) (filePath w))

handleInputIO (EventKey (Char '<') Up _ _) w
    = trace ("Key " ++ show '<' ++ " up: lower") $ return (World (initBoard ((size $ board w)) (target $ board w)) (turn w) (filePath w))
-- ( Board (tileSize $ board w ) ((size $ board w) - 1) (target $ board w) (buttonLoci $ board w) (nWs) (nBs) )
handleInputIO (EventKey (Char 'b') Up _ _) w
    = trace ("Key " ++ show 'b' ++ " up: Undoing one from current player") $ return $ undoTurn w
    
handleInputIO (EventKey (Char 's') Up _ _) w
    = trace ("Key " ++ show 's' ++ " up: saving") $ do 
                                                     a <- saveWorld w (filePath w)
                                                     return w
-- other input events

{--
 - Configs
     - 
--}

handleInputIO e b = return b

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}

