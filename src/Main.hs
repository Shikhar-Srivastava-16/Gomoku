module Main where

import Debug.Trace
import Graphics.Gloss
import System.Environment
import Options.Applicative
import Graphics.Gloss.Interface.IO.Game
import System.Directory

import Board
import Draw
import Input
import AI

-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated
-- move

-- parser library: https://hackage.haskell.org/package/optparse-applicative
data CLIArgs = CLIArgs { argSize :: Int,
                         argTarget :: Int, 
                         argSpd :: Int,
                         argAI :: Int,
                         argSaveFile :: String,
                         argLoadFilePath :: String }

cliParser :: Parser CLIArgs
cliParser = CLIArgs
         -- parser for size
         <$> option auto
             ( long "bsize"
            <> short 's'
            <> metavar "<SIZE>"
            <> value 6
            <> help "The size of the board" )
         -- parser for target
         <*> option auto
             ( long "target"
            <> short 't'
            <> metavar "<TARGET>"
            <> value 3
            <> help "The number of tokens in a row needed to win" )
         <*> option auto
             ( long "speed"
            <> short 'v'
            <> metavar "<GAME LOOP SPEED>"
            <> value 10
            <> help "The speed at which game loop runs, i.e the number of times the loop functions are called per second" )
         <*> option auto
             ( long "aigen"
            <> short 'a'
            <> metavar "<WHICH AI>"
            <> value 0 -- NOTE: No AI by default TODO change later?
            <> help "Which AI model to run: 0 is OFF, i.e no AI (2-player)" )
         <*> strOption
             ( long "filepath"
            <> short 'f'
            <> value "save.gku"
            <> metavar "<FILEPATH>"
            <> help "Where the game gets saved (if save is run)" )
         <*> strOption
             ( long "load"
            <> short 'l'
            <> value "none"
            <> metavar "<LOAD FILE>"
            <> help "if this argument is passed in, the game will load a save" )

main :: IO ()
main = do
    composed <- execParser cliargs
    
    -- let maybeSpec = if (doesFileExist $ argLoadFilePath composed)
    --                      then do 
    --                         a <- readFile $ argLoadFilePath composed
    --                         Just a
    --                      else Nothing
    let maybeSpec = readFile $ argLoadFilePath composed
    playIO (InWindow "Gomoku" (640, 480) (10, 10)) (light $ light $ black) (argSpd composed)
        ( initWorld (argSize composed) (argTarget composed) (argSaveFile composed) (Just maybeSpec) )         -- in Board.hs
        drawIOWorld               -- in Draw.hs
        handleInputIO             -- in Input.hs
        updateWorldIO             -- in AI.hs
    where
        cliargs = info (cliParser <**> helper)
            ( fullDesc
           <> header "Starting up gomoku"
           <> progDesc "Gomoku: Five-in-a-row, written in haskell!" )


drawIOWorld :: World -> IO Picture
drawIOWorld w = return $ drawWorld w
-- handleIOInput :: World -> World
updateWorldIO :: Float -> World -> IO World
updateWorldIO t w = return $ updateWorld t w
