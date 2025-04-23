# Provenance Changelog

## Apr 10th
### ss504
 - added rudimentary draw function

## Apr 15th
### ss504
 - changed board and world to contain pieces
 - changed draw function to add pieces to board
 - changed coloration to be done picture-by-picture rather than at top level

## Apr 17th
### ss504
 - Added logic to change board with clicks, no validation

## Apr 19th
### ss504
 - Partial code for AI calls

## Apr 20th
### ss504
 - Added cli parser
 - Added adjustments for all sizes of board (Co-Authored by Jack)
 - graphics changed
 - More work on AI calls

## Apr 21st
### ss504
 - added undo options with 'u' and 'b'



# Player Guide

 - left click: place piece
 - s: save
 - h: show hint (grey ring)
 - u: undo turn (both players)
 - b: undo turn (latest player)
 - >: increase size
 - <: decrease size
 - ,: decrease target
 - .: increase target
 - p: pause (only works with cabal repl, incomplete)


## Player Flags:
 - run gomoku with -h or --help to get a list of flags. 

 -  -s,--bsize <SIZE>        The size of the board; default = 15
 -  -t,--target <TARGET>     The number of tokens in a row needed to win; default = 5
 -  -v,--speed <GAME LOOP SPEED>
 -                           The speed at which game loop runs, i.e the number of 
 -                           times the loop functions are called per second; default = 10
 -  -a,--aigen <WHICH AI>    Which AI model to run: 0 is OFF, i.e no AI (2-player); default = 0
        0 -> Player v Player, no AI
        1 -> AI level 1
        2 -> AI level 2 etc
 
 -  -f,--filepath <FILEPATH> Where the game gets saved (if save is run); default = save.json
 -  -l,--load <LOAD FILE>    if this argument is passed in, the game will load a save
 -  -h,--help                Show this help text




