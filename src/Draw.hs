module Draw(drawWorld) where

import Graphics.Gloss
import Board
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Time.Clock as Clock
import System.IO.Unsafe

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: Bmps -> World -> Picture
drawWorld bmps w = do
    if (won w)
        then case (turn w) of
            White -> bwin bmps
            Black -> wwin bmps
        else do  
            let bDims = fromIntegral $ size $ board w
            Pictures (drawBmpGrid bmps w)

-- Old drawGrid function for drawing purely rather than with bitmap images
drawGrid tSize bDims wPieces bPieces = do
    -- bs is a list of points
    let bs = [(-1 * a), (tSize - a)..a] where a = bDims * 0.5 * tSize
    -- con is first thing in bs
    let con = bs !! 0
    -- loci are all the lines - specifically, a list of lists 
    -- inner list is two points which are extreme ends of each line
    let loci = [[(con, b), (-1 * con, b)] | b <- bs]++[[(b, con), (b, -1 * con)] | b <- bs]
    let wPics = [ translate xi yi (Color white $ circleSolid $ tSize * 0.4) | (xi, yi) <- wPieces]
    let bPics = [ translate xi yi (Color black $ circleSolid $ tSize * 0.4) | (xi, yi) <- bPieces]    
    [ Color white $ Line locus | locus <- loci ] ++ wPics ++ bPics

drawBmpGrid bmps w = tiles ++ wPics ++ bPics ++ hPic
    where
        wPics = [ translate xi yi (scale 0.08 0.08 $ wh bmps) | (xi, yi) <- wPieces $ board w]
        bPics = [ translate xi yi (scale 0.04 0.04 $ bl bmps) | (xi, yi) <- bPieces $ board w]
        tiles = [ translate xi yi (scale 0.5 0.5 $ sq bmps) | (xi, yi) <- buttonLoci $ board w]
        hPic = case (hint w) of
            Nothing -> []
            Just (x, y) -> [translate x y $ scale 0.5 0.5 $ hintPic bmps]
