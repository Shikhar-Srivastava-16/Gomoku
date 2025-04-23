module Draw(drawWorld) where

import Graphics.Gloss
import Board

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: Bmps -> World -> Picture
drawWorld bmps w = do
    let bDims = fromIntegral $ size $ board w
    Pictures (drawBmpGrid bmps w)
    -- Pictures $ drawGrid 50 bDims (wPieces $ board w) (bPieces $ board w)

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

drawBmpGrid bmps w = do
    let wPics = [ translate xi yi (scale 0.08 0.08 $ wh bmps) | (xi, yi) <- wPieces $ board w]
    let bPics = [ translate xi yi (scale 0.04 0.04 $ bl bmps) | (xi, yi) <- bPieces $ board w]
    let loci = [ translate xi yi (scale 0.5 0.5 $ sq bmps) | (xi, yi) <- buttonLoci $ board w]  
    
    loci ++ wPics ++ bPics