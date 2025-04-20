module Draw(drawWorld) where

import Graphics.Gloss
import Board
import qualified Data.Set as Set

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld w = do
    let bDims = fromIntegral $ size $ board w
    Pictures $ drawGrid 50 bDims (Set.toList $ wPieces $ board w) (Set.toList $ bPieces $ board w)
 
drawGrid tSize bDims wPieces bPieces = do
    -- bs is a list of points
    let bs = [(-1 * a), (tSize - a)..a] where a = bDims * 0.5 * tSize
    -- con is first thing in bs
    let con = bs !! 0
    -- loci are all the lines - specifically, a list of lists 
    -- inner list is two points which are extreme ends of each line
    let loci = [[(con, b), (-1 * con, b)] | b <- bs]++[[(b, con), (b, -1 * con)] | b <- bs]
    let wPics = [ translate xi yi (Color white $ circleSolid $ tSize * 0.4) | (xi, yi) <- wPieces]
    let bPics = [ translate xi yi (Color yellow $ circleSolid $ tSize * 0.4) | (xi, yi) <- bPieces]
    [ Color white $ Line locus | locus <- loci ] ++ wPics ++ bPics
