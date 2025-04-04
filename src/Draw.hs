module Draw(drawWorld) where

import Graphics.Gloss
import Board

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld w = do
    let bDims = fromIntegral $ size $ board w
    Color white $ drawGrid 50 bDims

drawGrid tSize bDims = do
    let bs = [(-1 * a), (tSize - a)..a] where a = bDims * 0.5 * tSize
    let con = bs !! 0
    let loci = [[(con, b), (-1 * con, b)] | b <- bs]++[[(b, con), (b, -1 * con)] | b <- bs]
    Pictures [ Line locus | locus <- loci ]
