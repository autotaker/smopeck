{-# LANGUAGE BangPatterns #-}
module Smopeck.Mock.Dependency where

import qualified Data.Map              as M
import qualified Data.Set              as S
import           Smopeck.Mock.Location

data DepEdge = DepEdge {
    depFrom :: Location,
    depTo   :: LocationBlob
}
type DepGraph = (S.Set Location, S.Set (Location, Location))

updateDepGraph :: DepGraph -> [DepEdge] -> DepGraph
updateDepGraph (vertices, edges0) depEdges = (vertices, go edges0)
    where
    go edges | edges == edges' = edges
             | otherwise = go edges'
        where
        edges' = foldl step edges depEdges
    step !edges depEdge = foldl stepV edges vertices
        where
        stepV !edges location
            | match (depTo depEdge) location =
                S.insert (depFrom depEdge, location) edges
            | otherwise = edges

findDepFreeLocation :: DepGraph -> S.Set Location -> Maybe Location
findDepFreeLocation (vertices, edges) assigned = S.lookupMin result
    where
    cands = vertices S.\\ assigned
    result = foldl step cands edges
    step !cands (from, to)
        | S.notMember to cands = cands
        | S.member from assigned = cands
        | otherwise = S.delete to cands




