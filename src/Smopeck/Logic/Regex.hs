{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Smopeck.Logic.Regex where

import           Control.Arrow                  (first, second)
import           Control.Monad.Primitive
import           Control.Monad.State.Strict
import           Data.Array.Unboxed             as U
import           Data.Char
import           Data.Coerce
import           Data.Function
import qualified Data.IntMap.CharMap2           as CM
import qualified Data.IntSet                    as IS
import           Data.List
import qualified Data.Map                       as M
import           Data.Maybe
import qualified Data.Set                       as S
import           Data.String
import           Data.Tuple
import           Debug.Trace
import           Smopeck.Logic.Model
import           Smopeck.Spec.Exp               hiding (interpret)
import           Smopeck.Spec.RegexUtil
import           System.Random.MWC
import           Text.PrettyPrint.HughesPJClass as PP hiding (first)
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Common         hiding (on)


data Node = Node {
    nodeId           :: !Int,
    nodeAccept       :: !Bool,
    nodeCharTrans    :: !(M.Map Char Int),
    nodeDefaultTrans :: !Int
}

data Automaton = Automaton {
    initState :: !Int,
    nodes     :: !(Array Int Node)
}

instance Pretty Node where
    pPrint Node {..} =
        hang ("Node" <+> pPrint nodeId PP.<> ":") 4 $
            ("accept:" <+> pPrint nodeAccept) $+$
            hang "charTrans:" 4 (vcat [ text (show ch) <+> "->" <+> pPrint to  | (ch, to) <- M.toList nodeCharTrans ]) $+$
            ("defaultTrans:" <+> pPrint nodeDefaultTrans)

instance Pretty Automaton where
    pPrint Automaton{..} =
        ("initState:" <+> pPrint initState) $+$
        vcat (map pPrint $ elems nodes)

instance Show Automaton where
    show = render . pPrint

instance Show Node where
    show = render . pPrint

extractAutomaton :: Regex -> Automaton
extractAutomaton Regex {..} = Automaton s0 delta
    where
    delta = array (0, M.size set - 1) [ (nodeId node, node) | node <- nodes ]
    (s0, (set, nodes)) = runState (go regex_dfa) (M.empty, [])
    go :: DFA -> State (M.Map IS.IntSet Int, [Node]) Int
    go node = gets (M.lookup (d_id node) . fst) >>= \case
        Just v -> pure v
        Nothing -> do
            i <- gets $ M.size . fst
            modify' (first $ M.insert (d_id node) i)
            case d_dt node of
                Simple' dt_win dt_trans dt_other -> do
                    charTrans <- M.fromAscList
                        <$> forM (CM.toList dt_trans) (\(key, tr) -> do
                            to <- go (trans_single tr)
                            pure (key, to))
                    defTrans <- go (trans_single dt_other)
                    modify $ second $ (:) $ Node i (not $ null dt_win) charTrans defTrans
                    pure i
                Testing' {} -> error "Testing' is not supported"

type RouteTable = [UArray Int Double]

alphabet :: [Char]
alphabet = [chr 32..chr 126]

computeRouteTable :: Automaton -> RouteTable
computeRouteTable automaton = iterate step arr0
    where
    bb = bounds (nodes automaton)
    arr0 = automaton
      & nodes
      & elems
      & map (\node -> if nodeAccept node then 1 else 0)
      & listArray bb
      & normalize
    step :: UArray Int Double -> UArray Int Double
    step arr = arr
        & indices
        & map update
        & listArray bb
        & normalize
        where
        update i = n
            where
            Node{..} = nodes automaton ! i
            defaultAssocs = [ (ch,nodeDefaultTrans) | ch <- alphabet, M.notMember ch nodeCharTrans ]
            n = M.assocs nodeCharTrans  ++ defaultAssocs
              & map (\(k,v) -> arr ! v)
              & sum
    normalize :: UArray Int Double -> UArray Int Double
    normalize arr = amap (/mother) arr
        where
        mother = sum $ elems arr

randomChoose :: PrimMonad f => Gen (PrimState f) -> [a] -> f a
randomChoose st l = do
    let n = length l
    (l !!) <$> uniformR (0,n-1) st

randomGenerate :: forall f. PrimMonad f => Gen (PrimState f) -> Automaton -> RouteTable -> Int -> f (Maybe String)
randomGenerate st automaton routeTbl maxLen
    | null lenCands = pure Nothing
    | otherwise = Just <$> do
        strLen <- randomChoose st lenCands
        randomWalk strLen (initState automaton) (reverse $ take strLen routeTbl)
  where
    list = take maxLen routeTbl
    lenCands = [ i | (i,arr) <- zip [0..maxLen-1] list, arr ! initState automaton > 0 ]
    chooseUniform :: Double -> Double -> [(Char, Int, Double)] -> (Char, Int, Double)
    chooseUniform p mother [v] = v
    chooseUniform p mother (v@(_,_,q):vs)
        | p <= mother * q = v
        | otherwise = chooseUniform (p - mother * q) mother vs
    randomWalk :: Int -> Int -> RouteTable -> f String
    randomWalk 0 !v _ = pure ""
    randomWalk k v (arr:arrs) = do
        let defaultAssocs = [ (ch,nodeDefaultTrans) | arr ! nodeDefaultTrans > 0, ch <- alphabet, M.notMember ch nodeCharTrans ]
            cands = M.assocs nodeCharTrans ++ defaultAssocs
                & map (\(k,v) -> (k,v, arr ! v))
                & filter (\(_,_,p) -> p > 0)
            mother = recip $ sum $ map (\(_,_,p) -> p) cands
            Node{..} = nodes automaton ! v
        r <- uniformR (0,1) st
        let (ch, w, _) = chooseUniform r mother cands
        (ch:) <$> randomWalk (k-1) w arrs

instance Model String where
    data Atom String = SAny | SOneOf !(S.Set String) | SPtn RString
    top = SAny
    bot = SOneOf S.empty
    join SAny _                       = SAny
    join _ SAny                       = SAny
    join (SOneOf xs) (SOneOf ys)      = SOneOf (S.union xs ys)
    join (SOneOf xs) (SPtn regex) = SPtn $ joinR (regex:map escapeR (S.toList xs))
    join (SPtn regex) (SOneOf xs) = SPtn $ joinR (regex:map escapeR (S.toList xs))
    join (SPtn rx) (SPtn ry) = SPtn $ joinR [rx, ry]
    meet SAny ys                  = ys
    meet xs SAny                  = xs
    meet (SOneOf xs) (SOneOf ys)  = SOneOf (S.intersection xs ys)
    meet (SOneOf xs) (SPtn regex) = SOneOf (S.filter (`matchR` regex) xs)
    meet (SPtn regex) (SOneOf xs) = SOneOf (S.filter (`matchR` regex) xs)
    meet (SPtn _) (SPtn _)        = error "meet of two regex is not supported"
    generate st SAny = generate st (SPtn ".*")
    generate st (SOneOf xs)
        | S.null xs = pure Nothing
        | otherwise = do
            i <- uniformR (0,S.size xs - 1) st
            pure $ Just $! S.elemAt i xs
    generate st (SPtn regex) = randomGenerate st automaton tbl 30
        where
        automaton = extractAutomaton $ compileR regex
        tbl = computeRouteTable automaton
    interpret Eq (LString v)   = SOneOf (S.singleton v)
    interpret Match (LRegex s) = SPtn (RString s)
