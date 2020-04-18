{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Smopeck.Logic.Regex where

import           Control.Arrow                  (first, second)
import           Control.Monad.State.Strict
import           Data.Array
import qualified Data.IntMap.CharMap2           as CM
import qualified Data.IntSet                    as IS
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import           Text.PrettyPrint.HughesPJClass as PP hiding (first)
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Common

exampleRegex :: Regex
exampleRegex = makeRegex ("(ab)*" :: String)

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
