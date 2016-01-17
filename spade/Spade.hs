module Spade where

import IdList
import Control.Exception
import Data.List
import Data.Foldable

type Item = String
type Database = [(Item, IdList)]

data Atom = EventAtom Item | SequenceAtom Item deriving (Eq)
instance Show Atom where
    show (EventAtom a) = " " ++ a
    show (SequenceAtom a) = "-> " ++ a

data Sequence = Sequence [Atom]
instance Show Sequence where
    show (Sequence []) = "{}"
    show (Sequence atoms) = foldl (++) "" (map show $ reverse atoms)

data Node = Node { seq_ :: Sequence,
                   idList :: IdList,
                   support :: Int,
                   generatedBy :: [Node] } deriving (Show)


loadDatabase :: Database -> [Node]
loadDatabase db = 
    let makeAtom (item, idList) = Node {seq_ = Sequence [SequenceAtom item],
                                        idList = idList,
                                        support = supportOf idList,
                                        generatedBy = []}
    in  map makeAtom db


generateCandidates :: Node -> Node -> [Node]
generateCandidates left right =
    assert (prefixLeft == prefixRight) $

    case (al, ar) of
        (EventAtom il, EventAtom ir) ->
            if il <= ir
                then []
                else [makeNode ar al eventMatched]

        (EventAtom il, SequenceAtom ir) -> [makeNode al ar temporalMatched]

        (SequenceAtom il, EventAtom ir) ->
            case compare il ir of
                LT -> [makeNode al ar eventMatched]
                EQ -> []
                GT -> [makeNode (SequenceAtom ir) (EventAtom il) eventMatched]

        (SequenceAtom il, SequenceAtom ir) ->
            case compare il ir of
                LT -> [makeNode al ar temporalMatched,
                       makeNode al (EventAtom ir) eventMatched]
                EQ -> [makeNode al ar temporalMatched]
                GT -> [makeNode al ar temporalMatched]

    where Node {seq_ = Sequence (al:prefixLeft), idList = idsLeft} = left
          Node {seq_ = Sequence (ar:prefixRight), idList = idsRight} = right
          eventMatched = eventMatch idsLeft idsRight
          temporalMatched = temporalMatch idsLeft idsRight
          makeNode a1 a2 idList = Node {seq_ = Sequence (a2:a1:prefixLeft),
                                        idList = idList,
                                        support = supportOf idList,
                                        generatedBy = [left]}


temporalJoin :: Int -> Node -> Node -> [Node]
temporalJoin minSup l r =
    let candidates = generateCandidates l r
    in  filter (\n -> support n >= minSup) candidates


enumerateFrequentSeq :: Int -> [Node] -> [Node]
enumerateFrequentSeq minSup nodes =
    let generateClass :: Node -> [Node]
        generateClass leftAtom =
            foldMap (temporalJoin minSup leftAtom) nodes

        prefixClasses :: [[Node]]
        prefixClasses = map generateClass nodes

    in (foldMap (enumerateFrequentSeq minSup) prefixClasses) ++ nodes
