module Spade where

import IdList
import Control.Exception
import Data.List
import Data.Foldable

type Item = Char
type Database = [(Item, IdList)]

data Atom = EventAtom Item | SequenceAtom Item deriving (Eq)
instance Show Atom where
    show (EventAtom a) = " " ++ [a]
    show (SequenceAtom a) = "-> " ++ [a]

data Sequence = Sequence [Atom]
instance Show Sequence where
    show (Sequence []) = "{}"
    show (Sequence atoms) = foldl (++) "" (map show $ reverse atoms)
seqLength :: Sequence -> Int
seqLength (Sequence l) = length l

data Node = Node { seq_ :: Sequence,
                   idList :: IdList,
                   support :: Int,
                   generatedBy :: [Node] } deriving (Show)

loadDatabase :: Database -> [Node]
loadDatabase db = map makeAtom db
    where makeAtom (item, idList) = Node {seq_ = Sequence [SequenceAtom item],
                                          idList = idList,
                                          support = supportOf idList,
                                          generatedBy = []}


generateCandidates :: Node -> Node -> [Node]
generateCandidates left right =
    assert (prefixLeft == prefixRight) $
    case (atomLeft, atomRight) of
        (EventAtom al, EventAtom ar) ->
            if al <= ar then [] else [
                makeNode atomRight atomLeft (eventMatch idsLeft idsRight)
            ]
        (EventAtom al, SequenceAtom ar) -> [
                makeNode atomLeft atomRight (temporalMatch idsLeft idsRight)
            ]
        (SequenceAtom al, EventAtom ar) ->
            case (compare al ar) of
                LT -> [makeNode atomLeft atomRight (eventMatch idsLeft idsRight)]
                EQ -> []
                GT -> [makeNode (SequenceAtom ar)
                                (EventAtom al)
                                (eventMatch idsLeft idsRight)]
        (SequenceAtom al, SequenceAtom ar) ->
            case (compare al ar) of
                LT -> [
                    makeNode atomLeft atomRight (temporalMatch idsLeft idsRight),
                    makeNode atomLeft (EventAtom ar) (eventMatch idsLeft idsRight)
                    ]
                EQ -> [makeNode atomRight atomLeft (temporalMatch idsLeft idsRight)]
                GT -> [
                    makeNode atomLeft atomRight (temporalMatch idsLeft idsRight)
                    ]
    where Sequence (atomLeft:prefixLeft) = seq_ left
          Sequence (atomRight:prefixRight) = seq_ right
          idsLeft = idList left
          idsRight = idList right
          makeNode a1 a2 idList = Node {seq_ = Sequence (a2:a1:prefixLeft),
                                        idList = idList,
                                        support = supportOf idList,
                                        {-generatedBy = [left, right]}-}
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

        prefixClasses = map generateClass nodes

    in (foldMap (enumerateFrequentSeq minSup) prefixClasses) ++ nodes
