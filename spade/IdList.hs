module IdList where

type Sid = Int
type Eid = Int
type IdList = [(Sid, Eid)]


supportOf :: IdList -> Int
supportOf ids =
    case ids of
        [] -> 0
        (sid0, _):tail -> snd $ foldl countUnique (sid0, 1) ids
    where countUnique (prev, count) (sid, eid) =
              if sid == prev then (prev, count) else (sid, count+1)


temporalMatch :: IdList -> IdList -> IdList
temporalMatch l r
    | l == []       = []
    | r == []       = []
    | otherwise     = innerTemporalMatch l r
    where innerTemporalMatch ((lsid, leid):lt) ((rsid, reid):rt)
              | lsid < rsid   = temporalMatch lt r
              | lsid > rsid   = temporalMatch l rt
              | leid < reid   = (rsid, reid) : temporalMatch l rt
              | leid >= reid  = temporalMatch l rt


eventMatch :: IdList -> IdList -> IdList
eventMatch l r
    | l == []       = []
    | r == []       = []
    | otherwise     = innerEventMatch l r
    where innerEventMatch ((lsid, leid):lt) ((rsid, reid):rt)
              | lsid < rsid   = eventMatch lt r
              | lsid > rsid   = eventMatch l rt
              | leid < reid   = eventMatch lt r
              | leid == reid  = (rsid, reid) : eventMatch l rt
              | leid > reid   = eventMatch l rt
