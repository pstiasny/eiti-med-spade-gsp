import Spade


d :: Database
d = [
    ('A', [(1, 15),
           (1, 20),
           (1, 25),
           (2, 15),
           (3, 10),
           (4, 25)]),
    ('B', [(1, 15),
           (1, 20),
           (2, 15),
           (3, 10),
           (4, 20)]),
    ('D', [(1, 10),
           (1, 25),
           (4, 10)]),
    ('F', [(1, 20),
           (1, 25),
           (2, 15),
           (3, 10),
           (4, 20)])
    ]


dropSecondFromLastItem (Sequence (s1:s2:t)) =
    Sequence $ case (s1, s2) of
        (SequenceAtom s1a, _) -> s1:t
        (EventAtom s1a, SequenceAtom s2a) -> (SequenceAtom s1a):t
        (EventAtom s1a, EventAtom s2a) -> s1:t

sequenceToHtml :: Sequence -> String
sequenceToHtml (Sequence l) =
    foldl (++) "" $ map atomToHtml $ reverse l
    where atomToHtml a =
            case a of
                EventAtom ea -> [ea]
                SequenceAtom sa -> " -&gt; " ++ [sa]

printSequence node =
    do
        let s = seq_ node
            seqStr = show s
            printIds (sid, eid) =
                putStr $ "<tr><td>" ++ (show sid) ++ "</td><td>" ++ (show eid) ++ "</td></tr>"

        putStr $ "    \"" ++ seqStr ++ "\" [shape=\"record\" label=<"
        putStr "<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" cellspacing=\"0\">"
        putStr $ "<tr><td colspan=\"2\">"  ++ (sequenceToHtml s) ++ "</td></tr>"
        putStr $ "<tr><td colspan=\"2\">" ++ (show $ support node) ++ "</td></tr>"
        mapM_ printIds (idList node)
        putStrLn "</table>>]"

        let printGeneratedBy s = putStrLn $ "    \"" ++ seqStr ++ "\" -- \"" ++ (show $ seq_ s) ++ "\""
        mapM_ printGeneratedBy (generatedBy node)

main = do
    let t = loadDatabase d
    let fs = enumerateFrequentSeq 2 t

    {-print fs-}

    putStrLn "graph sequences {"
    mapM_  printSequence fs
    putStrLn "}"
