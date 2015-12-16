import System.Environment
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
        if length (generatedBy node) == 0
            then putStrLn $ "    \"" ++ seqStr ++ "\" -- \"{}\""
            else mapM_ printGeneratedBy (generatedBy node)

main = do
    args <- getArgs

    let t = loadDatabase d
    let fs = enumerateFrequentSeq 2 t

    let showSeqSupport node = (show $ seq_ node) ++ " [" ++ (show $ support node) ++ "]"
    if "-g" `elem` args
        then do
            putStrLn "graph sequences {"
            mapM_  printSequence fs
            putStrLn "}"
        else mapM_ putStrLn $ map showSeqSupport fs

