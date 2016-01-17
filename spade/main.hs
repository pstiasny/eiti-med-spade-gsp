import System.Environment
import qualified Data.Map as Map
import Spade
import qualified IdList


sequenceToHtml :: Sequence -> String
sequenceToHtml (Sequence l) =
    foldl (++) "" $ map atomToHtml $ reverse l
    where atomToHtml a =
            case a of
                EventAtom ea -> ea
                SequenceAtom sa -> " -&gt; " ++ sa

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

loadHorizontalDb = do
    ct <- getContents
    let l = lines ct

        readHdbLine :: String -> (IdList.Sid, IdList.Eid, [Item])
        readHdbLine l =
            let ws = words l
            in  (read (ws!!0) :: Int, read (ws!!1) :: Int, drop 2 ws)
        lw = map readHdbLine l

        itemEvents :: [(Item, (IdList.Sid, IdList.Eid))]
        itemEvents = foldMap (\(sid, eid, items) -> map (\item -> (item, (sid, eid))) items) lw
        appendId :: (IdList.Sid, IdList.Eid) -> Maybe [(IdList.Sid, IdList.Eid)] -> Maybe [(IdList.Sid, IdList.Eid)]
        appendId x (Just items) = Just (items ++ [x])
        appendId x Nothing = Just [x]
        itemMap = foldl (\acc (item, id_) -> Map.alter (appendId id_) item acc) Map.empty itemEvents

    return $ Map.toList itemMap

main = do
    args <- getArgs

    hdb <- loadHorizontalDb

    let t = filter (\n -> support n >= 2) $ loadDatabase hdb
    let fs = enumerateFrequentSeq 2 t

    let showSeqSupport node = (show $ seq_ node) ++ " [" ++ (show $ support node) ++ "]"
    if "-g" `elem` args
        then do
            putStrLn "graph sequences {"
            mapM_  printSequence fs
            putStrLn "}"
        else mapM_ putStrLn $ map showSeqSupport fs
