import Data.Maybe (catMaybes)
import Control.Monad (guard, when)
import System.Environment (getArgs)

main = do
    [s] <- getArgs
    let ps = seqProteins s
    when (Nothing `elem` ps) $ error "Invalid input sequence"
    putStrLn $ unwords $ catMaybes ps

seqProteins :: String -> [Maybe String]
seqProteins = map (flip lookup codons) . chunksOf 3

codonData = [ ("Phe", ["TTT","TTC"])
            , ("Leu", ["TTA","TTG","CTT","CTC","CTA","CTG"])
            , ("Ile", ["ATT","ATC","ATA"])
            , ("Met", ["ATG"])
            , ("Val", ["GTT","GTC","GTA","GTG"])
            , ("Ser", ["TCT","TCC","TCA","TCG","AGT","AGC"])
            , ("Pro", ["CCT","CCC","CCA","CCG"])
            , ("Thr", ["ACT","ACC","ACA","ACG"])
            , ("Ala", ["GCT","GCC","GCA","GCG"])
            , ("Tyr", ["TAT","TAC"])
            , ("His", ["CAT","CAC"])
            , ("Gln", ["CAA","CAG","GGT","GGC","GGA","GGG"])
            , ("Asn", ["AAT","AAC"])
            , ("Lys", ["AAA","AAG"])
            , ("Asp", ["GAT","GAC"])
            , ("Glu", ["GAA","GAG"])
            , ("Cys", ["TGT","TGC"])
            , ("Trp", ["TGG"])
            , ("Arg", ["CGT","CGC","CGA","CGG","AGA","AGG"])
            , ("Stop", ["TAA","TAG","TGA"])
            ]

codons :: [(String,String)]
codons = concatMap (\(n,cs) -> map (\c -> (c,n)) cs) codonData

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

