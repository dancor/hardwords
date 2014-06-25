import Control.Applicative
import Crypto.Scrypt
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import System.Entropy
import System.Environment
import System.FilePath

type WdEntry = (BS.ByteString, BS.ByteString)

type Dict = [[WdEntry]]

-- Little-endian:
-- > bsToInteger (BS.pack [1,0])
-- 1
-- > bsToInteger (BS.pack [0,2])
-- 512
bsToInteger :: BS.ByteString -> Integer
bsToInteger =
    BS.foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0 . BS.reverse

prettyPass :: [[Int]] -> Dict -> BS.ByteString -> [WdEntry]
prettyPass wdLensOptions dict bs =
    snd $ mapAccumL genWd quot1 wdLens
  where
    (quot1, wdLensOptionN) =
        bsToInteger bs `quotRem` fromIntegral (length wdLensOptions)
    wdLens = wdLensOptions !! fromIntegral wdLensOptionN
    genWd :: Integer -> Int -> (Integer, WdEntry)
    genWd quot wdLen =
        (quot', possWds !! fromIntegral wdN)
      where
        possWds = dict !! wdLen
        (quot', wdN) = quot `quotRem` fromIntegral (length possWds)

prettyWebPass59Bit :: Dict -> BS.ByteString -> BS.ByteString
prettyWebPass59Bit dict bs =
    BSC.unlines $
    (BS.concat (fst wd1 : BSC.pack "1" : wd2' : map fst wdRest)) :
    map snd wds
  where
    wd2' = BSC.cons (toUpper wd2Head) wd2Rest
    Just (wd2Head, wd2Rest) = BSC.uncons $ fst wd2
    wds@(wd1:wd2:wdRest) = prettyPass
        [ [4, 4, 3, 3, 3]
        , [4, 3, 4, 3, 3]
        , [4, 3, 3, 4, 3]
        , [4, 3, 3, 3, 4]
        , [3, 4, 4, 3, 3]
        , [3, 4, 3, 4, 3]
        , [3, 4, 3, 3, 4]
        , [3, 3, 4, 4, 3]
        , [3, 3, 4, 3, 4]
        , [3, 3, 3, 4, 4]
        ] dict bs

prettyPass80Bit :: Dict -> BS.ByteString -> BS.ByteString
prettyPass80Bit dict bs =
    BSC.unlines $ (BS.concat $ map fst wds) : map snd wds
  where
    wds = prettyPass
        [ [4, 3, 3, 4, 3, 3, 3]
        , [4, 3, 3, 3, 4, 3, 3]
        , [3, 4, 3, 4, 3, 3, 3]
        , [3, 4, 3, 3, 4, 3, 3]
        , [3, 4, 3, 3, 3, 4, 3]
        , [3, 3, 4, 4, 3, 3, 3]
        , [3, 3, 4, 3, 4, 3, 3]
        , [3, 3, 4, 3, 3, 4, 3]
        , [3, 3, 4, 3, 3, 3, 4]
        , [3, 3, 3, 4, 4, 3, 3]
        , [3, 3, 3, 4, 3, 4, 3]
        , [3, 3, 3, 4, 3, 3, 4]
        ] dict bs

loadDict :: IO Dict
loadDict = do
    let procLine x = (BSC.map toLower . head . BSC.words $ BS.tail x, x)
    wds3 <- map procLine . BSC.lines <$> BS.readFile "/usr/share/dict/csw3"
    wds4 <- map procLine . BSC.lines <$> BS.readFile "/usr/share/dict/csw4"
    let err = error "Dict only has wds3 and wds4."
    return [err, err, err, wds3, wds4]

newMasterPass :: IO ()
newMasterPass = do
    dict <- loadDict
    pass <- prettyPass80Bit dict <$> getEntropy 32
    BSC.putStrLn pass

derivePass :: Dict -> String -> String -> BS.ByteString
derivePass dict masterPass siteDomain =
    prettyWebPass59Bit dict . getHash $ scrypt
        (fromJust $ scryptParams 16 8 1)
        (Salt $ BS.pack [0..31])
        (Pass . BSC.pack $ masterPass ++ ":" ++ siteDomain)

partitionPass :: Dict -> String -> [[BS.ByteString]]
partitionPass dict x
  | xLen == 0 = [[]]
  | xLen < 3 = []
  | xLen == 3 = maybe [] (\x -> [[x]]) $ lookup (BSC.pack x) (dict !! 3)
  | otherwise = poss4 ++ poss3
  where
    xLen = length x
    (x4, rest4) = splitAt 4 x
    poss4 = maybe [] (\ y -> map (y:) (partitionPass dict rest4)) $
      lookup (BSC.pack x4) (dict !! 4)
    (x3, rest3) = splitAt 3 x
    poss3 = maybe [] (\ y -> map (y:) (partitionPass dict rest3)) $
      lookup (BSC.pack x3) (dict !! 3)

ptnPass :: String -> IO ()
ptnPass x = do
    dict <- loadDict
    BS.putStr . BSC.unlines . head . partitionPass dict .
        map toLower $ filter isAlpha x

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["master"] -> newMasterPass
      ["show", f] -> readFile f >>= ptnPass
      [siteDomain] -> do
        home <- getHomeDirectory
        masterPass <- head . lines <$> readFile
            (home </> ".config" </> "scrygen" </> "master-pw")
        dict <- loadDict
        BS.putStr $ derivePass dict masterPass siteDomain
      _ -> error "usage"
