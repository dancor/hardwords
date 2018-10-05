{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Exception
import qualified Crypto.KDF.Scrypt as S
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Options.Applicative
import System.Directory
import System.Entropy
import System.Environment
import System.FilePath
import System.IO

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
    map (("- " <>) . snd) wds
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
    BSC.unlines $
    (BS.concat $ map fst wds) :
    map (("- " <>) . snd) wds
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
    let procLine x = (BSC.map toLower . head $ BSC.words x, x)
    wds3 <- map procLine . BSC.lines <$> BS.readFile "/usr/share/dict/csw3"
    wds4 <- map procLine . BSC.lines <$> BS.readFile "/usr/share/dict/csw4"
    let err = error "Dict only has wds3 and wds4."
    return [err, err, err, wds3, wds4]

derivePass :: Dict -> String -> String -> BS.ByteString
derivePass dict masterPass siteDomain =
    prettyWebPass59Bit dict $ S.generate
        (S.Parameters (2^16) 8 1 64)
        (BSC.pack $ masterPass ++ ":" ++ siteDomain)
        (BS.pack [0..31])

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

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getMasterPass :: IO String
getMasterPass = do
    putStr "Enter your hardwords master password: "
    hFlush stdout
    masterPass <- withEcho False getLine
    putChar '\n'
    return masterPass

data Opts = Opts
  { oNewMasterPass :: Bool
  , oAsInt         :: Bool
  --  , oAsRangeInt    :: Bool
  , oSiteDomain    :: Maybe String
  }

opts :: Parser Opts
opts = Opts
    <$> switch (long "new-master-password"
        <> help "To generate a new master password")
    <*> switch (long "as-integer"
        <> help "Generate an integer instead of a passphrase")
    <*> optional (argument str $ metavar "WEBSITE-DOMAIN")

main :: IO ()
main = hardwords =<< execParser (info (opts <**> helper) $
    fullDesc <> progDesc "descc" <> header "headerr")

hardwords :: Opts -> IO ()
hardwords opts = do
    dict <- loadDict
    case (oNewMasterPass opts, oSiteDomain opts) of
      (True, Nothing) -> do
        pass <- prettyPass80Bit dict <$> getEntropy 10
        BSC.putStrLn pass
      (False, Just siteDomain) -> do
        masterPass <- getMasterPass
        BS.putStr $ derivePass dict masterPass siteDomain
      _ -> error "Usage"
