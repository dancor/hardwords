{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Exception
import qualified Crypto.KDF.Scrypt as S
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
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

type WdEntry = (B.ByteString, B.ByteString)

type Dict = [[WdEntry]]

-- Little-endian:
-- > bsToInteger (B.pack [1,0])
-- 1
-- > bsToInteger (B.pack [0,2])
-- 512
bsToInteger :: B.ByteString -> Integer
bsToInteger =
    B.foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0 . B.reverse

prettyPass :: [[Int]] -> Dict -> B.ByteString -> [WdEntry]
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

prettyWebPass59Bit :: Dict -> B.ByteString -> B.ByteString
prettyWebPass59Bit dict bs =
    B8.unlines $
    (B.concat (fst wd1 : "1" : wd2' : map fst wdRest)) :
    map (("- " <>) . snd) wds
  where
    wd2' = B8.cons (toUpper wd2Head) wd2Rest
    Just (wd2Head, wd2Rest) = B8.uncons $ fst wd2
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

prettyPass80Bit :: Dict -> B.ByteString -> B.ByteString
prettyPass80Bit dict bs =
    B8.unlines $
    (B.concat $ map fst wds) :
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
    let procLine x = (B8.map toLower . head $ B8.words x, x)
    wds3 <- map procLine . B8.lines <$> B.readFile "/usr/share/dict/csw3"
    wds4 <- map procLine . B8.lines <$> B.readFile "/usr/share/dict/csw4"
    let err = error "Dict only has wds3 and wds4."
    return [err, err, err, wds3, wds4]

derivePass :: Dict -> String -> String -> B.ByteString
derivePass dict masterPass siteDomain =
    prettyWebPass59Bit dict $ S.generate
    --S.generate
        (S.Parameters (2^16) 8 1 64)
        (B8.pack $ masterPass ++ ":" ++ siteDomain)
        (B.pack [0..31])

partitionPass :: Dict -> String -> [[B.ByteString]]
partitionPass dict x
  | xLen == 0 = [[]]
  | xLen < 3 = []
  | xLen == 3 = maybe [] (\x -> [[x]]) $ lookup (B8.pack x) (dict !! 3)
  | otherwise = poss4 ++ poss3
  where
    xLen = length x
    (x4, rest4) = splitAt 4 x
    poss4 = maybe [] (\ y -> map (y:) (partitionPass dict rest4)) $
      lookup (B8.pack x4) (dict !! 4)
    (x3, rest3) = splitAt 3 x
    poss3 = maybe [] (\ y -> map (y:) (partitionPass dict rest3)) $
      lookup (B8.pack x3) (dict !! 3)

ptnPass :: String -> IO ()
ptnPass x = do
    dict <- loadDict
    B.putStr . B8.unlines . head . partitionPass dict .
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
  --, oAsRangeInt    :: Bool
  , oSiteDomain    :: Maybe String
  }

opts :: Parser Opts
opts = Opts
    <$> switch (long "new-master-password"
        <> help "To generate a new master password")
    <*> switch (long "as-integer"
        <> help "Generate an integer instead of a passphrase")
    <*> optional (argument str $ metavar "WEBSITE-DOMAIN")

optsInfo :: ParserInfo Opts
optsInfo = info (opts <**> helper) $
    fullDesc <> progDesc "descc" <> header "headerr"

main :: IO ()
main = hardwords =<< customExecParser (prefs showHelpOnEmpty) optsInfo

hardwords :: Opts -> IO ()
hardwords opts = do
    dict <- loadDict
    case (oNewMasterPass opts, oSiteDomain opts) of
      (True, Nothing) -> do
        pass <- prettyPass80Bit dict <$> getEntropy 10
        B8.putStrLn pass
      (False, Just siteDomain) -> do
        masterPass <- getMasterPass
        let pass = derivePass dict masterPass siteDomain
            i = bsToInteger pass
            --rLow = 1024
            --rHigh = 65535
            rLow = 1
            rHigh = 1023
        --print $ rLow + i `mod` (rHigh + 1 - rLow)
        B8.putStrLn pass
      _ -> error "Usage"
