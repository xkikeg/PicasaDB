module Data.PicasaDB.Reader where


import Data.PicasaDB
import Data.Word
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import Data.Binary.IEEE754
import Data.Time
import Data.Time.Clock.POSIX


getConditional :: (Show a) => (G.Get a) -> (a -> Bool) -> G.Get a
getConditional parser cond = do
  x <- parser
  if cond x then return x else fail ("Unmet condition: " ++ show x)


-- TODO: incorrect CSV parsing for string containing ',' itself
getCSV :: G.Get [BL.ByteString]
getCSV = G.getLazyByteStringNul >>= return . BL.split 0x2c


getVariantTime :: G.Get UTCTime
getVariantTime = do
  x <- getFloat64le
  return $ posixSecondsToUTCTime (realToFrac $ (x - 25569) * 86400)


parseList :: (G.Get a) -> Int -> G.Get [a]
parseList f n | n == 0    = return []
              | otherwise = do
  x  <- f
  xs <- parseList f (n - 1)
  return (x : xs)


parsePMPDB :: G.Get PMPDB
parsePMPDB = do
  magic       <- getConditional G.getWord32le (== 0x3fcccccd)
  fieldType   <- getConditional G.getWord16le (<  0x8)
  constant1   <- getConditional G.getWord16le (== 0x1332)
  constant2   <- getConditional G.getWord32le (== 0x00000002)
  fieldType'  <- getConditional G.getWord16le (== fieldType)
  constant3   <- getConditional G.getWord16le (== 0x1332)
  fieldLength <- G.getWord32le >>= return . fromIntegral
  let getL = flip parseList fieldLength
      retP = \x y -> getL y >>= return . x
  case fieldType of
    0x0 -> retP PMPString     G.getLazyByteStringNul
    0x1 -> retP PMPWord32     G.getWord32le
    0x2 -> retP PMPDateTime   getVariantTime
    0x3 -> retP PMPWord8      G.getWord8
    0x4 -> retP PMPWord64     G.getWord64le
    0x5 -> retP PMPWord16     G.getWord16le
    0x6 -> retP PMPStringList getCSV
    0x7 -> retP PMPWord32     G.getWord32le
