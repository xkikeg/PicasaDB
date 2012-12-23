{-
Copyright 2012 liquid_amber

This file is part of PicasaDB.

PicasaDB is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

PicasaDB is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with PicasaDB.  If not, see <http://www.gnu.org/licenses/>.
-}

module Data.PicasaDB.Reader where


import Data.PicasaDB

import Numeric (showHex)
import Data.Word
import Data.CSV.Conduit
import qualified Data.CSV.Conduit.Parser.Text as CSVT
import Data.Binary.IEEE754
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.DateTime as DT
import Data.Time
import Data.Time.Clock.POSIX


getConditional :: (Show a) => (G.Get a) -> (a -> Bool) -> G.Get a
getConditional parser cond = do
  x <- parser
  if cond x then return x else fail ("Unmet condition: " ++ show x)


getUtf8LazyTextNul :: G.Get TL.Text
getUtf8LazyTextNul = G.getLazyByteStringNul >>= return . E.decodeUtf8


getCSV :: G.Get [TL.Text]
getCSV = do
  str <- return . TL.toStrict =<< getUtf8LazyTextNul
  let x = CSVT.parseRow defCSVSettings str
  case x of
    Left  err        -> fail err
    Right Nothing    -> return []
    Right (Just res) -> return $ map TL.fromStrict res


parseVariantTime :: Double -> UTCTime
parseVariantTime = posixSecondsToUTCTime . realToFrac . (* 86400) . (+ (-25569))
-- days from DT.fromGregorian 1899 12 30 0 0 0


getVariantTime :: G.Get UTCTime
getVariantTime = getFloat64le >>= return . parseVariantTime


parseHiResTime :: Word64 -> UTCTime
parseHiResTime = flip DT.addSeconds origin . flip div 10000000 . fromIntegral
  where
    origin = DT.fromGregorian 1601 1 1 0 0 0


getHiResTime :: G.Get UTCTime
getHiResTime = G.getWord64le >>= return . parseHiResTime


getList :: (G.Get a) -> Int -> G.Get [a]
getList f n | n == 0    = return []
            | otherwise = do
  x  <- f
  xs <- getList f (n - 1)
  return (x : xs)


getListUntil :: G.Get a -> G.Get [a]
getListUntil f = do
  empty <- G.isEmpty
  case empty of
    True  -> return  []
    False -> do
      x <- f
      return . (x :) =<< getListUntil f


hexDump :: BL.ByteString -> String
hexDump = foldr f "" . BL.unpack
  where f = flip $ flip showHex' . (' ' :)
        showHex' x | x < 16    = ('0' :) . showHex x
                   | otherwise = showHex x


getPMPDB :: G.Get PMPDB
getPMPDB = do
  magic       <- getConditional G.getWord32le (== 0x3fcccccd)
  fieldType   <- getConditional G.getWord16le (<  0x8)
  constant1   <- getConditional G.getWord16le (== 0x1332)
  constant2   <- getConditional G.getWord32le (== 0x00000002)
  fieldType'  <- getConditional G.getWord16le (== fieldType)
  constant3   <- getConditional G.getWord16le (== 0x1332)
  fieldLength <- G.getWord32le >>= return . fromIntegral
  let getL = flip getList fieldLength
      retP = \x y -> getL y >>= return . x
  case fieldType of
    0x0 -> retP PMPString     getUtf8LazyTextNul
    0x1 -> retP PMPWord32     G.getWord32le
    0x2 -> retP PMPDateTime   getVariantTime
    0x3 -> retP PMPWord8      G.getWord8
    0x4 -> retP PMPWord64     G.getWord64le
    0x5 -> retP PMPWord16     G.getWord16le
    0x6 -> retP PMPStringList getCSV
    0x7 -> retP PMPWord32     G.getWord32le


getThumbIndexHeader :: G.Get Int
getThumbIndexHeader = ensureMagic >> G.getWord32le >>= return . fromIntegral
  where
    ensureMagic = getConditional G.getWord32be (== 0x66664640)


getThumbIndexEntry :: G.Get ThumbIndexEntry
getThumbIndexEntry = do
  path      <- return . TL.unpack     =<< getUtf8LazyTextNul
  ctime     <- getHiResTime
  mtime     <- getHiResTime
  infox     <- return . hexDump       =<< G.getLazyByteString 4
  ftype     <- return . intToFileType =<< G.getWord8
  infoy     <- return . hexDump       =<< G.getLazyByteString 4
  valid     <- return . fromIntegral  =<< G.getWord8
  directory <- return . fromIntegral  =<< G.getWord32le
  if valid == 0 && directory /= -1 then fail "invalid with valid idir"
    else return $ ThumbIndexEntry path ctime mtime infox ftype infoy valid directory


getThumbIndexDB :: G.Get [ThumbIndexEntry]
getThumbIndexDB = getThumbIndexHeader >>= getList getThumbIndexEntry
