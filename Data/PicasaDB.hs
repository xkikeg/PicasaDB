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

module Data.PicasaDB where


import Data.Word
import Data.Time
import Data.Maybe
import qualified Data.Text.Lazy as TL


data PMPDB = PMPString     [TL.Text]
           | PMPWord8      [Word8]
           | PMPWord16     [Word16]
           | PMPWord32     [Word32]
           | PMPWord64     [Word64]
           | PMPStringList [[TL.Text]]
           | PMPDateTime   [UTCTime]
           deriving (Show)


listPMPDB :: PMPDB -> IO ()
listPMPDB (PMPString     xs) = mapM_ (putStrLn . TL.unpack) xs
listPMPDB (PMPWord8      xs) = mapM_ (putStrLn . show) xs
listPMPDB (PMPWord16     xs) = mapM_ (putStrLn . show) xs
listPMPDB (PMPWord32     xs) = mapM_ (putStrLn . show) xs
listPMPDB (PMPWord64     xs) = mapM_ (putStrLn . show) xs
listPMPDB (PMPStringList xs) = (putStr . TL.unpack . TL.unlines . map TL.unwords) xs
listPMPDB (PMPDateTime   xs) = mapM_ (putStrLn . show) xs


data FileType = Empty
              | Directory
              | Jpeg
              | Gif
              | Wbmp
              | Photoshop
              | Avi -- ambiguous, avi is only the container
              | Mpeg4 -- ??
              | H264 -- ??
              | Tiff
              | Png
              | NikonRaw -- I'm not sure about Canon or other vendor's RAW.
              | Xml
              | UnExpected --  detecting error
              deriving (Show, Eq)


intToFileTypeValid :: Integral a => a -> Maybe FileType
intToFileTypeValid    0 = Just Empty
intToFileTypeValid 0xe9 = Just Empty
intToFileTypeValid 0x01 = Just Directory
intToFileTypeValid 0x02 = Just Jpeg
intToFileTypeValid 0x03 = Just Gif
intToFileTypeValid 0x05 = Just Directory
intToFileTypeValid 0x06 = Just Wbmp
intToFileTypeValid 0x07 = Just Photoshop
intToFileTypeValid 0x08 = Just Avi
intToFileTypeValid 0x09 = Just Mpeg4
intToFileTypeValid 0x0a = Just H264
intToFileTypeValid 0x0d = Just Tiff
intToFileTypeValid 0x0e = Just Png
intToFileTypeValid 0x12 = Just NikonRaw
intToFileTypeValid 0x1e = Just Xml
intToFileTypeValid    _ = Nothing


intToFileType :: Integral a => a -> FileType
intToFileType = maybe UnExpected id . intToFileTypeValid


data ThumbIndexEntry = ThumbIndexEntry
                       { path :: FilePath
                       , ctime :: UTCTime
                       , mtime :: UTCTime
                       , infox :: String
                       , ftype :: FileType
                       , infoy :: String
                       , valid :: Int
                       , directory :: Int
                       }
                     deriving (Show)


showTSV :: ThumbIndexEntry -> String
showTSV x = tail $ concatMap ('\t':) $ map (flip ($) x) [path,
                                                         show . ctime,
                                                         show . mtime,
                                                         infox,
                                                         show . ftype, 
                                                         infoy,
                                                         show . valid,
                                                         show . directory]

{- END -}
