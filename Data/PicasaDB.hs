module Data.PicasaDB where


import Data.Word
import Data.Time
import qualified Data.Text.Lazy as TL


data PMPDB = PMPString     [TL.Text]
           | PMPWord8      [Word8]
           | PMPWord16     [Word16]
           | PMPWord32     [Word32]
           | PMPWord64     [Word64]
           | PMPStringList [[TL.Text]]
           | PMPDateTime   [UTCTime]
           deriving (Show)
