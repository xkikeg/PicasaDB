module Data.PicasaDB where


import Data.Word
import Data.Time
import qualified Data.ByteString.Lazy as BL


data PMPDB = PMPString     [BL.ByteString]
           | PMPWord8      [Word8]
           | PMPWord16     [Word16]
           | PMPWord32     [Word32]
           | PMPWord64     [Word64]
           | PMPStringList [[BL.ByteString]]
           | PMPDateTime   [UTCTime]
           deriving (Show)
