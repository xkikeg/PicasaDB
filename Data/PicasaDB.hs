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

listPMPDB :: PMPDB -> IO ()
listPMPDB (PMPString     xs) = mapM_ (putStrLn . TL.unpack) xs
listPMPDB (PMPWord8      xs) = mapM_ (putStrLn . show) xs
listPMPDB (PMPWord16     xs) = mapM_ (putStrLn . show) xs
listPMPDB (PMPWord32     xs) = mapM_ (putStrLn . show) xs
listPMPDB (PMPWord64     xs) = mapM_ (putStrLn . show) xs
listPMPDB (PMPStringList xs) = (putStr . TL.unpack . TL.unlines . map TL.unwords) xs
listPMPDB (PMPDateTime   xs) = mapM_ (putStrLn . show) xs
