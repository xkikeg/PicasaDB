import Data.Word
import Data.PicasaDB.Reader
import Numeric
import Control.Monad ((>=>), (<=<))
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as E
import System.Environment (getArgs)
import Text.Printf

-- x y z 
-- f = \x y -> showHex x (' ' : y)
-- f = flip g
-- g = \y x -> flip showHex (' ' : y) x
--   = flip showHex . (' ' :)

hexDump :: BL.ByteString -> String
hexDump = foldr f "" . BL.unpack
  where f = flip $ flip showHex' . (' ' :)
        showHex' x | x < 16    = ('0' :) . showHex x
                   | otherwise = showHex x


getListUntil :: G.Get a -> G.Get [a]
getListUntil f = do
  empty <- G.isEmpty
  case empty of
    True  -> return  []
    False -> do
      x <- f
      return . (x :) =<< getListUntil f


getHeader :: G.Get Int
getHeader = ensureMagic >> G.getWord32le >>= return . fromIntegral
  where
    ensureMagic = getConditional G.getWord32be (== 0x66664640)


getEntry :: G.Get (String, String)
getEntry = do
  path <- return . TL.unpack =<< getUtf8LazyTextNul
  info <- return . hexDump =<< G.getLazyByteString 30
  return (path, info)


readThumbsIndex :: FilePath -> IO [(String, String)]
readThumbsIndex = return . G.runGet (getHeader >> getListUntil getEntry) <=< BL.readFile


main = do
  args <- getArgs
  mapM_ (readThumbsIndex >=> mapM_ (putStrLn . uncurry f)) args
  where
    f = \x y -> x ++ "\t" ++ y
