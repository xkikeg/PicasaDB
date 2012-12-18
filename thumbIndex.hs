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


type Entry = (String, String, String, String, Int, Int)


getEntry :: G.Get Entry
getEntry = do
  path <- return . TL.unpack =<< getUtf8LazyTextNul
  b64x <- return . hexDump =<< G.getLazyByteString 8
  b64y <- return . hexDump =<< G.getLazyByteString 8
  info <- return . hexDump =<< G.getLazyByteString 9
  valid <- getConditional (G.getWord8) (\x -> x == 0 || x == 1)
  idir <- return . fromIntegral =<< G.getWord32le
  if valid == 0 && idir /= -1 then fail "invalid with valid idir"
    else return (path, b64x, b64y, info, fromIntegral valid, idir)


readThumbsIndex :: FilePath -> IO [Entry]
readThumbsIndex = return . G.runGet (getHeader >> getListUntil getEntry) <=< BL.readFile


main = do
  args <- getArgs
  mapM_ (readThumbsIndex >=> mapM_ (putStrLn . f)) args
  where
    f = \(u, v, w, x, y, z) -> u ++ concatMap ('\t':) [v, w, x, (show y), (show z)]
