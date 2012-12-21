import Data.Word
import Data.DateTime
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


fromHiResTime :: Integral a => a -> DateTime
fromHiResTime = flip addSeconds origin . flip div 10000000 . fromIntegral
  where
    origin = fromGregorian 1601 1 1 0 0 0


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


type Entry = (String, DateTime, DateTime, String, Int, String, Int, Int)


getEntry :: G.Get Entry
getEntry = do
  path <- return . TL.unpack =<< getUtf8LazyTextNul
  ctime <- return . fromHiResTime =<< G.getWord64le
  mtime <- return . fromHiResTime =<< G.getWord64le
  infx <- return . hexDump =<< G.getLazyByteString 4
  tval <- return . fromIntegral =<< G.getWord8
  infy <- return . hexDump =<< G.getLazyByteString 4
  valid <- G.getWord8
  idir <- return . fromIntegral =<< G.getWord32le
  if valid == 0 && idir /= -1 then fail "invalid with valid idir"
    else return (path, ctime, mtime, infx, tval, infy, fromIntegral valid, idir)


readThumbsIndex :: FilePath -> IO [Entry]
readThumbsIndex = return . G.runGet (getHeader >> getListUntil getEntry) <=< BL.readFile


main = do
  args <- getArgs
  mapM_ (readThumbsIndex >=> mapM_ (putStrLn . f)) args
  where
    f = \(s, t, u, v, w, x, y, z) -> s ++ concatMap ('\t':) [show t, show u, v, show w, x, show y, show z]
