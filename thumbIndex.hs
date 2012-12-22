import Data.Word
import Data.DateTime
import Data.PicasaDB
import Data.PicasaDB.Reader
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

readThumbsIndex :: FilePath -> IO [ThumbIndexEntry]
readThumbsIndex = return . G.runGet (getThumbIndexDB) <=< BL.readFile


main = do
  args <- getArgs
  mapM_ (readThumbsIndex >=> mapM_ (putStrLn . showTSV)) args