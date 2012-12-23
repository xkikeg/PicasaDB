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

import Control.Monad ((>=>), (<=<))
import Data.PicasaDB
import Data.PicasaDB.Reader
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import System.Environment (getArgs)

readPMPDBFile :: FilePath -> IO PMPDB
readPMPDBFile = return . G.runGet getPMPDB <=< BL.readFile

main = do
  args <- getArgs
  mapM_ (readPMPDBFile >=> listPMPDB) args
