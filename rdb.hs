import Data.Conduit hiding (Done,sequence)
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as B8
import Data.Redis.Rdb
import Control.Monad.IO.Class
import IO

-- Take things, show them, add newlines, output bytestrings.
showLines = CL.map (B8.pack . (\x -> show x ++ "\n"))

-- Print a tedt RDB file.
printTest :: IO ()
printTest = runResourceT $ C.sourceFile "./source.rdb" $= parseRDB $= showLines $$ C.sinkHandle stdout

-- Print the sum of all key lengths in a large test file
printKeylen :: IO ()
printKeylen = do
  x <- runResourceT $ C.sourceFile "/Users/peterscott/dump.rdb" $$ keylen
  print x

-- Sum of all key lengths in an RDB file
keylen :: Monad m => Sink B8.ByteString m Int
keylen = parseRDB =$= CL.map klen =$ CL.fold (+) 0
    where klen (RDBPair _ _ (RDBString s)) = B8.length s
          klen _ = 0

-- Behold, the small constant memory usage! Victory is mine!
main = printKeylen
