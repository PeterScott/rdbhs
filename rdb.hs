import Data.Conduit hiding (Done,sequence)
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as B8
import Data.Redis.Rdb
import Control.Monad.IO.Class
import IO

showLines = CL.map (B8.pack . (\x -> show x ++ "\n"))

main = runResourceT $ C.sourceFile "./source.rdb" $= parseRDB $= showLines $$ C.sinkHandle stdout
