import qualified Data.Text.Lazy.IO as L
import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Control.Monad.Writer

import Text.Morphosyntax.Tagset (parseTagset)
import Text.Morphosyntax.Plain (parsePlain, showPlain)
import Data.Morphosyntax.Sync

main = do
    [tagsetPath, goldPath, otherPath, resPath] <- getArgs
    tagset <- parseTagset tagsetPath <$> readFile tagsetPath
    xs <- parsePlain tagset <$> L.readFile goldPath
    ys <- parsePlain tagset <$> L.readFile otherPath
    let (zs, log) = runWriter (sync xs ys)
    L.writeFile resPath (showPlain tagset zs)
    mapM_ putStrLn log
