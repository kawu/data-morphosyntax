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

    let readData path = parsePlain tagset <$> L.readFile path

    -- | Sentence level segmentation.
    segm <- map length <$> readData otherPath

    xs <- concat <$> readData goldPath
    ys <- concat <$> readData otherPath
    let zs = sync tagset xs ys

    L.writeFile resPath (showPlain tagset $ segment segm zs)

segment :: [Int] -> [a] -> [[a]]
segment (n:ns) xs = 
    let (first, rest) = splitAt n xs 
    in  first : segment ns rest
segment [] [] = []
