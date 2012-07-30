-- Compare two plain files and print
-- accuracy lower bound.

import qualified Data.Text.Lazy.IO as L
import System.Environment (getArgs)
import Control.Applicative ((<$>))

import Data.Morphosyntax.Compare
import Text.Morphosyntax.Tagset (parseTagset)
import Text.Morphosyntax.Plain (parsePlain)

main = do
    [tagsetPath, goldPath, otherPath] <- getArgs
    tagset <- parseTagset tagsetPath <$> readFile tagsetPath
    xs <- concat . parsePlain tagset <$> L.readFile goldPath
    ys <- concat . parsePlain tagset <$> L.readFile otherPath
    let s = stats tagset xs ys
    putStrLn $ "number of words in gold part: " ++ show (gold s)
    putStrLn $ "number of correct tags: " ++ show (good s)
    putStrLn $ "weak accuracy lower bound: " ++ show (weakAccuracyLB s)
