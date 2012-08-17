import Control.Monad (forM_)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Text.Morphosyntax.TeiNCP (readMorph, canoWord, sentences, segments)
import Text.Morphosyntax.Plain (showPlain)

main = do
    [teiPath] <- getArgs
    parts <- readMorph teiPath
    forM_ parts $ \(path, para) -> do
        putStrLn $ "### " ++ path
        let xs = concatMap sentences para
        let ys = map (map canoWord . segments) xs
        L.putStrLn (showPlain ys)
