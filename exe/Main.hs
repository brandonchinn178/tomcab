import qualified Data.List.NonEmpty as NonEmpty
import System.Environment (getArgs)
import Tomcab (generateCabalFiles)

main :: IO ()
main = generateCabalFiles . fmap NonEmpty.toList . NonEmpty.nonEmpty =<< getArgs
