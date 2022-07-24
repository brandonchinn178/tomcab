import qualified Data.List.NonEmpty as NonEmpty
import System.Environment (getArgs)
import Tomcab (runTomcab)

main :: IO ()
main = runTomcab . fmap NonEmpty.toList . NonEmpty.nonEmpty =<< getArgs
