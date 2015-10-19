import           Control.Monad       (when)
import           Distribution.Simple
import           System.Directory    (doesFileExist)
import           System.Process      (readProcess)
import           Data.ByteString.Char8 as BS

gitVersion :: IO ()
gitVersion = do
    let filename = "src/Gitomail/Version.hs"
    ver <- fmap  BS.pack $ readProcess "./version.sh" [] ""

    let override = BS.writeFile filename ver
    e <- doesFileExist filename
    if e then do orig_ver <- BS.readFile filename
                 when (ver /= orig_ver) $ do
                     override
         else override

main :: IO ()
main = gitVersion >> defaultMain
