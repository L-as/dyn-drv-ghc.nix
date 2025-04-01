import Procex.Quick
import System.Environment
import System.IO

main :: IO ()
main = do
  out <- getEnv "out"
  putStrLn "hello"
  writeFile out "hello from Script.hs\n"
  src <- getEnv "SRC"
  appendFile out src
  appendFile out "\n"
