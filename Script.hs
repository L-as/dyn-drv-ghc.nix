{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

import Data.Map qualified as M
import Procex.Quick
import Data.List
import System.Directory
import System.Environment
import System.IO
import Text.Parsec
import Text.Parsec.Char
import Data.Maybe

{- The goal is to extract the dependency graph of all Haskell modules,
   then emit derivations that compile them in the correct order.
   Rought steps:
   - Find all .hs (modules) and .hs-boot (module interfaces) files.
   - Extract module name and imports, and whether each import is a SOURCE import, by doing limited parsing.
   - Create map connecting module name to file path, err if there are multiple options (TODO: support),
     except for `Main` modules, which are assumed to not be used as a dependency in other modules.
   - Create map representing the dependency graph.
   - For each module and module interface, create a derivation that generates the compiled module interface,
     depending on the derivations producing the module interfaces for the dependencies.
   - For each module, create a derivation that generates the compiled object file,
     depending on the derivations producing the module interfaces for the dependencies.
   - For each `Main` module, export the list of compiled object files needed for it.
   - The downstream user must invoke the linker to produce the final executable,
     since we don't know what external libraries or C files etc. it depends on.

   The story for cross-compilation should be exactly the same, but interestingly
   the compiled module interfaces can be reused, reducing the compilation time
   somewhat if you compile for multiple architectures.

   The strategy should also work for compilers other than GHC, and even for other languages,
   if they somewhat have the same structure.
   Notably, C++ modules are very similar to this. Without modules, it's simpler since you
   don't need to compile module interfaces.

   FIXME: We're not parsing comments. We need to.
-}

endsWith = \xs -> go xs xs
  where
    go [] _ _ = True -- vacuous truth
    go _ [] [] = True -- done, return true
    go xs [] ys = go xs xs ys -- failed impermanently, try again
    go _ _ [] = False -- failed permanently, give up
    go xs (x' : xs') (y : ys) -- possible match
      | x' == y = go xs xs' ys -- succeeded, move on to next
      | otherwise = go xs xs ys -- failed impermanently, try again

data SourceKind = Impl | Intf

data Import = Import {name :: String, k :: SourceKind}

data ParseResult = ParseResult {name :: Maybe String, imports :: [Import]}

splitOn' :: (Eq a) => [a] -> a -> [a] -> [[a]]
splitOn' acc splitter [] = []
splitOn' acc splitter (x : xs)
  | splitter == x = reverse acc : splitOn' [] splitter xs
  | otherwise = splitOn' (x : acc) splitter xs

splitOn = splitOn' []

data Name = Main | NotMain {name :: String, rest :: [String]}

type P = Parsec String ()

parseName :: P String
parseName = spaces *> many (alphaNum <|> char '.') <* skipMany anyChar <* eof

run :: Parsec String () a -> String -> a
run parser input
  | Right r <- runParser parser () "<input>" input = r
  | otherwise = error "parser failed"

getModule' :: [String] -> Name
getModule' [] = Main
getModule' (('m' : 'o' : 'd' : 'u' : 'l' : 'e' : ' ' : name) : rest) = NotMain {name = run parseName name, rest}
getModule' (_ : rest) = getModule' rest

parseImport :: P Import
parseImport =
  spaces
    *> ( ((\name -> Import {name, k = Impl}) <$> many (alphaNum <|> char '.'))
           <|> ((\name -> Import {name, k = Intf}) <$> (string "{-# SOURCE #-}" *> spaces *> many (alphaNum <|> char '.')))
       )
    <* skipMany anyChar
    <* eof

getImports :: [String] -> [Import]
getImports [] = []
getImports (('i' : 'm' : 'p' : 'o' : 'r' : 't' : ' ' : im) : rest) = run parseImport im : getImports rest

parse :: String -> ParseResult
parse file = ParseResult {name, imports}
  where
    lines = splitOn '\n' file
    name' = getModule' lines
    (imports, name)
      | Main <- name' = (getImports lines, Nothing)
      | NotMain {name, rest} <- name' = (getImports rest, Just name)

data Derivation = Derivation {contents :: String, name :: String}

constructDerivationMap :: M.Map String (String, [Import]) -> String -> Derivation
constructDerivationMap module_map = ()

makeModuleMap :: (String, ParseResult) -> Maybe (String, (String, [Import]))
makeModuleMap (path, ParseResult {name = Just name, imports}) = Just (name, (path, imports))

unPath '/' = '-'
unPath c = c

placeholderCaOut = "/1rz4g4znpzjwh1xymhjpm42vipw92pr73vdgl6xs1hycac8kf2n9"

mkDerivation :: SourceKind -> FilePath -> (FilePath, ParseResult) -> Derivation
mkDerivation k cc (path, ParseResult {imports}) = Derivation {contents, name = map unPath path} where
  out = "(\"out\",\"\",\"sha256\",\"\")"
  o
    | Impl <- k = "-o"
    | Intf <- k = "-ohi"
  quote xs = '"' : xs <> "\""
  args = intercalate "," $ map quote [ "-c", "-fsplit-sections", "-fPIC", "-O1", o, placeholderCaOut]
  contents = "Derive([" <> out <> "],[],[],\"x86_64-linux\"," <> quote cc <> ",[" <> args <> "],[])"

main :: IO ()
main =
  do
    putStrLn "hello"
    writeFile out "hello from Script.hs\n"
    src <- getEnv "SRC"
    cc <- getEnv "CC"
    appendFile out src
    appendFile out "\n"
    listed <- listDirectory src
    modules_intf <- traverse (\x -> ((x,) . Main.parse) <$> readFile x) $ filter (endsWith ".hs-boot") listed
    modules_impl <- traverse (\x -> ((x,) . Main.parse) <$> readFile x) $ filter (endsWith ".hs") listed
    let derivations_intf = map (mkDerivation Intf cc) modules_intf
    let derivations_impl = map (mkDerivation Impl cc) modules_impl
    pure ()
