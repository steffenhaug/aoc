module Input
  ( input,
    Cmd (Cd, Ls),
    Fs (Dir, File),
    filename,
    print_filesystem
  )
where

import qualified Data.Map as Map
import Data.Map (Map, fromList, (!))

import Text.ParserCombinators.Parsec

data Cmd
  = Cd String
  | Ls [Fs]
  deriving (Show)

-- File system objects; file or directory.
data Fs
  = File String Int
  | Dir String (Map String Fs)
  deriving (Show)

filename (File name size) = name
filename (Dir name content) = name

-- Quick and dirty function to inspect the tree.
print_filesystem :: Fs -> IO ()
print_filesystem tree = psexpr' 0 tree
  where
    psexpr' indent tree =
      case tree of
        File name size -> putStrLn $ (replicate indent ' ') ++ name ++ " [" ++ (show size) ++ "]"
        Dir name children -> do
          putStrLn $ (replicate indent ' ') ++ name ++ " (dir)"
          mapM_ (psexpr' (indent + 2)) (Map.elems children)


input f = do
  txt <- readFile f
  let Right parsed = parse terminal "AoC Input Parser" txt
  return parsed

terminal :: Parser [Cmd]
terminal = do
  cmds <- many1 cmd
  eof
  return cmds

cmd = (string "$ ") >> (ls <|> cd)
  where
    ls = do
      string "ls" >> newline
      nodes <- sepEndBy (try dirnode) newline
      return (Ls nodes)
    cd = do
      string "cd" >> space
      path <- dirname
      newline
      return (Cd path)

dirnode = dir <|> file
  where
    dir = do
      string "dir"
      space
      name <- dirname
      return (Dir name Map.empty)
    file = do
      size <- nat
      space
      name <- many1 $ oneOf ('.' : ['a' .. 'z'])
      return (File name size)

nat :: Parser Int
nat = (many1 digit) >>= (return . read)

dirname =
  choice
    [ string "..",
      string "/",
      many1 (oneOf ['a' .. 'z'])
    ]
