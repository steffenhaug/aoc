module Input
  ( input,
    Cmd (Cd, Ls),
    Fs (Dir, Fi)
  )
where

import Text.ParserCombinators.Parsec

data Cmd
  = Cd String
  | Ls [Fs]
  deriving (Show)

-- File system objects; file or directory.
data Fs
  = Dir String
  | Fi String Int
  deriving (Show)

input f = do
  txt <- readFile f
  let parsed = parse terminal "AoC Input Parser" txt
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
      return (Dir name)
    file = do
      size <- nat
      space
      name <- filename
      return (Fi name size)

nat :: Parser Int
nat = (many1 digit) >>= (return . read)

filename :: Parser String
filename = many1 $ oneOf ('.' : ['a' .. 'z'])

dirname =
  choice
    [ string "..",
      string "/",
      many1 (oneOf ['a' .. 'z'])
    ]
