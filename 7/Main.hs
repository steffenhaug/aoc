import Control.Monad.State
import Data.Map as Map
import Input

-- Good paper: Gérard Huet - Function Pearl; The Zipper.
-- This AoC solution uses a HashMapped Zippered Tree to represent
-- the file system on the Elves' device.

data Tree
  = File String Int
  | Directory String (Map String Tree)
  deriving (Show)

data Zipper
  = Top
  | Node (Map String Tree) Zipper
  deriving (Show)

data Cursor = Cursor Tree Zipper

name (File name size) = name
name (Directory name content) = name

up (Cursor t z) =
  case z of
    Top -> undefined
    Node down up -> Cursor (reroot down) up
  where
    objname = name t
    reroot down = Directory objname (Map.insert objname t down)

exec :: Cmd -> (State Cursor) ()
exec cmd = do
  fs <- get
  return ()

printall :: Show a => [a] -> IO ()
printall = mapM_ print

one = do
  Right log <- input "test.txt"
  -- a
  --  b
  --    x.txt 69
  --    y.txt 345
  --  c         <- cursor here
  --    z.txt 123
  let c =
        Cursor (Directory "c" $ Map.fromList [("z.txt", File "z.txt" 123)]) $
          Node
            ( Map.fromList
                [ ( "a",
                    Directory "a" $
                      Map.fromList
                        [ ( "b",
                            Directory "b" $
                              Map.fromList
                                [ ("y.txt", File "y.txt" 354),
                                  ("x.txt", File "x.txt" 59)
                                ]
                          )
                        ]
                  )
                ]
            )
            Top

  let Cursor root zipper = up c

  let r =
        Directory
          "c"
          ( fromList
              [ ( "a",
                  Directory
                    "a"
                    ( fromList
                        [ ( "b",
                            Directory
                              "b"
                              ( fromList
                                  [ ("x.txt", File "x.txt" 59),
                                    ("y.txt", File "y.txt" 354)
                                  ]
                              )
                          )
                        ]
                    )
                ),
                ( "c",
                  Directory
                    "c"
                    ( fromList
                        [ ("z.txt", File "z.txt" 123)
                        ]
                    )
                )
              ]
          )

  print zipper
  print root
