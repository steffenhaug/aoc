import Aoc (intp, printall)
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Debug.Trace
import Graphics.Image (Image, Pixel (PixelRGB), RGB, VU, Word8, makeImage)
import qualified Graphics.Image as Im
import Text.ParserCombinators.Parsec hiding (State)

input file = do
  txt <- readFile file
  let Right parsed = parse (sepEndBy1 irp newline) "AoC Input Parser" txt
  pure parsed

irp :: Parser Ir
irp =
  choice
    [ do string "noop"; pure Noop,
      do string "addx"; space; dx <- intp; pure (Addx dx)
    ]

data Cpu = Cpu
  { rx :: Int,
    ir :: Ir,
    pc :: Int
  }
  deriving (Show)

data Crt = Crt
  { pix :: IntMap Bool,
    crtw :: Int,
    crth :: Int
  }
  deriving (Show)

rasterize :: Crt -> Image VU RGB Double
rasterize Crt {pix = p, crtw = w, crth = h} =
  Im.scale Im.Nearest Im.Edge (10, 10) $ Im.makeImage (h, w) getpixel
  where
    getpixel (i, j) =
      let lit = Map.findWithDefault False (w * i + j) p
       in if lit
            then PixelRGB 0.4 0 0.6
            else PixelRGB 0 0 0

data Ir
  = Noop
  | Addx Int
  deriving (Show)

revcat xs ys = (reverse xs) ++ ys

every n [] = []
every n (x : xs) = x : (every n (drop (n - 1) xs))

-- Light up a pixel.
putpixel crt@Crt {pix = p, crtw = w} i j =
  let p' = Map.insert (w * i + j) True p
   in crt {pix = p'}

-- Calculate the pixel we are over at a civen cycle.
pixel Crt {crtw = w} cycle = (cycle `div` w, cycle `mod` w)

-- Compute the next state, as well as intermediate states.
execute :: Ir -> Cpu -> [Cpu]
execute ir (Cpu {rx = x, pc = i}) =
  case ir of
    Noop ->
      [ Cpu {rx = x, pc = i + 1, ir = ir}
      ]
    Addx dx ->
      [ Cpu {rx = x, pc = i + 1, ir = ir},
        Cpu {rx = x + dx, pc = i + 2, ir = ir}
      ]

render :: Crt -> Cpu -> Crt
render crt@Crt {pix = p, crtw = w} tick@Cpu {rx = x, pc = ij} =
  let (i, j) = pixel crt ij
      should_draw = abs (x - j) < 2
   in if should_draw
        then putpixel crt i j
        else crt

-- Execute one instruction.
step :: Ir -> State (Crt, [Cpu]) ()
step ir = do
  (crt, states) <- get
  -- Yikes! O(n) to get last state
  let cpu = last states
      ticks = execute ir cpu

  -- Bug: Currently, the initial state doesnt trigger rendering.
  let crt' = foldl render crt ticks

  -- Yikes! O(n) append
  put (crt', states ++ ticks)
  pure ()

main = do
  ir <- input "input.txt"

  let cpu = Cpu {rx = 1, pc = 0, ir = Noop}
      crt =
        Crt
          { pix = Map.empty,
            crtw = 40,
            crth = 6
          }

      (crt', trace) = execState (mapM step ir) (crt, [cpu])

      samples = every 40 (drop 19 trace)
      signals = zipWith (*) (map rx samples) [20, 60 ..]

  print (sum signals)

  let img = rasterize crt'
  Im.writeImage "crt.png" img

