import Aoc (every, intp, plot, printall, revcat)
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
    pc :: Int
  }
  deriving (Show)

data Crt = Crt
  { pix :: IntMap (Double, Double, Double),
    w :: Int,
    h :: Int
  }
  deriving (Show)

data Ir
  = Noop
  | Addx Int
  deriving (Show)

-- Compute the next state, as well as intermediate states.
execute :: Ir -> Cpu -> (Cpu, [Cpu])
execute ir (Cpu {rx = x, pc = i}) =
  case ir of
    Noop -> (Cpu {rx = x, pc = i + 1}, [])
    Addx dx -> (Cpu {rx = x + dx, pc = i + 2}, [Cpu {rx = x, pc = i + 1}])

-- Light up a pixel.
putpixel crt ij =
  let buf = pix crt
      buf' = Map.insert ij (1, 0, 1) buf
   in crt {pix = buf'}

-- Light up a pixel if rx is close to the cathode ray.
render :: Crt -> Cpu -> Crt
render crt tick@Cpu {rx = x} =
  let ij = pc tick
   in if abs (x - ij `mod` w crt) < 2
        then putpixel crt ij
        else crt

-- Execute one instruction.
step :: Ir -> State (Crt, Cpu, [Cpu]) ()
step ir = do
  (crt, cpu, hist) <- get

  let (cpu', intmd) = execute ir cpu
      crt' = foldl render crt (cpu : intmd)
      past = cpu : hist

  put (crt', cpu', revcat intmd past)
  pure ()

main = do
  ir <- input "input.txt"

  let cpu = Cpu {rx = 1, pc = 0}
      crt =
        Crt
          { pix = Map.empty,
            w = 40,
            h = 6
          }

      (crt', cpu', hist) = execState (mapM step ir) (crt, cpu, [])
      trace = cpu' : hist

      samples = every 40 (drop 19 (reverse trace))
      signals = zipWith (*) (map rx samples) [20, 60 ..]

  printall samples
  print (sum signals)
  plot "crt.png" 10 (h crt', w crt') (pix crt')
