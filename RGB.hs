{-module RGB (findNearest) where-}

import Control.Exception
import Data.Array.Repa (Array, DIM2, DIM3, D, Z(..), (:.)(..))
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Specialised.Dim2
import Data.Bits
import Data.List
import Data.Maybe
import Data.Ord
import Data.Word
import System.Directory
import System.IO.Error

import qualified Data.Array.Repa          as A
import qualified Data.Array.Repa.IO.DevIL as IL
import qualified Data.IntMap              as I
import qualified Data.Map                 as M

data RGB = RGB !Word8 !Word8 !Word8

black, red, green, blue, white :: RGB
black = RGB 0   0   0
red   = RGB 255 0   0
green = RGB 0   255 0
blue  = RGB 0   0   255
white = RGB 255 255 255

hue :: RGB -> Float
hue (RGB r g b) =
    atan2 (sqrt 3 * fromIntegral (g - b)) (fromIntegral (2 * r - g - b))

distance :: RGB -> RGB -> Word
distance (RGB r0 g0 b0) (RGB r1 g1 b1) = r * r + g * g + b * b
    where r = w8_sad r0 r1
          g = w8_sad g0 g1
          b = w8_sad b0 b1

w8_sad :: Word8 -> Word8 -> Word
w8_sad a b = xor (diff + mask) mask
    where diff = fromIntegral a - fromIntegral b
          mask = diff `unsafeShiftR` 15

colours :: Int -> [RGB]
colours n = sortBy (comparing hue)
    [RGB (norm r) (norm g) (norm b) | r <- [0..n], g <- [0..n], b <- [0..n]]
    where norm x = fromIntegral (x * 255 `quot` (n - 1))

neighbours :: DIM2 -> [DIM2]
neighbours (Z :. y :. x) =
    [ Z :. y + dy :. x + dx | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0 ]

generate :: DIM2 -> DIM2 -> [RGB] -> I.IntMap RGB
generate size _    []     = I.empty
generate size orig (r:rs) = snd (foldr gen (s0, p0) rs)
    where s0 = M.fromList (map (\x -> (x, r)) (neighbours orig))
          p0 = I.singleton (toIndex orig) r
          toIndex = A.toIndex size
          gen rgb (open, placed)
            | M.null open = (open, placed)
            | otherwise   =
                let min = fst (findNearest (distance rgb) open)
                    newlyOpen = filter isOpen (neighbours min)
                    isOpen sh = isInside2 size sh && I.notMember (toIndex sh) placed
                    placed' = I.insert (toIndex min) rgb placed
                    smeared = smear (flip I.lookup placed' . toIndex) newlyOpen
                in (M.delete min open `M.union` M.fromList smeared, placed')

smear :: (DIM2 -> Maybe RGB) -> [DIM2] -> [(DIM2, RGB)]
smear f = map (\x -> (x, average (mapMaybe f (neighbours x))))

average :: [RGB] -> RGB
average [] = black
average rs = RGB (norm r) (norm g) (norm b)
    where (r, g, b, n) = foldr f (0, 0, 0, 0 :: Int) rs
          norm x = fromIntegral (x `div` n)
          f (RGB r' g' b') (r, g, b, n) =
            (fromIntegral r' + r, fromIntegral g' + g, fromIntegral b' + b, n + 1)

findNearest :: Ord a => (v -> a) -> M.Map k v -> (k, a)
findNearest ord m0 = M.foldrWithKey minBy (k0, ord v0) m1
    where ((k0, v0), m1) = M.deleteFindMin m0
          minBy k v x@(_, a') = let a = ord v in if a < a' then (k, a) else x

serialize :: DIM2 -> I.IntMap RGB -> IO (Array F DIM3 Word8)
serialize size m0 = A.computeP (A.fromFunction (size :. 3) f)
    where f (sh :. z) =
            case I.lookup (A.toIndex size sh) m0 of
                Nothing -> 0
                Just (RGB r g b) -> case z of 0 -> r; 1 -> g; 2 -> b;

removeIfExists :: FilePath -> IO ()
removeIfExists f = removeFile f `catch` dne
    where dne ex | isDoesNotExistError ex = return ()
                 | otherwise = throwIO ex

main :: IO ()
main = do
    img <- serialize size (generate size orig (colours bits))
    removeIfExists fout
    IL.runIL (IL.writeImage fout (IL.RGB img))
    where size = A.ix2 1080 1920
          orig = A.ix2 540  960
          bits = 32
          fout = "rgb.png"
