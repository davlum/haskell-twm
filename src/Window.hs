module Window (

    hamming
  , hammingC
  , c
  , Window
  , CWindow

  ) where

import Data.Vector
import qualified Data.Complex as C

type Window = Vector Double -- Windowing

type CWindow = Vector (C.Complex Double) -- Windowing

type WindowFunction = Int -> Window


-- quick conversion to complex
c :: Num a => a -> C.Complex a
c = (C.:+ 0)

-- A hamming window of size n.
hamming :: Int -> Window
hamming m = generate m hamming' where
  hamming' n = 0.54 - 0.46 * cos (2 * pi * fromIntegral n / (fromIntegral m - 1))

-- A hamming window of type Complex Double of size n
hammingC :: Int -> CWindow
hammingC m = generate m hamming' where
  hamming' n = c $ 0.54 - 0.46 * cos (2 * pi * fromIntegral n / (fromIntegral m - 1))
