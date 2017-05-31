module Window (

    hamming
  , hammingC

  ) where

import Data.Vector
import qualified Data.Complex as Comp

-- quick conversion to complex
c :: Num a => a -> Comp.Complex a
c = (Comp.:+ 0)

-- A hamming window of size n.
hamming :: Int -> Vector Double
hamming m = generate m hamming' where
  hamming' n = 0.54 - 0.46*cos(2 * pi * (fromIntegral n)/(fromIntegral m-1))

-- A hamming window of type Complex Double of size n
hammingC :: Int -> Vector (Comp.Complex Double)
hammingC m = generate m hamming' where
  hamming' n = c $ 0.54 - 0.46*cos(2 * pi * (fromIntegral n)/(fromIntegral m-1))


