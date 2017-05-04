-- | This module implements a data type for natural 32-bit signed integers.
--
-- This is what's used for the @REFRESH@, @RETRY@ and @EXPIRE@ fields as
-- defined by <https://tools.ietf.org/html/rfc1035#section-3.3.13 RFC1035>.
--
module Nexus.NatInt32 (NatInt32(..)) where

import Control.Arrow (first)
import Control.Monad (liftM2)
import Data.Int (Int32)
import GHC.Exception (throw, ArithException(Underflow, Overflow))

-- | A positive 32-bit signed integer ranging from @0@ to @2147483647@
newtype NatInt32 = NatInt32 Int32
    deriving (Eq, Ord)

instance Read NatInt32 where
    readsPrec d = fmap (first NatInt32) . filter (cond . fst) . readsPrec d
            where cond = liftM2 (&&) (>= minVal) (<= maxVal)

instance Show NatInt32 where
    showsPrec d (NatInt32 n) = showsPrec d n

instance Num NatInt32 where
    NatInt32 a + NatInt32 b = checkBounds (fromIntegral a + fromIntegral b)
    NatInt32 a * NatInt32 b = checkBounds (fromIntegral a * fromIntegral b)
    NatInt32 a - NatInt32 b = checkBounds (fromIntegral a - fromIntegral b)
    fromInteger = checkBounds
    signum (NatInt32 n) = NatInt32 (signum n)
    abs = id

instance Real NatInt32 where
    toRational (NatInt32 n) = toRational n

instance Enum NatInt32 where
    pred (NatInt32 0) = enumErr "pred" False
    pred (NatInt32 n) = NatInt32 $ pred n
    succ (NatInt32 n) | n == (maxBound :: Int32) = enumErr "succ" True
                      | otherwise                = NatInt32 $ succ n
    fromEnum (NatInt32 n) = fromEnum n
    toEnum n | n < minVal = enumErr "toEnum" False
             | n > maxVal = enumErr "toEnum" True
             | otherwise = NatInt32 (toEnum n)
    enumFrom (NatInt32 n) = NatInt32 <$> enumFrom n
    enumFromThen (NatInt32 a) (NatInt32 b) = NatInt32 <$> enumFromThen a b

instance Bounded NatInt32 where
    minBound = NatInt32 0
    maxBound = NatInt32 (maxBound :: Int32)

instance Integral NatInt32 where
    quotRem (NatInt32 a) (NatInt32 b) = (NatInt32 q, NatInt32 r)
                                  where (q, r) = quotRem a b
    toInteger (NatInt32 n) = toInteger n

-- This is VERY expensive, but we're only using this for zone files, so it's
-- less an issue and we care more about the constraints rather than speed.
checkBounds :: Integer -> NatInt32
checkBounds n | n < minVal = throw Underflow
              | n > maxVal = throw Overflow
              | otherwise  = NatInt32 $ fromIntegral n

minVal :: Integral n => n
minVal = fromIntegral (minBound :: NatInt32)

maxVal :: Integral n => n
maxVal = fromIntegral (maxBound :: NatInt32)

enumErr :: String -> Bool -> a
enumErr fun minMax =
    errorWithoutStackTrace $ "Nexus.NatInt32." ++ fun ++ ": " ++ desc
  where
    desc = if minMax then "n > " ++ show (maxVal :: NatInt32)
                     else "n < " ++ show (minVal :: NatInt32)
