module Main where

import Data.Bits
import Data.Char
import Data.Word
import System.Environment

toHex :: Char -> Word8
toHex ch = fromIntegral $ digitToInt ch

toChar :: Word8 -> Char
toChar = intToDigit.fromIntegral

hexChXor :: Char -> Char -> Char
hexChXor ch1 ch2
	| isHexDigit ch1 = toChar $ (toHex ch1) `xor` (toHex ch2)
	| otherwise = ch1

main = do
	(a:b:_) <- getArgs
	putStr a
	putStr " xor "
	putStr b
	putStr " = "
	putStrLn $ zipWith (hexChXor) a b