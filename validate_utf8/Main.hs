{-# LANGUAGE BinaryLiterals #-}
module Main where

import Data.Bits
import Data.Word

getByteLength :: Word8 -> Int
getByteLength = (+ 1) . length . takeWhile (== mask) . map (.&. mask) . iterate (`shiftL` 1)
    where mask = 0b11000000 

hasValidByteStart :: Word8 -> Bool
hasValidByteStart = (== 0b10000000) . (.&. 0b11000000) 

isValidUtf8 :: [Word8] -> Bool
isValidUtf8 [] = False
isValidUtf8 (x:xs) 
    | byteLength < 1 || byteLength > 4 = False
    | byteLength == 1 = null xs
    | otherwise = length xs == byteLength - 1 && all hasValidByteStart xs
    where byteLength = getByteLength x
    

main :: IO ()
main = do
    putStrLn "Input: [0b00000000]"
    putStrLn "Expected Result: True"
    putStrLn $ "Actual Result: " ++ show (isValidUtf8 [0b00000000])
    putStrLn $ "Input: [0b00000000, 0b00000000]"
    putStrLn $ "Expected Result: False"
    putStrLn $ "Actual Result: " ++ show (isValidUtf8 [0b00000000, 0b00000000])
    putStrLn $ "Input: [0b11110000, 0b10000000, 0b10000000, 0b10000000]"
    putStrLn $ "Expected Result: True"
    putStrLn $ "Actual Result: " ++ show (isValidUtf8 [0b11110000, 0b10000000, 0b10000000, 0b10000000])
