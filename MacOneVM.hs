module Main (w16_w8, word8Toword16) where 
import MacOne
import System.Environment (getArgs)
import qualified  Data.ByteString.Lazy as L
import Data.Bits
import Data.Word

main :: IO ()
main = 
    do { args <- getArgs
       ; let file = args!! 0 
         in  (L.readFile  file) >>= 
             (mapM_  print) . traceCycle . boot . word8Toword16 . L.unpack
    
       }
    --mapM_ $ print  traceCycle (boot objFile)
    where
      objFile :: IO String
      objFile = do { args <- getArgs
                   ; let file = args!!0 in
                     return file
                   }
-- Word8 から Word16に変換
word8Toword16 :: [Word8] -> [Word]
word8Toword16 (x1:x2:xs) =
    (f x1 x2) : word8Toword16 xs
    where
      f :: Word8 -> Word8 -> Word
      f x y =   (x' `shiftL` 8) .|. fromIntegral y
          where
            x' :: Word
            x' = fromIntegral x

word8Toword16 (x:[]) = [fromIntegral x]
word8Toword16 [] = []

               
          


w16_w8 = toEnum . fromEnum :: Word8  -> Word



