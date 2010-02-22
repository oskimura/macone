module Main where
import Text.ParserCombinators.Parsec
import Data.Char (digitToInt)
import System.Environment (getArgs)
import System.IO
import qualified  Data.ByteString.Lazy as L(writeFile, pack, unpack)

import Data.Word

import Data.Bits

import  MacOneAS
import  MacOne
import  Parser


-- parse'' parser s = result
--     where
--       result =
--           case parse parser "" s of
--             Left err -> err
--             Right result' -> result'

w8_w16 = toEnum . fromEnum :: Word -> Word8




word16Toword8 :: [Word] -> [Word8]
word16Toword8 xs =
    foldr (\ a b -> (f a) ++ b) [] xs
    where
      f :: Word -> [Word8]
      f x = 
          x1:[x2]
              where
                (x1,x2) = w16Tow8 x

      w16Tow8 :: Word -> (Word8,Word8)
      w16Tow8 b = ( fromIntegral (b `shiftR` 8) :: Word8
                  , fromIntegral b :: Word8) 
            


main :: IO ()
main =
    do{ args  <- getArgs
      ;let source = args!!0 in
       print (args!!0)
      ; file_handle <- openFile (args!!0) ReadMode
      ; text <- hGetContents file_handle
--      ; print text
--      ; mapM_ print $ parse' assembler' tex
--     ; let word  = assembler . parse' assembler' $ text in       
--       mapM_ print (word16Toword8 . assembler . parse' assembler' $ text)
      ; let word  = assembler . parse' assembler' $ text in 
        L.writeFile (outFile (args!!0)) . L.pack  . word16Toword8 $ word 
      ; hClose file_handle
      }
      where
        outFile fn = (++"o") . reverse . snd . break (== '.') $ reverse fn
        --source = (args!!0)
        tprint :: Word -> IO ()
        tprint = print