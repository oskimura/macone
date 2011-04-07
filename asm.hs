module Main where
import Text.ParserCombinators.Parsec
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Char (digitToInt)
import System.Environment (getArgs)
import System.IO
import System.Console.GetOpt
import Control.Monad (liftM)
import qualified  Data.ByteString.Lazy as L(writeFile, pack, unpack)
import Data.Word
import Data.Bits

import  MacOneAS
import  MacOne
import  Parser


-- Option flag function
data Flag
    = Source String
    | Object String
    | Version
    deriving (Show, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['o']      ["output"] (OptArg outp "file") "output object file"
    , Option ['V', '?'] ["version"] (NoArg Version) "show version number"
    , Option ['c']      []         (OptArg inp "file") "input assembler file"
    ]


inp,outp :: Maybe String -> Flag
inp  = Source . fromMaybe "stdin"
outp = Object . fromMaybe "stdout"

assemblerOpts :: [String] -> IO ([Flag],[String])
assemblerOpts argv =
    case getOpt Permute options argv of
      (o,n,[])  -> return (o,n)
      (_,_,err) -> ioError (userError (concat err ++ usageInfo header options))
        where
          header = "Usge : ic [OPTION..] files.."

data Parameter = Parameter
    { sourceFile :: String
    , objectFile :: String
    } deriving Show 

parseOpts :: ([Flag],[String]) -> Parameter
parseOpts (flags, strings) =
    foldr  parseOpts'  (Parameter{sourceFile = [] , objectFile = []}) 
               (zip flags strings)
    where
      parseOpts' (flag, arg) parameter =
          case flag of 
            (Object _) -> parameter { objectFile = arg }
            (Source _) -> parameter { sourceFile = arg }
            _ -> parameter



-- parse'' parser s = result
--     where
--       result =
--           case parse parser "" s of
--             Left err -> err
--             Right result' -> result'


-- Bit Parser
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
      ; (flags, strings) <- assemblerOpts args
      ; param <- return $ parseOpts (flags,strings)
      ; print param 
      ; file_handle <- openFile (sourceFile param) ReadMode
      ; text <- hGetContents file_handle
--      ; print text
--      ; mapM_ print $ parse' assembler' tex
--      ; let word  = assembler . parse' assembler' $ text in       
--       mapM_ print (word16Toword8 . assembler . parse' assembler' $ text)
      ; let word  = assembler . parse' assembler' $ text in 
        L.writeFile (objectFile param) . L.pack  . word16Toword8 $ word 
      ; hClose file_handle
      }
      where
        outFile fn = (++"o") . reverse . snd . break (== '.') $ reverse fn
        --source = (args!!0)

