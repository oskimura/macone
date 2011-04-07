module Main (w16_w8, word8Toword16) where 
import MacOne
import System.Environment (getArgs)
import qualified  Data.ByteString.Lazy as L
import Data.Bits
import Data.Word
import System.Console.GetOpt


data Flag = Object
          | Version
          | DebugMode
          deriving (Show, Eq)

data Parameter =  
    Parameter { objectFile :: String
              , debugMode :: Bool 
              } deriving (Show)
inp,outp :: Maybe String -> Flag
inp  = Source . fromMaybe "stdin"

options :: [OptDescr Flag]
options =
    [ Option []         ["debug"]   (NoArg DebugMode)   "debug mode"
    , Option ['V', '?'] ["version"] (NoArg Version)     "show version number"
    , Option ['c']      []          (OptArg inp "file") "input assembler file"
    ]

vmOpts :: [String] -> IO ([Flag],[String])
vmOpts argv =
    case getOpt Permute options argv of
      (o,n,[])  -> return (o,n)
      (_,_,err) -> ioError (userError (concat err ++ usageInfo header options))
        where
          header = "Usge : ic [OPTION..] files.."



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



