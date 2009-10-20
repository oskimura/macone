module Parser
    ( assembler'
    , parse
    )
    where
import Text.ParserCombinators.Parsec
import Data.Char (digitToInt)
import System.Environment (getArgs)
import System.IO

import MacOneAS ( Word
                , Addr
                , AStatement(..)
                , Address(..)
                , Assembly
                )

-- auxi function
listToDigit xs = sum $ zipWith (*) (reverse xs) (iterate (*10) 1)


-- auxi func
operandHelper :: (Int -> AStatement) -> String   -> (Parser AStatement)
operandHelper constr op_code  =
    do { string op_code ; spaces ; num <- many digit
       ; let num' = listToDigit . map digitToInt $ num
         in return $ constr $ num'
       }

addrHelper :: (Address -> AStatement) -> String   -> (Parser AStatement)
addrHelper constr = operandHelper (constr .  C)

lodd :: Parser AStatement
lodd = addrHelper Lodd "lodd"
stod = addrHelper Stod "stod"
addd = addrHelper Addd "addd"
subd = addrHelper Subd "subd"
call = addrHelper Call "call"
jpos = addrHelper Jpos "jpos"
jzer = addrHelper Jzer "jzer"
jump = addrHelper Jump "jump"
jneg = addrHelper Jneg "jneg"
jnze = addrHelper Jnze "jnze"


varHelper :: (Int -> AStatement) -> String   -> (Parser AStatement)
varHelper constr = operandHelper constr
-- addrHelper constr = operandHelper (constr .  C)

loco :: Parser AStatement
loco = varHelper Loco "loco"
lodl = varHelper Lodl "lodl"
stol = varHelper Stol "stol"
addl = varHelper Addl "addl"
subl = varHelper Subl "subl"


opecodeHelper :: AStatement -> String -> (Parser AStatement)
opecodeHelper constr ope_code =
    do { string ope_code
       ; return constr
       }

pshi = opecodeHelper Pshi "pshi"
popi = opecodeHelper Popi "popi"
push = opecodeHelper Push "push"
pop  = opecodeHelper Pop  "pop"
retn = opecodeHelper Retn "retn"

insp = varHelper Insp "insp"
desp = varHelper Desp "desp"
const' = varHelper Const "const"

swap :: Parser AStatement
swap = opecodeHelper Swap "swap"
stop = opecodeHelper Stop "stop"

label' :: Parser AStatement
label' =
    do { string "label"; spaces; labl <- many1 alphaNum
       ; return . Label $ labl
       }



assembler' :: Parser [AStatement]
assembler' =
    do { skipMany (char '\n')
       ; stmts <- sepEndBy1 assemble' (many1 (char  '\n'))
       ; skipMany (char '\n')
       ; return stmts
       }
    where
      assemble' :: Parser AStatement
      assemble' = operands
      operands  = foldr1 op
                  [ lodd,   stod, addd, subd, call
                  , jpos,   jzer, jump, jneg, jnze
                  , loco,   lodl, stol, addl, subl
                  , pshi,   popi, push, pop,  retn
                  , swap,   insp, desp
                  , const', stop
                  , label'
                    ]
          where op = (\ x y -> try x <|> y)

parse' parser s = result
    where
      Right result = parse parser "" s


-- assemble example
main :: IO ()
main =
    do{ args  <- getArgs
      ; print (args!!0)
      ; file_handle <- openFile (args!!0) ReadMode
      ; text <- hGetContents file_handle
      ; print text
      ; mapM_ print $ parse' assembler' text
      ; hClose file_handle
      }
