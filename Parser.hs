module Parser
    ( assembler'
    , parse
    , parse'
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
operandHelper :: (Address -> AStatement) -> String   -> (Parser AStatement)
operandHelper constr op_code  =
    do { string op_code ; spaces 
       
       ; addr <-  (try (do 
                        { num <- many1 digit
                        ; return $ C . fromIntegral . listToDigit . map digitToInt $ num
                        }) 
                  <|> 
                  do {label1 <- letter 
                     ;label2 <- many1 alphaNum
                     ; return $ L (label1 : label2)
                     }                
                 )
       ; return $ constr $ addr
       }

addrHelper :: (Address -> AStatement) -> String   -> (Parser AStatement)
addrHelper constr = operandHelper constr 



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
varHelper constr = operandHelper constr'
    where
      constr' :: Address -> AStatement
      constr' (C addr) = constr  $ (fromIntegral addr)

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
    do { spaces
       ; stmts <- sepEndBy1 assemble' spaces
       ; spaces
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
          where op = (\ x y -> try  (do {spaces;v <- x; spaces ; return v}) <|> y)


parse' parser s = result
    where
     Right result = parse parser "" s
--      case parse parser "" s of
--        Left err -> err
--        Right result = result

-- assemble example


tStr = "woofoobarbazgoo" 

foobarbaz :: Parser String
foobarbaz = do { string "foo"
               ; string "bar"
               ; string "baz"
               ; return "foobaz"
               }
mypar = do { foo <- do {foo'<- (try foobarbaz); cs <- many alphaNum ; return (foo'++cs) }
                  <|> do {c <- alphaNum; cs <- mypar; return (c:cs) }
         ; return foo
         }

