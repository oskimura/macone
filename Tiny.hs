module Tiny where

import Text.ParserCombinators.Parsec hiding (State)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Expr
import Data.Char (digitToInt)

import System.Environment (getArgs)
import System.IO
import Debug.Trace

import MacOneAS
import MacOne ( State
              , assembler
              , addrBound
              , wordBound
              , signBound
              , wordBound
              , instructionCycle
              , boot
              , addrOsr
              , addrOut
              , operBound
              )


-- Tiny
type Tiny = [TStatement]
data TStatement = Declare Variable Expression
                | Assign  Variable Expression
                | If    Expression [TStatement] [TStatement]
                | While Expression [TStatement]
                | Print Expression
                  deriving Show

data Expression = Add   Expression Expression
                | Sub   Expression Expression
                | Eq    Expression Expression
                | Less  Expression Expression
                | Var   Variable
                | CONST Int
                | TRUE
                | FALSE
                  deriving Show

type Variable = String



-- tiny scanner
tiny       = P.makeTokenParser tinyDef
tinyDef = emptyDef
          { P.reservedNames = ["var"
                              , "if", "then", "else"
                              , "while", "do", "end"
                              , "print"
                              , "const"
                              , "true", "false"
                              ]
           , P.reservedOpNames = [ ":=", "+", "-", "<"]
          }
parens     = P.parens      tiny
symbol     = P.symbol      tiny
identifier = P.identifier  tiny
reserved   = P.reserved    tiny
whiteSpace = P.whiteSpace  tiny
integer    = P.integer     tiny
reservedOp = P.reservedOp  tiny

infixExpr :: Parser Expression
infixExpr = buildExpressionParser operators simpleExpr
operators =
    [ [ op "+" AssocLeft Add
      , op "-" AssocLeft Sub
      ]
    , [ op "=" AssocLeft Eq
      , op "<" AssocLeft Less
      ]
--    , [ op ":=" AssocRight Assign]
    ]
    where

      op name assoc con = Infix (do{ reservedOp name
                               ; return (\ e1 e2 -> con e1 e2)
                               }
                            )
                           assoc


-- Tiny Parser
program :: Parser Tiny
program =
    do { many statement }
-- Statement
statement :: Parser TStatement
statement =
    do { choice [ declareStmt
                , assignStmt
                , ifStmt
                , whileStmt
                , printStmt
                ]
       }
declareStmt :: Parser TStatement
declareStmt =
    do { reserved "var"
       ; v <- variable
       ; symbol ":="
       ; e <- expr
       ; return $ Declare v  e
       }
assignStmt :: Parser TStatement
assignStmt =
    do { v <- variable
       ; symbol ":="
       ; e <- expr
       ; return $ Assign  v e
       }
ifStmt :: Parser TStatement
ifStmt =
    do { reserved "if"
       ; cond_expr <- expr
       ; reserved "then"
       ; true_stmts <- many  statement
       ; reserved "else"
       ; false_stmts <- many statement
       ; return $ If cond_expr true_stmts false_stmts
       }
whileStmt :: Parser TStatement
whileStmt =
    do { reserved "while"
       ; cond_expr <- expr
       ; reserved "do"
       ; stmts <- many statement
       ; reserved "end"
       ; return $ While cond_expr stmts
       }
printStmt :: Parser TStatement
printStmt =
    do { reserved "print"
       ; e <- expr
       ; return $ Print e
       }
-- Expr
exprs :: Parser [Expression]
exprs = many expr
expr :: Parser Expression
expr =
    do { choice [ addExpr
                , subExpr
                , eqExpr
                , lessExpr
                , simpleExpr
                ]
       }
simpleExpr =
    do { choice [ constExpr
                , trueExpr
                , falseExpr
                , varExpr
                , parens expr
         ]
    }
addExpr :: Parser Expression
addExpr = infixExpr
    -- do { e1 <- expr
    --    ; symbol "+"
    --    ; e2 <- expr
    --    ; return $ Add e1 e2
    -- }

subExpr :: Parser Expression
subExpr = infixExpr
    -- do { e1 <- expr
    --    ; symbol "-"
    --    ; e2 <- expr
    --    ; return $ Sub e1 e2
    --    }
eqExpr :: Parser Expression
eqExpr = infixExpr
    -- do { e1 <- expr
    --    ; symbol "="
    --    ; e2 <- expr
    --    ; return $ Eq e1 e2
    -- }
lessExpr :: Parser Expression
lessExpr = infixExpr
    -- do { e1 <- expr
    --    ; symbol "<"
    --    ; e2 <- expr
    --    ; return $ Less e1 e2
    -- }
varExpr :: Parser Expression
varExpr =
    do { v <- variable
       ; return $ Var v
       }
constExpr :: Parser Expression
constExpr =
    do { num <- int
       ; return $ CONST num
       }
trueExpr :: Parser Expression
trueExpr  = symbol "true"  >> return TRUE
falseExpr :: Parser Expression
falseExpr = symbol "false" >> return FALSE

variable :: Parser Variable
variable =
    do { id <- identifier
       ; return id
       }
int :: Parser Int
int =
    do { num <- integer
       ; return . fromInteger $ num
       }



--------------------------------------------------------
compile :: Tiny -> [Word]
compile = assembler . compiler

compiler :: Tiny -> Assembly
compiler prog =
    instructions ++ [Stop] ++ fixed_code ++ declare decls
    where
      (instructions, decls, _) = comp prog ["l_" ++ show n | n <- [1..]]

comp :: [TStatement] -> [String] -> ([AStatement],[(String,Int)],[String])
comp []       labels = ([],[],labels)
comp (s:rest) labels = (s_code ++ code_rest, decl ++ decl_rest, labels'')
    where
      (s_code,    decl,      labels')  = comp_s s labels
      (code_rest, decl_rest, labels'') = comp rest labels'


comp_s :: TStatement -> [String] -> ([AStatement],[(String,Int)],[String])
comp_s (Declare v (CONST n)) labs =    ([],[(v,n)],labs)
comp_s (Declare v TRUE)      labs =    ([],[(v,1)],labs)
comp_s (Declare v FALSE)     labs =    ([],[(v,1)],labs)
comp_s (Assign  v  e)        labs =    (e_code ++ [Stod (L v)], [],labs')
    where
      (e_code, e_decls, labs') = comp_e e labs

comp_s (If c t e)     (l1:l2:labs) =
    ( c_code ++ [Jzer (L l1)] ++ t_code ++ [Jump (L l2), Label l2]
    , c_decls ++ t_decls ++ e_decls
    , labs3
    )
    where
      (c_code, c_decls, labs1) = comp_e c labs
      (t_code, t_decls, labs2) = comp   t labs1
      (e_code, e_decls, labs3) = comp   e labs2

comp_s (While c b) (l1: l2:labs) =
    ( [Label l1] ++ c_code ++ [Jzer (L l2)] ++ b_code ++ [Jump (L l1), Label l2]
    , c_decls ++ b_decls
    , labs2
    )
    where
      (c_code, c_decls, labs1) = comp_e c labs
      (b_code, b_decls, labs2) = comp   b labs1
comp_s (Print e) labs =
    ( e_code ++ [Call (L "print")]
    , []
    , labs1
    )
    where
      (e_code, e_decls, labs1) = comp_e e labs

-- expression
comp_e :: Expression -> [String] -> ([AStatement],[(String,Int)],[String])
comp_e (Add e1 e2) labs =
    ( e2_code ++ [Push] ++ e1_code ++ [Addl 0,Insp 1]
    , e1_decls ++ e2_decls
    , labs2
    )
    where
      (e1_code, e1_decls, labs1) = comp_e e1 labs
      (e2_code, e2_decls, labs2) = comp_e e2 labs1

comp_e (Sub e1 e2) labs =
    ( e2_code ++ [Push] ++ e1_code ++ [Subl 0, Insp 1]
    , e1_decls ++ e2_decls
    , labs2
    )
    where
      (e1_code, e1_decls, labs1) = comp_e e1 labs
      (e2_code, e2_decls, labs2) = comp_e e2 labs1
comp_e (Eq e1 e2) labs =
    ( e2_code  ++ [Push] ++ e1_code ++ [Push,Call (L "Eq")]
    , e1_decls ++ e2_decls
    , labs2
    )
    where
      (e1_code, e1_decls, labs1) = comp_e e1 labs
      (e2_code, e2_decls, labs2) = comp_e e2 labs1
comp_e (Less e1 e2) labs =
    ( e2_code ++ [Push] ++ e1_code ++ [Push,Call (L "Less")]
    , e1_decls ++ e2_decls
    , labs2
    )
    where
      (e1_code, e1_decls, labs1) = comp_e e1 labs
      (e2_code, e2_decls, labs2) = comp_e e2 labs1
comp_e (CONST n) (l:labs)
    | n >= 0 && n < fromInteger operBound = ([Loco n],          [],              labs)
    | n >= addrBound && n < wordBound     = ([Lodd (L l)], [(l,n)],              labs)
    | n < 0 && (- n) < signBound          = ([Lodd (L l)], [(l, n + wordBound)], labs)

comp_e TRUE    labs = ([Loco 1],           [], labs)
comp_e FALSE   labs = ([Loco 0],           [], labs)

comp_e (Var v) labs = ([Lodd (L v)], [], labs)


declare :: [(String,Int)] -> [AStatement]
declare vars =
    concat [ [Label v, Const n] | (v, n) <- vars ]

var :: String -> String
var v = ("v_" ++ v)

run :: Tiny -> State
run = instructionCycle . boot . assembler . compiler


fixed_code :: Assembly
fixed_code
    = [            Label                  -- print、ルーチン、acの数値
      "print",     Push                   -- 数をプッシュしてprint
      ,            Label
      "wait",      Lodd  (C addrOsr)     -- ステータスレジスタのロード
      ,            Jzer  (L "wait")       -- 前の印字終了まで待つ
      ,            Pop                    -- 数をロードして印字
      ,            Stod  (C addrOut)     -- 出力レジスタに数を保存
      ,            Retn

      ,            Label                  -- 等号、スタック上の引数、ac内の結果
      "Eq",        Lodl  1                -- ac := 引数1
      ,            Subl  2                -- ac := 引数1-引数2
      ,            Jzer  (L "Ret_TRUE")   -- ac = 0 ⇔ 引数1 = 引数2
      ,            Label                  -- ac ≠ 0 ⇔ 引数1 ≠ 引数2
      "Ret_FALSE", Pop                    -- FALSEを返す。ac := returnアドレス
      ,            Insp  2                -- 引数の除去
      ,            Push                   -- スタックにアドレスを返す
      ,            Loco  0                -- FALSEのロード
      ,            Retn                   --
      ,            Label                  -- 比較、スタック上の引数、ac内の結果
      "Less",      Lodl  1                -- ac := 引数1
      ,            Subl  2                -- ac := 引数1-引数2
      ,            Jpos  (L "Ret_FALSE")  -- ac ≥ 0 ⇔ 引数1 ≥ 引数2
      ,            Label                  -- ac < 0 ⇔ 引数1 < 引数2
      "Ret_TRUE",  Pop                    -- TRUEを返す。ac := returnアドレス
      ,            Insp  2                -- 引数の除去
      ,            Push                   -- スタックにアドレスを返す
      ,            Loco  1                -- Trueのロード
      ,            Retn                   --
      ]


-- exmaple tiny code
imp_fib :: Int -> Tiny
imp_fib n =
    [ Declare "x" (CONST n)
    , Print   (Var "x")
    , Declare "fibn" (CONST 1)
    , Declare "fibn-1" (CONST 0)
    , Declare "fibn-2" (CONST 0)
    , While (Var "x")             -- x ≠ 0
               [ Assign "fibn-2"  (Var "fibn-1")
               , Assign "fibn-1"  (Var "fibn")
               , Assign "fibn"    (Add (Var "fibn-1") (Var "fibn-2"))
               , Assign "x"       (Sub (Var "x") (CONST 1))
               ]
    , Print (Var "fibn")
 ]

fib'' n =  show . run $ imp_fib n

-- for debug
showAssembly :: Assembly -> [String]
showAssembly asms =
    zipWith (\ n asm -> (show n) ++ ": " ++ asm) [0..] asms'
    where
      asms' = map show asms

-- main :: IO ()
-- main = print . show . traceCycle . boot . assembler . compiler . imp_fib $ 20

-- main :: IO ()
-- main = mapM_  print $ showAssembly . compiler . imp_fib $ 20


-- parse' parser s = result
--     where
--       Right result = parse parser "" s


-- assemble example
-- main :: IO ()
-- main =
--     do{ args  <- getArgs
--       ; print (args!!0)
--       ; file_handle <- openFile (args!!0) ReadMode
--       ; text <- hGetContents file_handle
--       ; print text
--       ; mapM_ print $ parse' program text
--       ; hClose file_handle
--       }

