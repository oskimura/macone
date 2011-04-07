module MacOne
    ( State
    , assembler
    , instructionCycle
    , doIO
    , boot
    , traceCycle
    , addrBound
    , wordBound
    , signBound
    , addrOsr
    , addrOut
    , operBound
    )
    where
import Data.List (concat)
-- import Data.Word
import Debug.Trace
import MacOneAS ( Word
                , Addr
                , AStatement(..)
                , Address(..)
                , Assembly
                )


wordBound = 2^16
signBound = 2^15
addrBound = 2^12
operBound = 2^12

---type Memory = [Word]

data Memory =
    Memory { osr :: Word
           , out :: Word
           , mem :: [Word]
           } deriving Show

addrOsr = 4095
addrOut = 4094
free = 2^15
busy = 0

data State =
    State { pc :: Word
          , ac :: Word
          , sp :: Word
          , mm :: Memory
          } deriving Show


initState =
    State { pc=0
          , ac=0
          , sp=0
          , mm=Memory{osr=free
                     ,out=0
                     ,mem=[]
                     }
          }


(|+|) :: Word -> Word -> Word
(|+|) n m = (n+m) `mod` wordBound

(|-|) :: Word -> Word -> Word
(|-|) n m = (n-m) `mod` wordBound

neg :: Word -> Bool
neg w = w >= signBound


format :: Word -> Int
--format :: Word -> Word
format n
    | n > signBound = fromIntegral(n - wordBound)
    | otherwise     = fromIntegral(n)


store :: [Word] -> Memory
store init =
    Memory { osr = free
           , out = 0
           , mem = evalList $ take addrBound (init ++ repeat 0)
           }
--store init = evalList (take addrBound (init ++ repeat 0))

(|!|) :: Memory -> Addr -> Word
(|!|) mm a
      | a == addrOsr = osr(mm)
      | a == addrOut = out(mm)
      | otherwise    = mem(mm) !! (fromIntegral(a) `mod` addrBound)
--(|!|) mm a = mm !! (a `mod` addrBound)

(|:=|) :: Addr -> Word -> (Memory -> Memory)
(|:=|) a w
       | a == addrOut = \ m -> m{ out=w, osr=busy }
       | otherwise    = \ m ->
                        let mem' = update (fromIntegral(a) `mod` addrBound) w $ mem(m)
                        in m{ mem = mem'}


--(|:=|) a w  = \ m -> update (a `mod` addrBound) w m

update :: Int -> a -> [a] -> [a]
update 0 w (x:r) = w:r
update a w (x:r)
    | a>0 = let r' = update (a-1) w r
            in x:r'


evalList :: [a] -> [a]
evalList []     = []
evalList (x:xs) = x:(evalList xs)


abort s = trace s undefined


type Instruction = State -> State

-- load  - assign "ac" value
loco x s = s{ ac = x }
lodd x s = s{ ac = mm(s)|!| x }
lodl x s = s{ ac = mm(s)|!|(sp(s)|+|x) }

-- store - store memory
stod x s = s{ mm=(x|:=|ac(s))$mm(s) }
stol x s = s{ mm=(sp(s)|+|x|:=|ac(s))$mm(s) }

-- add 
addd x s = s{ ac =ac(s)|+|(mm(s)|!|x) }
addl x s = s{ ac =ac(s)|+|(mm(s)|!|(sp(s)|+|x)) }

-- sub
subd x s = s{ ac =ac(s)|-|(mm(s)|!|x) }
subl x s = s{ ac =ac(s)|-|(mm(s)|!|(sp(s)|+|x)) }

-- jump
jump :: Addr -> Instruction
jump x s = s{ pc=x }
jpos x s = if not . neg . ac$s then s{ pc=x } else s
jzer x s = if ac(s)==0         then s{ pc=x } else s
jneg x s = if neg $ ac(s)      then s{ pc=x } else s
jnze x s = if ac(s)/=0         then s{ pc=x } else s
call x s = s{ pc=x
            , sp=sp'
            , mm=(sp'|:=|pc(s))$mm(s)
            }
    where
      sp' = sp(s) |-| 1
retn s = s{ sp=sp(s)|+|1
          , pc=mm(s)|!|sp(s)
          }
-- push
push s = s{ sp=sp'
          , mm=(sp'|:=|ac(s))$mm(s)
          }
    where
      sp' = sp(s) |-| 1
pshi s = s{ sp=sp'
          , mm=(sp'|:=|(mm(s)|!|ac(s))$mm(s))
          }
    where
      sp' = sp(s) |-| 1
-- pop
pop s = s{ sp=sp(s)|+|1
         , ac = mm(s)|!|sp(s)
         }
popi s = s{ sp=sp(s)|+|1
          , mm=ac(s)|:=|(mm(s)|!|sp(s))$mm(s)
          }
-- swap  - swaping ac to sp
swap s = s{ ac=sp(s)
          , sp=ac(s)
          }

insp y s = s{ sp=sp(s)|+|y }
desp y s = s{ sp=sp(s)|-|y }


--
decode :: Word -> (Bool,Instruction)
decode word
 = case mytrace (word`quot`2^12) of  -- ワードはビット列に等しい
    0  -> t (lodd x)                 -- 0000  XXXX  XXXX   XXXX   = 0x0XXX
    1  -> t (stod x)                 -- 0001  XXXX  XXXX   XXXX   = 0x1XXX
    2  -> t (addd x)                 -- 0010  XXXX  XXXX   XXXX   = 0x2XXX
    3  -> t (subd x)                 -- 0011  XXXX  XXXX   XXXX   = 0x3XXX
    4  -> t (jpos x)                 -- 0100  XXXX  XXXX   XXXX   = 0x4XXX
    5  -> t (jzer x)                 -- 0101  XXXX  XXXX   XXXX   = 0x5XXX
    6  -> t (jump x)                 -- 0110  XXXX  XXXX   XXXX   = 0x6XXX
    7  -> t (loco x)                 -- 0111  XXXX  XXXX   XXXX   = 0x7XXX
    8  -> t (lodl x)                 -- 1000  XXXX  XXXX   XXXX   = 0x8XXX
    9  -> t (stol x)                 -- 1001  XXXX  XXXX   XXXX   = 0x9XXX
    10 -> t (addl x)                 -- 1010  XXXX  XXXX   XXXX   = 0xAXXX
    11 -> t (subl x)                 -- 1011  XXXX  XXXX   XXXX   = 0xBXXX
    12 -> t (jneg x)                 -- 1100  XXXX  XXXX   XXXX   = 0xCXXX
    13 -> t (jnze x)                 -- 1101  XXXX  XXXX   XXXX   = 0xDXXX
    14 -> t (call x)                 -- 1110  XXXX  XXXX   XXXX   = 0xEXXX
    15 -> case (x`quot`2^8) of            -- 1111  ....  ....   ....   = 0xF...
           0  | y==0 -> t pshi       -- 1111  0000  0000  0000  = 0xF000
           2  | y==0 -> t popi       -- 1111  0010  0000  0000  = 0xF200
           4  | y==0 -> t push       -- 1111  0100  0000  0000  = 0xF400
           6  | y==0 -> t pop        -- 1111  0110  0000  0000  = 0xF600
           8  | y==0 -> t retn       -- 1111  1000  0000  0000  = 0xF800
           10 | y==0 -> t swap       -- 1111  1010  0000  0000  = 0xFA00
           12        -> t (insp y)   -- 1111  1100  YYYY   YYYY   = 0xFCYY
           14        -> t (desp y)   -- 1111  1110  YYYY   YYYY   = 0xFEYY
           n         -> (False,id)   -- ワードは命令では
   where
     t instru = (True,instru)
     x = word `mod` 2^12
     y = word `mod` 2^8



doIO :: State -> ([Int],State)
doIO s
     | osr(mm(s)) == busy = ( [fromIntegral(out(mm(s)))]
                            , s{mm =mm'{osr=free}}
                            )
     | otherwise          = ([],s)
    where
      mm'  = mm(s)

--                            , s{mm= s(mm)   mm{osr=free})

instructionCycle :: State -> State
instructionCycle state
    | is_instr  = instructionCycle (instr(state{pc=pc(state)|+|1}))
    | otherwise = state
    where
      (is_instr,instr) = decode(mm(state)|!|pc(state))
      (output,state')  = doIO $ instr $ state{pc=pc(state)|+|1}



boot :: [Word] -> State
boot program =
    State { pc = 0
          , ac = 0
          , sp = initSp
          , mm = store $ program
          }

initSp = 4092

--

assembler :: Assembly -> [Word]
assembler statements = words
    where
      (words,labels) = assemble 0 statements (translate labels)

assemble :: Addr -> Assembly -> (Address -> Addr) -> ([Word], String->Addr)
assemble n [] trans = ([], \ s -> abort("label " ++ show(s) ++ " is not defiend"))
--([],\s->undefined)

-- ラベルの場合
assemble n (Label l:rest) trans =
    (words, \s->if s==l then n else labs$s)
    where
      (words,labs) = assemble n rest trans

assemble n (statement:rest) trans = (word:words,labs)
    where
      (words,labs) = assemble (n+1) rest trans
      word = case trace (show statement) statement of
               Lodd a -> c1 0   (trans a)
               Stod a -> c1 1   (trans a)
               Addd a -> c1 2   (trans a)
               Subd a -> c1 3   (trans a)
               Jpos a -> c1 4   (trans a)
               Jzer a -> c1 5   (trans a)
               Jump a -> c1 6   (trans a)
               Loco c -> c1 7   (index c 12)
               Lodl n -> c1 8   (index n 12)
               Stol n -> c1 9   (index n 12)
               Addl n -> c1 10  (index n 12)
               Subl n -> c1 11  (index n 12)
               Jneg a -> c1 12  (trans a)
               Jnze a -> c1 13  (trans a)
               Call a -> c1 14  (trans a)
               Pshi   -> c2 0   0 
               Popi   -> c2 2   0
               Push   -> c2 4   0
               Pop    -> c2 6   0
               Retn   -> c2 8   0
               Swap   -> c2 10  0
               Insp i -> c2 12  (index i 8)
               Desp i -> c2 14  (index i 8)
               Stop   -> c2 0   1           -- ある無効なオペコード
               Const n
                   | 0<= fromIntegral n && fromIntegral n <wordBound -> fromIntegral n
                   | -2^15< fromIntegral n && fromIntegral n <0 -> fromIntegral (fromIntegral (n) + wordBound)
                   | otherwise    -> abort ("Bad Const "++ show n)

      c1 opcode x = opcode * 2^12 + x
      c2 opcode y = 15*2^12 + opcode*2^8 + y
      index :: Int -> Int -> Word
      index n p
            | 0<=n && n<2^p = fromIntegral n
            | otherwise     = 0

isAddr n = (0<=n && n<addrBound)

translate :: (String->Addr) -> Address -> Addr
translate addrs (C n)
    | isAddr (fromIntegral n)  = n
    | otherwise = abort ("Constant" ++ show n ++ "no an address.")
translate addrs (L s)
    | isAddr (fromIntegral n)  = n
    | otherwise = abort ("Label " ++ s ++ " out of range.")
    where
      n = trace (show . addrs$s) addrs$s


-- sample code
fib_program n
    = [      Label "start"
      ,      Loco  n            --  0:引数をアキュムレータにロード
      ,      Call  (L "fib")    --  1:フィボナッチ関数の呼出
      ,      Stod  (C addrOut) --  2:結果の印刷
      ,      Stop               --  3:実行の停止
      ,      Label "fib"        --    フィボナッチ関数
      ,      Subd  (L "two")    --  4:n-2の計算
      ,      Jneg  (L "neg")    --  5:n-2<0ならば、negに行く
      ,      Push               --  6:n-2のプッシュ
      ,      Addd  (L "one")    --  7:n-1の計算
      ,      Call  (L "fib")    --  8:fib (n-1)の計算
      ,      Push               --  9:fib (n-1)のプッシュ
      ,      Lodl  1            -- 10:n-2をアキュムレータにロード
      ,      Call  (L "fib")    -- 11:fib (n-2)の計算
      ,      Addl  0            -- 12:fib (n-2)をfib (n-1)に加える
      ,      Insp  2            -- 13:スタックのクリア
      ,      Retn               -- 14:呼出側に戻る

      ,      Label "neg"        --    n-2<0を処理する
      ,      Loco  1            -- 15:結果をアキュムレータにロード
      ,      Retn               -- 16:呼出側に戻る

      ,      Label "one"
      ,      Const 1            -- 17:定数1

      ,      Label "two"
      ,      Const 2            -- 18:定数2
   ]


--traceCycle :: State -> [(String, String, String, String)]
traceCycle s
    | is_instr  = (mytrace trace_cont): traceCycle stat'
    | otherwise = []
    where
      (is_instr,instr) = decode (mm(s)|!|pc(s))
      (output,stat')   = doIO (instr s{pc=pc(s)|+|1})
      trace_cont       = ( "pc = "    ++ (show $ pc(s))
                         , "ac = "    ++ (show $ ac(s))
                         , "sp = "    ++ (show $ sp(s))
                         , "stack = " ++ (show $ [mm(s)|!|w | w<-[sp(s) .. initSp-1]])
                         , "output =" ++ (show output)
                         )

mytrace x = trace (show x) x
-- traceCycle (boot (assembler (fib_program 10)))
