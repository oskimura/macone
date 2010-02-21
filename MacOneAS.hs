module MacOneAS
    ( Word
    , Addr
    , AStatement(..)
    , Address(..)
    , Assembly

    )
    where
import Data.Word

--type Word = Int
type Addr = Word


data AStatement
    = Lodd Address | Stod Address | Addd Address | Subd Address | Call Address
    | Jpos Address | Jzer Address | Jump Address | Jneg Address | Jnze Address
    | Loco Int     | Lodl Int     | Stol Int     | Addl Int     | Subl Int
    | Pshi         | Popi         | Push         | Pop          | Retn
    | Swap         | Insp Int     | Desp Int
    | Const Int    | Stop         | Label String deriving Show


data Address  = L String | C Addr deriving Show
type Assembly = [AStatement]
