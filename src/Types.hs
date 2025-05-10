module Types where

-- Arithmetic operations
data ArithOp = Add | Sub | Mul | FltDiv | IntDiv
  deriving (Show, Eq, Ord)

-- Boolean/comparison operations
data BoolOp = St | Gt | Eq | And | Or
  deriving (Show, Eq, Ord)

-- Not operation
data NotOp = Not
  deriving (Show, Eq, Ord)

-- Stack operations
data StackOp = Dup | Swap | Pop
  deriving (Show, Eq, Ord)

-- String operations
data StringOp = ParInt  | ParFlt | Words
  deriving (Show, Eq, Ord)

-- List operations
data ListOp = Head | Tail | Empty | Length | Cons | Append | Each | Map | Foldl
  deriving (Show, Eq, Ord)

-- Block operation
data BlockOp = Exec
  deriving (Show, Eq, Ord)

-- Value types in the language
data Types =
      VInt Integer          -- Integer
    | VFloat Double         -- Float
    | VBool Bool            -- Boolean
    | VArithOp ArithOp      -- Arithmetic operation
    | VBoolOp BoolOp        -- Boolean operation
    | VNotOp NotOp          -- Not operation
    | VStackOp StackOp      -- Stack operation
    | VStringOp StringOp    -- String operation
    | VListOp ListOp        -- List operation
    | VBlockOp BlockOp      -- Block operation
    | VString String        -- String
    | VList [Types]         -- List
    | VBlock [Types]        -- Code block
    | VVarib String         -- Variable
    | VAssign               -- Assignment
    | VFun                  -- Function definition
    deriving (Show, Eq, Ord)

-- The stack is a list of Types
type Stack = [Types]

-- Environment maps variable names to values
type Env = [(String, Types)]

-- Evaluation state: environment + stack
type EvalState = (Env, Stack)