module Interpreter
    ( bprogList,
      bprogParse
    ) where

import Text.Read (readMaybe)
import Data.List (isSuffixOf)
import Data.Char (isAlpha)
import Types
import Format (formatResult, formatType)
import ErrorHandeling

-- Parse a string into a Stack of Types
bprogList :: String -> Either ParserError Stack
bprogList input = process (words input) [] False []
  where
    -- Base case: return final stack
    process [] stack _ _ = Right stack
    
    -- String literal processing
    process (t:ts) stack True strTokens
        | "\"" `isSuffixOf` t =  -- End of string
            let fullString = trim (unwords (strTokens ++ [stripEndQuote t]))
            in process ts (stack ++ [VString fullString]) False []
        | otherwise = process ts stack True (strTokens ++ [t])  -- Accumulate string tokens
    
    -- Token processing
    process (t:ts) stack _ _ 
        | t == "\"" = process ts stack True []  -- Start of string
        | t == "["  = do  -- Start of list
            let (listTokens, rest) = extractList ts 0
            parsedList <- bprogList (unwords listTokens)
            process rest (stack ++ [VList parsedList]) False []
        | t == "{"  = do  -- Start of code block
            let (blockTokens, rest) = extractBlock ts 0
            parsedBlock <- bprogList (unwords blockTokens)
            process rest (stack ++ [VBlock parsedBlock]) False []
        | otherwise = do  -- Normal token
            newStack <- step1 t stack
            process ts newStack False []

    -- Helper functions
    trim = f . f where f = reverse . dropWhile (== ' ')  -- Trim whitespace
    stripEndQuote s = if not (null s) && last s == '"' then init s else s  -- Remove trailing quote

    -- Extract list tokens between [ and ]
    extractList [] _ = ([], [])
    extractList (t:ts) depth
        | t == "[" = let (inner, rest) = extractList ts (depth + 1) in (t : inner, rest)
        | t == "]" = if depth == 0 then ([], ts) else let (inner, rest) = extractList ts (depth - 1) in (t : inner, rest)
        | otherwise = let (inner, rest) = extractList ts depth in (t : inner, rest)

    -- Extract block tokens between { and }
    extractBlock [] _ = ([], [])
    extractBlock (t:ts) depth
        | t == "{" = let (inner, rest) = extractBlock ts (depth + 1) in (t : inner, rest)
        | t == "}" =
            if depth == 0 then ([], ts)
            else let (inner, rest) = extractBlock ts (depth - 1) in (t : inner, rest)
       | otherwise = let (inner, rest) = extractBlock ts depth in (t : inner, rest)

-- Convert a token to a Types value and add to stack
step1 :: String -> Stack -> Either ParserError Stack
step1 token stack = case token of
    -- Arithmetic operations
    "+"     -> Right $ stack ++ [VArithOp Add]
    "-"     -> Right $ stack ++ [VArithOp Sub]
    "*"     -> Right $ stack ++ [VArithOp Mul]
    "/"     -> Right $ stack ++ [VArithOp FltDiv]
    "div"   -> Right $ stack ++ [VArithOp IntDiv]
    -- Boolean operations
    "<"     -> Right $ stack ++ [VBoolOp St]
    ">"     -> Right $ stack ++ [VBoolOp Gt]
    "=="    -> Right $ stack ++ [VBoolOp Eq]
    "&&"    -> Right $ stack ++ [VBoolOp And]
    "||"    -> Right $ stack ++ [VBoolOp Or]
    "not"   -> Right $ stack ++ [VNotOp Not]
    -- Stack operations
    "dup"   -> Right $ stack ++ [VStackOp Dup]
    "swap"  -> Right $ stack ++ [VStackOp Swap]
    "pop"   -> Right $ stack ++ [VStackOp Pop]
    -- String operations
    "parseInteger" -> Right $ stack ++ [VStringOp ParInt]
    "parseFloat"   -> Right $ stack ++ [VStringOp ParFlt]
    "words"        -> Right $ stack ++ [VStringOp Words]
    -- List operations
    "head"   -> Right $ stack ++ [VListOp Head]
    "tail"   -> Right $ stack ++ [VListOp Tail]
    "empty"  -> Right $ stack ++ [VListOp Empty]
    "length" -> Right $ stack ++ [VListOp Length]
    "cons"   -> Right $ stack ++ [VListOp Cons]
    "append" -> Right $ stack ++ [VListOp Append]
    -- Block operation
    "exec"   -> Right $ stack ++ [VBlockOp Exec]
    -- Assignment and function definition
    ":="     -> Right $ stack ++ [VAssign]
    "fun"    -> Right $ stack ++ [VFun]
    -- Literals (integers, floats, booleans, variables)
    _ -> case readMaybe token :: Maybe Integer of
        Just num -> Right $ stack ++ [VInt num]
        Nothing  -> case readMaybe token :: Maybe Double of
            Just num -> Right $ stack ++ [VFloat num]
            Nothing  -> case readMaybe token :: Maybe Bool of
                Just bol -> Right $ stack ++ [VBool bol]
                Nothing  -> Right $ stack ++ [VVarib token]

-- Evaluate a Stack of Types
bprogParse :: [Types] -> EvalState -> Either ProgramError EvalState
bprogParse input initialState = process input initialState

-- Process each token in the stack
process :: [Types] -> EvalState -> Either ProgramError EvalState
process [] state = Right state  -- Base case
process (token:tokens) (env, stack) = do
  newState <- step2 token (env, stack)  -- Evaluate current token
  process tokens newState  -- Process remaining tokens

-- Arithmetic functions
arithFunc :: ArithOp -> (Double -> Double -> Double)
arithFunc Add = (+)
arithFunc Sub = (-)
arithFunc Mul = (*)
arithFunc FltDiv = (/)
arithFunc IntDiv = (/)

-- Integer arithmetic functions
intArithFunc :: ArithOp -> (Integer -> Integer -> Integer)
intArithFunc Add = (+)
intArithFunc Sub = (-)
intArithFunc Mul = (*)
intArithFunc FltDiv = div
intArithFunc IntDiv = div

-- Evaluate a single Types value
step2 :: Types -> EvalState -> Either ProgramError EvalState
step2 token (env, stack) =
  case token of
    -- Arithmetic operation
    VArithOp op -> 
      case applyOp op stack of
        Right newStack -> Right (env, newStack)
        Left err -> Left err
    -- Boolean operation
    VBoolOp op -> 
      case applyComp op stack of
        Right newStack -> Right (env, newStack)
        Left err -> Left err
    -- Not operation
    VNotOp _ -> 
      case applyNot stack of
        Right newStack -> Right (env, newStack)
        Left err -> Left err
    -- Stack operation
    VStackOp op -> 
      case applyStackOp op stack of
        Right newStack -> Right (env, newStack)
        Left err -> Left err
    -- String operation
    VStringOp op -> 
      case applyStringOp op stack of
        Right newStack -> Right (env, newStack)
        Left err -> Left err
    -- List operation
    VListOp op -> 
      case applyListOp op stack of
        Right newStack -> Right (env, newStack)
        Left err -> Left err
    -- Execute code block
    VBlockOp Exec ->
      case stack of
        (VBlock block:rest) -> 
          case bprogParse block (env, rest) of
            Right (_, result) -> Right (env, result)
            Left err -> Left err
        _ -> Left ExpectedQuotation
    -- Variable assignment
    VAssign ->
      case stack of
        (value:VVarib name:rest) -> 
          let newEnv = (name, resolveValue env value):env
          in Right (newEnv, rest)
        _ -> Left ExpectedVariable
    -- Function definition
    VFun ->
      case stack of
        (VBlock block:VVarib name:rest) -> 
          Right ((name, VBlock block):env, rest)
        _ -> Left ExpectedQuotation
    -- Variable lookup
    VVarib name ->
      case lookup name env of
        Just (VBlock block) ->  -- Function call
          case bprogParse block (env, stack) of
            Right (_, result) -> Right (env, result)
            Left err -> Left err
        Just val -> Right (env, val:stack)  -- Variable value
        Nothing -> Right (env, VVarib name:stack)  -- Unbound variable
    -- List evaluation
    VList xs -> Right (env, VList (map (resolveValue env) xs):stack)
    -- Default: push to stack
    _ -> Right (env, token:stack)

-- Apply a binary operation (+, -, *, /)
applyOp :: ArithOp -> Stack -> Either ProgramError Stack
applyOp op stack =
  case stack of
    (x:y:rest) -> case (x, y) of
      -- Float operations
      (VFloat x, VFloat y) -> case op of
        IntDiv -> Right $ VInt (truncate y `div` truncate x) : rest
        _      -> Right $ VFloat (arithFunc op y x) : rest
      -- Mixed float/int
      (VFloat x, VInt y)   -> case op of
        FltDiv -> Right $ VFloat (fromIntegral y / x) : rest
        IntDiv -> Right $ VInt (y `div` truncate x) : rest
        _      -> Right $ VFloat (arithFunc op (fromIntegral y) x) : rest
      (VInt x, VFloat y)   -> case op of
        FltDiv -> Right $ VFloat (y / fromIntegral x) : rest
        IntDiv -> Right $ VInt (truncate y `div` x) : rest
        _      -> Right $ VFloat (arithFunc op y (fromIntegral x)) : rest
      -- Integer operations
      (VInt x, VInt y)     -> case op of
        FltDiv -> Right $ VFloat (fromIntegral y / fromIntegral x) : rest
        _      -> Right $ VInt (intArithFunc op y x) : rest
      _ -> Left UnknownSymbol
    _ -> Left StackEmpty

-- Apply a bool operation (<, >, ==, &&, ||)
applyComp :: BoolOp -> Stack -> Either ProgramError Stack
applyComp op stack = 
  case stack of
    (x:y:rest) -> case op of
      St -> if lt y x then Right $ VBool True : rest else Right $ VBool False : rest
      Gt -> if gt y x then Right $ VBool True : rest else Right $ VBool False : rest
      Eq -> if eq y x then Right $ VBool True : rest else Right $ VBool False : rest
      And -> case (x, y) of
        (VBool a, VBool b) -> Right $ VBool (b && a) : rest
        _ -> Left UnknownSymbol
      Or -> case (x, y) of
        (VBool a, VBool b) -> Right $ VBool (b || a) : rest
        _ -> Left UnknownSymbol
      where
        lt (VInt a) (VInt b) = a < b
        lt (VFloat a) (VFloat b) = a < b
        lt (VInt a) (VFloat b) = fromIntegral a < b
        lt (VFloat a) (VInt b) = a < fromIntegral b
        lt _ _ = False

        gt (VInt a) (VInt b) = a > b
        gt (VFloat a) (VFloat b) = a > b
        gt (VInt a) (VFloat b) = fromIntegral a > b
        gt (VFloat a) (VInt b) = a > fromIntegral b
        gt _ _ = False

        eq (VInt a) (VInt b) = a == b
        eq (VFloat a) (VFloat b) = a == b
        eq (VInt a) (VFloat b) = fromIntegral a == b
        eq (VFloat a) (VInt b) = a == fromIntegral b
        eq (VBool a) (VBool b) = a == b
        eq (VString a) (VString b) = a == b
        eq (VList a) (VList b) = a == b
        eq _ _ = False
    _ -> Left StackEmpty

-- Apply a not operation (not)
applyNot :: Stack -> Either ProgramError Stack
applyNot stack = 
  case stack of
    (x:rest) -> case x of
      VBool x -> Right $ VBool (not x) : rest
      VInt x -> Right $ VInt ((-1) * x) : rest
      VFloat x -> Right $ VFloat ((-1) * x) : rest
      _ -> Left UnknownSymbol
    _ -> Left StackEmpty

-- Apply stack operation (dup, swap, pop)
applyStackOp :: StackOp -> Stack -> Either ProgramError Stack
applyStackOp op stack =
  case op of
    Dup  -> case stack of
      (x:xs) -> Right $ x : x : xs
      _      -> Left StackEmpty
    Swap -> case stack of
      (x:y:xs) -> Right $ y : x : xs
      _        -> Left StackEmpty
    Pop  -> case stack of
      (_:xs) -> Right xs
      _      -> Left StackEmpty

-- Apply string operation (parseInteger, parseFloat, words)
applyStringOp :: StringOp -> Stack -> Either ProgramError Stack
applyStringOp op stack = 
  case stack of
    (x:rest) -> case x of
      VString x -> case op of
        ParInt -> Right $ VInt (read x :: Integer) : rest
        ParFlt -> Right $ VFloat (read x :: Double) : rest
        Words  -> Right $ VList (map VString (words x)) : rest
      _ -> Left UnknownSymbol
    _ -> Left StackEmpty

-- Apply a list/block operation (Head, Tail, Empty, Length, Cons, Append, Each, Map, Foldl)
applyListOp :: ListOp -> Stack -> Either ProgramError Stack
applyListOp op stack =
  case (op, stack) of
    (Head, VList (x:_):rest) -> Right (x:rest)
    (Tail, VList (_:xs):rest) -> Right (VList xs:rest)
    (Empty, VList x:rest) -> Right (VBool (null x):rest)
    (Length, VList x:rest) -> Right (VInt (fromIntegral $ length x):rest)
    (Length, VString x:rest) -> Right (VInt (fromIntegral $ length x):rest)
    (Length, VBlock x:rest) -> Right (VInt (fromIntegral $ length x):rest)
    (Cons, VList ys:VList xs:rest) -> Right (VList (VList xs:ys) : rest)
    (Cons, VList xs:x:rest) -> Right (VList (x:xs) : rest)
    (Append, VList x:VList y:rest) -> Right (VList (y ++ x):rest)
    _ -> Left UnknownSymbol

-- Resolve variable values in the environment
resolveValue :: Env -> Types -> Types
resolveValue env (VVarib name) = 
  case lookup name env of
    Just val -> val
    Nothing -> VVarib name
resolveValue env (VList xs) = VList (map (resolveValue env) xs)
resolveValue _ val = val

-- Executes a codeblock (exec)
applyBlockOp :: Env -> Stack -> Either ProgramError Stack
applyBlockOp env (VBlock x : rest) = do
  (_, resultStack) <- process x (env, rest)
  return resultStack
applyBlockOp _ _ = Left ProgramFinishedWithMultipleValues