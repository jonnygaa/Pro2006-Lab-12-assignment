module Format
    ( formatResult,
      formatType
    ) where

import Data.List (intersperse)
import Types

-- Pretty print the entire stack
prettyPrintStack :: Stack -> String
prettyPrintStack = formatResult

-- Format stack contents
formatResult :: Stack -> String
formatResult [x] = formatType x  -- Single value
formatResult xs  = "[" ++ commaSep (map formatType xs) ++ "]"  -- Multiple values

-- Convert a Types value to its string representation
formatType :: Types -> String
formatType (VInt n)       = show n
formatType (VFloat d)     = show d
formatType (VBool b)      = show b
formatType (VString s)    = "\"" ++ s ++ "\""  -- Strings are quoted
formatType (VList xs)     = "[" ++ commaSep (map formatType xs) ++ "]"  -- Lists
formatType (VBlock xs)    = "{ " ++ unwords (map formatType xs) ++ " }"  -- Code blocks
-- Operations and variables:
formatType (VArithOp Add) = "+"
formatType (VArithOp Sub) = "-"
formatType (VArithOp Mul) = "*"
formatType (VArithOp FltDiv) = "/"
formatType (VArithOp IntDiv) = "div"
formatType (VBoolOp St) = "<"
formatType (VBoolOp Gt) = ">"
formatType (VBoolOp Eq) = "=="
formatType (VBoolOp And) = "&&"
formatType (VBoolOp Or) = "||"
formatType (VNotOp Not) = "not"
formatType (VStackOp Dup) = "dup"
formatType (VStackOp Swap) = "swap"
formatType (VStackOp Pop) = "pop"
formatType (VStringOp ParInt) = "parseInteger"
formatType (VStringOp ParFlt) = "parseFloat"
formatType (VStringOp Words) = "words"
formatType (VListOp Head) = "head"
formatType (VListOp Tail) = "tail"
formatType (VListOp Empty) = "empty"
formatType (VListOp Length) = "length"
formatType (VListOp Cons) = "cons"
formatType (VListOp Append) = "append"
formatType (VBlockOp Exec) = "exec"
formatType (VVarib var) = var  -- Variable names as-is
formatType _ = "<unsupported>"  -- Fallback

-- Join strings with commas
commaSep :: [String] -> String
commaSep = concat . intersperse ","