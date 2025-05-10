module Main (main) where

import Test.Hspec
import Interpreter
import Types
import Data.List (intersperse)

main :: IO ()
main = hspec $ do
    describe "official tests for non-error programs" $ do
        {-- literals -}
        t "3"                           "3"
        t "121231324135634563456363567" "121231324135634563456363567"
        t "1.0"                         "1.0"
        t "0.0"                         "0.0"
        t "-1"                          "-1"
        t "-1.1"                        "-1.1"
        t "False"                       "False"
        t "True"                        "True"
        t "[ [ ] [ ] ]"                 "[[],[]]"
        t "[ False [ ] True [ 1 2 ] ]"  "[False,[],True,[1,2]]"
        t "\" [ so { not if ] and } \"" "\"[ so { not if ] and }\""
        {-- quotation literals -}
        t "{ 20 10 + }"             "{ 20 10 + }"
        t "[ { + } { 10 + } { 20 10 + } ]"   "[{ + },{ 10 + },{ 20 10 + }]"
        
        {-- simple arithmetic -}
        t "1 1 +"               "2"       
        t "10 20 *"             "200"
        t "20 2 div"            "10"
        t "20 2 /"              "10.0"
        
        {-- arithmetic with type coercion -}
        t "1 1.0 +"             "2.0"       
        t "10 20.0 *"           "200.0"
        t "20 2.0 div"          "10"
        t "20.0 2.0 div"        "10"
        
        {-- bool operations -}
        t "False False &&"      "False"
        t "False True ||"       "True"
        t "False not"           "True"
        t "True not"            "False"
        
        {-- comparisons -}
        t "20 10 <"             "False"
        t "20 10 >"             "True"
        t "20 10.0 >"           "True"
        t "20.0 20.0 >"         "False"
        t "10 10 =="            "True"
        t "10 10.0 =="          "True"
        t "True True =="        "True"
        t "True 40 40 == =="    "True"
        t "\" abba \" \" abba \" ==" "True"
        t "[ ] [ ] =="          "True"
        t "[ 1 2 ] [ 1 2 ] =="  "True"
        t " [ [ ] ] [ [ ] ] ==" "True"
        
        {-- stack operations -}
        t "10 20 swap pop"          "20"
        t "10 dup dup + swap pop"   "20"
        t "10 20 swap dup + div"    "1"
        
        {-- length -}
        t "\" hello \" length"              "5"
        t "\" hello world \" length"        "11"
        t "[ 1 2 3 [ ] ] length"            "4"
        t "{ 10 20 + } length"              "3"

        {-- String parsing -}
        t "\" 12 \" parseInteger"           "12"
        t "\" 12.34 \" parseFloat"          "12.34"
        t "\" adam bob charlie \" words"    "[\"adam\",\"bob\",\"charlie\"]"          
        
        {-- lists -}
        t "[ 1 2 3 ]"           "[1,2,3]"
        t "[ 1 \" bob \" ]"     "[1,\"bob\"]"
        t "[ 1 2 ] empty"       "False"
        t "[ ] empty"           "True"
        t "[ 1 2 3 ] head"      "1"
        t "[ 1 2 3 ] length"    "3"
        t "[ 1 2 3 ] tail"      "[2,3]"
        t "1 [ ] cons"          "[1]"
        t "1 [ 2 3 ] cons"      "[1,2,3]"
        t "[ 1 ] [ 2 3 ] append" "[1,2,3]"
        t "[ 1 2 ] [ ] append"  "[1,2]"
        t "[ 1 ] [ 2 3 ] cons"  "[[1],2,3]"

        {-- assignments -}
        t "age"                             "age"
        t "age 10 := age"                   "10"
        t "10 age swap := age"              "10"
        t "[ 1 2 3 ] list swap := list"     "[1,2,3]"
        t "age 20 := [ 10 age ]"            "[10,20]"

        t "inc { 1 + } fun 1 inc"           "2"
        t "mul10 { 10 * } fun inc { 1 + } fun 10 inc mul10" "110"
        
        {-- quotations -}
        t "{ 20 10 + } exec"                "30"
        t "10 { 20 + } exec"                "30"
        t "10 20 { + } exec"                "30"
        t "{ { 10 20 + } exec } exec"       "30"
        t "{ { 10 20 + } exec 20 + } exec"  "50"

-- Test helper
t :: String -> String -> SpecWith ()
t input expected = it ("evaluates `" ++ input ++ "` to " ++ expected) $ do
    case bprogList input of
        Left parseErr -> expectationFailure $ "Parse error: " ++ show parseErr
        Right parsed -> 
            case bprogParse parsed ([], []) of
                Left evalErr -> expectationFailure $ "Evaluation error: " ++ show evalErr
                Right (_, stack) -> formatResult stack `shouldBe` expected

-- Format stack as string (mirrors Format.hs)
formatResult :: Stack -> String
formatResult [x] = formatType x
formatResult xs  = "[" ++ commaSep (map formatType xs) ++ "]"

formatType :: Types -> String
formatType (VInt n)       = show n
formatType (VFloat d)     = show d
formatType (VBool b)      = show b
formatType (VString s)    = "\"" ++ s ++ "\""
formatType (VList xs)     = "[" ++ commaSep (map formatType xs) ++ "]"
formatType (VBlock xs)    = "{ " ++ unwords (map formatType xs) ++ " }"
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
formatType (VVarib var) = var
formatType _ = "<unsupported>"

-- Join strings with commas
commaSep :: [String] -> String
commaSep = concat . intersperse ","