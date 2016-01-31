module Palto (Eval(..), Stringify(..), Expr, Mult, runStringify, runEval, int, boolean, add, compare, mul)

	where

import Prelude hiding (add, mul, compare)

data Eval ret = Eval ret

runEval :: forall a. Eval a -> a
runEval (Eval ret) = ret

data Stringify ret = Stringify (String)

runStringify :: forall a. (Show a) => Stringify a -> String
runStringify (Stringify ret) = ret

class Expr repr where
    int :: Int -> repr Int
    boolean :: Boolean -> repr Boolean
    add :: repr Int -> repr Int -> repr Int
    compare :: forall a. (Eq a) =>
               repr a -> repr a -> repr Boolean

instance exprEval :: Expr Eval where
    int = Eval
    boolean = Eval
    add (Eval l) (Eval r) = Eval $ l + r
    compare (Eval l) (Eval r) = Eval $ l == r

instance exprStringify :: Expr Stringify where
    int = Stringify <<< show
    boolean = Stringify <<< show
    add (Stringify l) (Stringify r) =
        Stringify $ "(" ++ l ++ " + " ++ r ++ ")"
    compare (Stringify l) (Stringify r) = 
        Stringify $ "(" ++ l ++ " == " ++ r ++ ")"

class Mult repr where
    mul :: repr Int -> repr Int -> repr Int

instance multEval :: Mult Eval where
    mul (Eval l) (Eval r) = Eval $ l * r

instance multStringify :: Mult Stringify where
    mul (Stringify l) (Stringify r) = 
        Stringify $ "(" ++ l ++ " * " ++ r ++ ")"