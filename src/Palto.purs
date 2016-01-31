module Palto 
    ( Eval(..)
    , Stringify(..)
    , Expr
    , Mult
    , runStringify
    , runEval
    , pint
    , pboolean
    , padd
    , pcompare
    , pmul
    ) where

import Prelude hiding

data Eval ret = Eval ret

runEval :: forall a. Eval a -> a
runEval (Eval ret) = ret

data Stringify ret = Stringify (String)

runStringify :: forall a. (Show a) => Stringify a -> String
runStringify (Stringify ret) = ret

class Expr repr where
    pint :: Int -> repr Int
    pboolean :: Boolean -> repr Boolean
    padd :: repr Int -> repr Int -> repr Int
    pcompare :: forall a. (Eq a) =>
               repr a -> repr a -> repr Boolean

instance exprEval :: Expr Eval where
    pint = Eval
    pboolean = Eval
    padd (Eval l) (Eval r) = Eval $ l + r
    pcompare (Eval l) (Eval r) = Eval $ l == r

instance exprStringify :: Expr Stringify where
    pint = Stringify <<< show
    pboolean = Stringify <<< show
    padd (Stringify l) (Stringify r) =
        Stringify $ "(" ++ l ++ " + " ++ r ++ ")"
    pcompare (Stringify l) (Stringify r) = 
        Stringify $ "(" ++ l ++ " == " ++ r ++ ")"

class Mult repr where
    pmul :: repr Int -> repr Int -> repr Int

instance multEval :: Mult Eval where
    pmul (Eval l) (Eval r) = Eval $ l * r

instance multStringify :: Mult Stringify where
    pmul (Stringify l) (Stringify r) = 
        Stringify $ "(" ++ l ++ " * " ++ r ++ ")"