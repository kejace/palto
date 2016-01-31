module Test.Main where

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Prelude hiding (add, mul, compare)

import Palto

expr :: forall repr. (Expr repr) =>
    repr Int
expr = int 90

expr' :: forall repr. (Mult repr, Expr repr) =>
    repr Boolean
expr' = int 90 `compare` (int 3 `mul` (int 10 `add` int 20))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    log $ show $ (runStringify expr')
    log $ show $ (runEval expr') -- == false
