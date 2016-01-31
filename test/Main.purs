module Test.Main where

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Prelude hiding

import Palto

expr :: forall repr. (Expr repr) =>
    repr Int
expr = pint 90

expr' :: forall repr. (Mult repr, Expr repr) =>
    repr Boolean
expr' = pint 90 `pcompare` (pint 3 `pmul` (pint 10 `padd` pint 20))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    log $ show $ (runStringify expr')
    log $ show $ (runEval expr') -- == false
