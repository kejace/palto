module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Prelude hiding

import Palto

expr :: forall repr. Expr repr 
     => repr Number
expr = pint 90.0

expr' :: forall repr. Mult repr
      => Expr repr
      => repr Boolean
expr' = pint 90.0 `pcompare` (pint 3.0 `pmul` (pint 10.0 `padd` pint 20.0))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    log $ show $ (runStringify expr')
    log $ show $ (runEval expr') -- == false
