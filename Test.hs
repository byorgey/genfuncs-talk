{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}

import           Control.Lens                 ((&), (.~), (^.))
import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude
import           Structures

dia = prodSum' (with & showX .~ True) 3 gf1 gf2

main :: IO ()
main = defaultMain $ dia

