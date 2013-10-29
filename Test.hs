{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

import Diagrams.Prelude
import Diagrams.Backend.Postscript.CmdLine
import Structures
import Control.Lens ((&), (.~), (^.))

dia = prodSum' (with & showX .~ True) 3 gf1 gf2

main :: IO ()
main = defaultMain $ dia

