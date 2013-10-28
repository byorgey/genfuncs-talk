{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Postscript.CmdLine
import Structures

main = defaultMain (drawSpecies with speciesB)
