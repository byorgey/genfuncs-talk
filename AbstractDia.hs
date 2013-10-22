{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

import           Structures

dia
  = hcat' with {sep = 10}
    [ vcat' with {sep = 10} [theGraph, theList] # centerXY
    , vcat' with {sep = 5} [theCycles, theTree] # centerXY
    ]


main = defaultMain (dia # centerXY # pad 1.1 # sized (Width 4))
