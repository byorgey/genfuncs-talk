{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

import           Structures

theTree = tree (parseTree "((())()((()()())()(()())))") # centerXY

theGraph = graph [(0,1), (0,2), (1,3), (2,3), (2,4), (2,5), (3,5), (3,6), (6,7), (6,8)] # centerXY

theList = list 5 # centerXY

theCycles = hcat' with {sep = 2} [cyc 5, cyc 7] # centerXY # rotateBy (1/20)

dia
  = hcat' with {sep = 10}
    [ vcat' with {sep = 10} [theGraph, theList] # centerXY
    , vcat' with {sep = 5} [theCycles, theTree] # centerXY
    ]


main = defaultMain (dia # centerXY # pad 1.1 # sized (Width 4))
