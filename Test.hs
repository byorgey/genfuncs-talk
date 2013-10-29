{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

import Diagrams.Prelude
import Diagrams.Backend.Postscript.CmdLine
import Structures
import Control.Lens ((&), (.~), (^.))

foo :: BucketOpts -> [Bucket] -> [Bucket] -> Dia
foo opts s1 s2 = row === col # translateX (negate (opts ^. bucketSize))
  where
    col = drawSpecies (opts & bucketDir .~ unit_Y) s1
    row = drawSpecies (opts & flipIndices .~ True) s2

prodBucket :: Bucket
prodBucket = (speciesA !! 4) %* (speciesB !! 4)

dia =
  (vcat' with {sep=3} . map alignR)
  [ hcat' with {sep=3} [text' 8 "0", drawSpecies (with & showIndices .~ False) (repeat zero) ]
  , hcat' with {sep=3} [text' 8 "0(x)", drawGF (repeat 0)]
  ]

main :: IO ()
main = defaultMain $ dia

