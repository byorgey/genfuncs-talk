{-# LANGUAGE NoMonomorphismRestriction #-}

module Structures where

import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree

import           Control.Arrow             (second)
import           Control.Lens              ((^.))
import           Data.List                 (nub)
import qualified Data.Map                  as M
import           Data.Tree
import           Physics.ForceLayout
import           Text.Parsec
import           Text.Parsec.String

type DC = Diagram SVG R2

dot :: DC
dot = circle 1 # fc blue

list :: Int -> DC
list n = dots <> rule
  where
    dots = hcat' with {sep = 2} (replicate n dot)
    rule = hrule (width dots) # translateXTo dots

translateXTo ref mv = alignL mv # maybe id translateX (fst <$> extentX ref)

tree :: Tree () -> DC
tree =
  renderTree (const dot) (~~) . symmLayout' with { slHSep = 4, slVSep = 7 }

treeParser :: Parser (Tree ())
treeParser = Node () <$> between (char '(') (char ')') (many treeParser)

parseTree :: String -> Tree ()
parseTree s = case runParser treeParser () "" s of
                Left _ -> error "parse error"
                Right t -> t

graph :: [(Int,Int)] -> DC
graph es = drawEnsemble es $
           forceLayout (FLOpts { damping     = 0.8
                               , energyLimit = Just 0.001
                               , stepLimit   = Nothing
                               }
                       )
           ens
  where
    ens :: Ensemble R2
    ens = Ensemble [ (es,  hookeForce 0.05 4)
                   , (allPairs, coulombForce 1)
                   ]
                   particleMap
    vs = nub (map fst es ++ map snd es)
    allPairs = [(x,y) | x <- vs, y <- vs, x < y ]
    particleMap :: M.Map Int (Particle R2)
    particleMap = M.fromList $ zip vs (map initParticle (regPoly (length vs) 4))

drawEnsemble :: [(Int,Int)] -> Ensemble R2 -> DC
drawEnsemble es = applyAll (map drawEdge es) . mconcat . map drawPt . (map . second) (^.pos) . M.assocs . (^.particles)
  where
    drawPt (pid, p) = dot # named pid # moveTo p
    drawEdge (v1,v2) = withNames [v1,v2] $ \[s1,s2] -> beneath (location s1 ~~ location s2)

cyc :: Int -> DC
cyc n
  = mconcat
    [ position (zip (polygon with { polyType = PolyRegular n r }) (repeat dot))
    , circle r
    ]
  where
    r = 4* fromIntegral n / tau

data BucketOpts
  = BucketOpts
  { numBuckets    :: Int
  , showEllipses  :: Bool
  , bucketSize    :: Double
  , expandBuckets :: Bool
  }

instance Default BucketOpts where
  def = BucketOpts
    { numBuckets        = 8
    , showEllipses      = True
    , bucketSize        = 1
    , expandBuckets     = False
    }

bucketed' :: BucketOpts -> [[DC]] -> DC
bucketed' opts buckets = undefined

bucketed :: [[DC]] -> DC
bucketed = bucketed' def