{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Structures where

import           Diagrams.Backend.Postscript
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree
import           Graphics.SVGFonts

import           Control.Arrow               (second)
import           Control.Lens                (makeLenses, (^.))
import           Data.Default.Class
import           Data.List                   (genericLength, mapAccumL, nub)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           Data.Tree
import           Physics.ForceLayout
import           Text.Parsec                 (between, char, many, runParser)
import           Text.Parsec.String          (Parser)

type DC = Diagram Postscript R2

dot :: DC
dot = circle 1 # fc blue # lw 0

list :: Int -> DC
list 0 = square 1 # fc black
list n = dots <> rule
  where
    dots = hcat' with {sep = 2} (replicate n dot)
    rule = hrule (width dots) # translateXTo dots

translateXTo ref mv = alignL mv # maybe id translateX (fst <$> extentX ref)

tree :: Tree () -> DC
tree =
  renderTree (const dot) (~~) . symmLayout' with { slHSep = 4, slVSep = 4 }

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
cyc 0 = mempty
cyc 1 = dot
cyc n
  = mconcat
    [ position (zip (polygon with { polyType = PolyRegular n r }) (repeat dot))
    , circle r
    ]
  where
    r = 4* fromIntegral n / tau

binTree :: BTree () -> DC
binTree Empty = square 1 # fc black
binTree t = fromMaybe mempty . fmap (renderTree (const dot) (~~)) . symmLayoutBin' with { slHSep = 5, slVSep = 4 } $ t

allBinTrees :: [[BTree ()]]
allBinTrees = map binTreesK [0..]
  where
    binTreesK 0 = [Empty]
    binTreesK n = [ BNode () t1 t2 | k <- [(n-1), (n-2) .. 0], t1 <- binTreesK k, t2 <- binTreesK (n - 1 - k)]

allTrees :: [[Tree ()]]
allTrees = map treesK [0..]
  where
    treesK 0 = []
    treesK n = [ Node () ts
               | part <- oPartitions (n-1)
               , ts <- mapM treesK part
               ]

oPartitions 0 = [[]]
oPartitions n | n < 0 = []
oPartitions n = concat [ map (k:) (oPartitions (n-k)) | k <- [n, n-1 .. 1] ]

------------------------------------------------------------
-- Bucketing
------------------------------------------------------------

-- XXX make bucket lines thicker

data BucketOpts
  = BucketOpts
  { _numBuckets    :: Int
  , _showEllipses  :: Bool
  , _bucketSize    :: Double
  , _expandBuckets :: Bool
  }

$(makeLenses ''BucketOpts)

instance Default BucketOpts where
  def = BucketOpts
    { _numBuckets        = 6
    , _showEllipses      = True
    , _bucketSize        = 10
    , _expandBuckets     = False
    }

bucketed' :: BucketOpts -> [[DC]] -> DC
bucketed' opts buckets
  = (if opts ^. showEllipses then (||| ellipses) else id)
  . hcat' with {sep = 1}
  . take (opts ^. numBuckets)
  . zipWith (makeBucket opts) [0..]
  $ buckets
  where
    ellipses = strutX 1 ||| hcat' with {sep = 1} (replicate 3 (circle 0.5 # fc black))

makeBucket :: BucketOpts -> Int -> [DC] -> DC
makeBucket opts n elts
    = vcat' with {sep = 1}
      [ bucketDia <> wrapLayout s s elts
      , text' 5 (show n)
      ]
  where
    bucketDia :: DC
    bucketDia = roundedRect s s (s / 8)
    s = opts ^. bucketSize

wrapLayout :: Double -> Double -> [DC] -> DC
wrapLayout w h = layoutGrid w h . wrap w h

wrap :: Double -> Double -> [DC] -> [[DC]]
wrap w h [] = []
wrap w h es = map snd this : wrap w h (map snd rest)
  where
    (this, rest) = span ((<w) . fst) esWeighted
    esWeighted :: [(Double, DC)]
    esWeighted = snd $ mapAccumL (\w e -> let w' = w + width e in (w', (w', e))) 0 es

layoutGrid :: Double -> Double -> [[DC]] -> DC
layoutGrid w h es = centerY . spread unit_Y h $ map (centerX . spread unitX w) es
  where
    spread :: R2 -> Double -> [DC] -> DC
    spread v total es = cat' v with {sep = (total - sum (map (extent v) es)) / (genericLength es + 1)} es
    extent v d
      = maybe 0 (negate . uncurry (-))
      $ (\f -> (-f (negateV v), f v)) <$> (appEnvelope . getEnvelope $ d)

bucketed :: [[DC]] -> DC
bucketed = bucketed' def

------------------------------------------------------------

theTree = tree (parseTree "((())()((()()())()(()())))") # centerXY

theGraph = graph [(0,1), (0,2), (1,3), (2,3), (2,4), (2,5), (3,5), (3,6), (6,7), (6,8)] # centerXY

theList = list 5 # centerXY

theCycles = hcat' with {sep = 2} [cyc 5, cyc 7] # centerXY # rotateBy (1/20)

------------------------------------------------------------
-- misc

text' d t = stroke (textSVG' $ TextOpts t lin INSIDE_H KERN False d d ) # fc black # lw 0
