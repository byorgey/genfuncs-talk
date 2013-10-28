{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Structures where

import           Diagrams.Backend.Postscript
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree
import           Graphics.SVGFonts

import           Control.Applicative         ((<|>))
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

type Dia = Diagram Postscript R2

nil :: Dia
nil = square 1 # fc white

dot :: Dia
dot = circle 1 # fc blue # lw 0

list :: Int -> Dia
list 0 = nil
list n = dots <> rule
  where
    dots = hcat' with {sep = 2} (replicate n dot)
    rule = hrule (width dots) # translateXTo dots

translateXTo ref mv = alignL mv # maybe id translateX (fst <$> extentX ref)

tree :: Tree () -> Dia
tree =
  renderTree (const dot) (~~) . symmLayout' with { slHSep = 4, slVSep = 4 }

treeParser :: Parser (Tree ())
treeParser = Node () <$> between (char '(') (char ')') (many treeParser)

bTreeParser :: Parser (BTree ())
bTreeParser = char '(' *>
  (     BNode () <$> bTreeParser <*> bTreeParser
    <|> pure Empty
  )
  <* char ')'

parseTree :: String -> Tree ()
parseTree s = case runParser treeParser () "" s of
                Left _ -> error "parse error"
                Right t -> t

parseBTree :: String -> BTree ()
parseBTree s = case runParser bTreeParser () "" s of
                 Left _ -> error "parse error"
                 Right t -> t

graph :: [(Int,Int)] -> Dia
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

drawEnsemble :: [(Int,Int)] -> Ensemble R2 -> Dia
drawEnsemble es = applyAll (map drawEdge es) . mconcat . map drawPt . (map . second) (^.pos) . M.assocs . (^.particles)
  where
    drawPt (pid, p) = dot # named pid # moveTo p
    drawEdge (v1,v2) = withNames [v1,v2] $ \[s1,s2] -> beneath (location s1 ~~ location s2)

cyc :: Int -> Dia
cyc 0 = mempty
cyc 1 = dot
cyc n
  = mconcat
    [ position (zip (polygon with { polyType = PolyRegular n r }) (repeat dot))
    , circle r
    ]
  where
    r = 4* fromIntegral n / tau

decorateLeaves :: BTree a -> Tree (Maybe a)
decorateLeaves Empty = Node Nothing []
decorateLeaves (BNode a l r) = Node (Just a) [decorateLeaves l, decorateLeaves r]

binTree :: BTree () -> Dia
binTree Empty = nil
binTree t
  = renderTree (maybe nil (const dot)) (~~)
  . symmLayout' with { slHSep = 5, slVSep = 4 }
  . decorateLeaves
  $ t

pair :: Dia -> Dia -> Dia
pair d1 d2 =
  hcat
  [ d1 # centerXY <> halfBox (w1 + padding) (h + padding)
  , d2 # centerXY <> halfBox (w2 + padding) (h + padding) # reflectX
  ]
  where
    w1 = width d1
    w2 = width d2
    h  = max (height d1) (height d2)
    padding = maximum [w1 * padFactor, w2 * padFactor, h * padFactor]
    padFactor = 0.2
    halfBox w h = roundedRect' w h with { radiusTL = min w h / 8, radiusBL = min w h / 8 }

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
  , _showIndices   :: Bool
  , _bucketSize    :: Double
  , _expandBuckets :: Bool
  }

$(makeLenses ''BucketOpts)

instance Default BucketOpts where
  def = BucketOpts
    { _numBuckets        = 6
    , _showEllipses      = True
    , _showIndices       = True
    , _bucketSize        = 10
    , _expandBuckets     = False
    }

bucketed' :: BucketOpts -> [[Dia]] -> Dia
bucketed' opts buckets
  = (if opts ^. showEllipses then (||| ellipses) else id)
  . hcat' with {sep = 1}
  . take (opts ^. numBuckets)
  . zipWith (makeBucket opts) [0..]
  $ buckets
  where
    ellipses = strutX 1 ||| hcat' with {sep = 1} (replicate 3 (circle 0.5 # fc black))

makeBucket :: BucketOpts -> Int -> [Dia] -> Dia
makeBucket opts n elts
    = (if (opts ^. showIndices) then (=== (strutY 1 === text' 5 (show n))) else id)
      (bucketDia <> wrapLayout s s elts)
  where
    bucketDia :: Dia
    bucketDia = roundedRect s s (s / 8)
    s = opts ^. bucketSize

wrapLayout :: Double -> Double -> [Dia] -> Dia
wrapLayout w h = layoutGrid w h . wrap w h

wrap :: Double -> Double -> [Dia] -> [[Dia]]
wrap w h [] = []
wrap w h es = map snd this : wrap w h (map snd rest)
  where
    (this, rest) = span ((<w) . fst) esWeighted
    esWeighted :: [(Double, Dia)]
    esWeighted = snd $ mapAccumL (\w e -> let w' = w + width e in (w', (w', e))) 0 es

layoutGrid :: Double -> Double -> [[Dia]] -> Dia
layoutGrid w h es = centerY . spread unit_Y h $ map (centerX . spread unitX w) es
  where
    spread :: R2 -> Double -> [Dia] -> Dia
    spread v total es = cat' v with {sep = (total - sum (map (extent v) es)) / (genericLength es + 1)} es
    extent v d
      = maybe 0 (negate . uncurry (-))
      $ (\f -> (-f (negateV v), f v)) <$> (appEnvelope . getEnvelope $ d)

bucketed :: [[Dia]] -> Dia
bucketed = bucketed' def

------------------------------------------------------------

listBuckets opts = bucketed' opts (map (:[]) . zipWith scale ([1,1,1,0.6] ++ repeat 0.4) . map list $ [0..])

binTreeBuckets opts
  = bucketed' opts
      ( map (map (pad 1.3 . centerXY . binTree)) allBinTrees
      # zipWith scale [1,1,0.4, 0.2, 0.1, 0.05]
      )

treeDef = vcat' with {catMethod = Distrib, sep = 4}
  [ dot # named "parent"
  , hcat' with {catMethod = Distrib, sep = 6}
    [ subtree # named "left"
    , subtree # named "right"
    ]
    # centerX
  ]
  # withNames ["parent", "left", "right"] (\[p,l,r] ->
      beneath (location p ~~ location l <> location p ~~ location r)
    )

subtree = triangle 4 # scaleY 1.5 # atop (text' 3 "T") # alignT

------------------------------------------------------------

theTree = tree (parseTree "((())()((()()())()(()())))") # centerXY

theGraph = graph [(0,1), (0,2), (1,3), (2,3), (2,4), (2,5), (3,5), (3,6), (6,7), (6,8)] # centerXY

theList = list 5 # centerXY

theCycles = hcat' with {sep = 2} [cyc 5, cyc 7] # centerXY # rotateBy (1/20)

------------------------------------------------------------

data Struct = Struct { structShape :: Int, structVariant :: Int, structColor :: Colour Double }

shape :: Int -> Located (Trail' Loop R2)
shape 0 = circle 1
shape 1 = circle 1 # scaleY 1.5
shape 2 = rect 4 2
shape n = polygon with {polyType = PolyRegular n 2}

variant :: Int -> Located (Trail' Loop R2) -> Dia
variant 0 = strokeLocLoop
variant 1 = fc white . strokeLocLoop
variant 2 = \t -> strokeLocLoop t # scale 0.8 <> strokeLocLoop t # fc white
variant 3 = \t -> strokeLocLoop t # scale 0.8 # fc white <> strokeLocLoop t # fc white
variant _ = variant 3

drawStruct :: Struct -> Dia
drawStruct (Struct shp var c) = shape shp # variant var # lc c # fc c

type Species = [[Struct]]

drawSpecies :: BucketOpts -> Species -> Dia
drawSpecies opts = bucketed' opts . map (map drawStruct)

mkSpecies :: Colour Double -> [Int] -> [[Struct]]
mkSpecies c = zipWith (\i n -> [Struct i var c | var <- [0..(n-1)]]) [0..]

speciesA = mkSpecies green [1,1,1,3,4,2]

speciesB = mkSpecies purple [1,0,2,0,3,0]

------------------------------------------------------------
-- misc

text' d t = stroke (textSVG' $ TextOpts t lin INSIDE_H KERN False d d ) # fc black # lw 0
