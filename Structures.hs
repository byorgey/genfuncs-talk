{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Structures where

-- import Debug.Trace

import           Data.Colour.SRGB
import           Diagrams.Backend.PGF
import           Diagrams.Prelude          hiding (Empty, dot, highlight, trace,
                                            zero)
import           Diagrams.TwoD.Layout.Tree
import           Graphics.SVGFonts

import           Control.Applicative       ((<|>))
import           Control.Arrow             (second)
import           Control.Lens              (makeLenses, (^.))
import           Data.Default.Class
import           Data.List                 (foldl', genericLength, inits,
                                            mapAccumL, nub)
import qualified Data.Map                  as M
import           Data.Tree
import           Physics.ForceLayout
import           Text.Parsec               (between, char, many, runParser)
import           Text.Parsec.String        (Parser)

------------------------------------------------------------
-- misc

text' :: Double -> String -> Dia
text' d t = stroke (textSVG' $ TextOpts t lin INSIDE_H KERN False d d ) # fc black # lw 0

maximum0 :: (Num c, Ord c) => [c] -> c
maximum0 = maximum . (0:)

highlight :: (Monoid c, HasStyle c) => c
highlight = lc schoolDark . lw 0.5 $ mempty

houghtonPurple, houghtonGold :: Colour Double
houghtonPurple = sRGB24 0x2c 0x2a 0x6e
houghtonGold   = sRGB24 0xca 0xb7 0x78

williamsPurple, williamsGold :: Colour Double
williamsPurple = purple
williamsGold   = gold

schoolDark, schoolLight :: Colour Double
schoolDark  = williamsPurple
schoolLight = williamsGold

--------------------------------------------------
-- Types

type Dia = Diagram B

data Atom = Atom { structShape :: Int, structVariant :: Int, structColor :: Colour Double }
data Struct = SDia Dia
            | SAtom Atom
            | SPair Struct Struct
            | SMono Int Int  -- ax^b

data Bucket = Bucket { _bucket :: [Struct] }

makeLenses ''Bucket

type Species = [Bucket]

------------------------------------------------------------

nil :: Dia
nil = square 1.5 # fc white

dot :: Dia
dot = circle 1 # fc schoolDark # lw 0

list :: Int -> Dia
list 0 = nil
list n = dots <> rule
  where
    dots = hcat' (with & sep .~ 2) (replicate n dot ++ [nil])
    rule = hrule (width dots) # translateXTo dots

translateXTo :: (Transformable b, Alignable b, HasOrigin b, Enveloped a, V a ~ V2, V b ~ V2) => a -> b -> b
translateXTo ref mv = alignL mv # maybe id translateX (fst <$> extentX ref)

tree :: Tree () -> Dia
tree =
  renderTree (const dot) (~~) . symmLayout' (with & slHSep .~ 4 & slVSep .~ 4)

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
                Left _  -> error "parse error"
                Right t -> t

parseBTree :: String -> BTree ()
parseBTree s = case runParser bTreeParser () "" s of
                 Left _  -> error "parse error"
                 Right t -> t

graph :: [(Int,Int)] -> Dia
graph es = drawEnsemble es $
           forceLayout (with & damping     .~ 0.8
                             & energyLimit .~ Just 0.001
                             & stepLimit   .~ Nothing
                       )
           ens
  where
    ens :: Ensemble V2 Double
    ens = Ensemble [ (es,  hookeForce 0.05 4)
                   , (allPairs, coulombForce 1)
                   ]
                   particleMap
    vs = nub (map fst es ++ map snd es)
    allPairs = [(x,y) | x <- vs, y <- vs, x < y ]
    particleMap :: M.Map Int (Particle V2 Double)
    particleMap = M.fromList $ zip vs (map initParticle (regPoly (length vs) 4))

drawEnsemble :: [(Int,Int)] -> Ensemble V2 Double -> Dia
drawEnsemble es = applyAll (map drawEdge es) . mconcat . map drawPt . (map . second) (^.pos) . M.assocs . (^.particles)
  where
    drawPt (pid, p) = dot # named pid # moveTo p
    drawEdge (v1,v2) = withNames [v1,v2] $ \[s1,s2] -> beneath (location s1 ~~ location s2)

cyc :: Int -> Dia
cyc 0 = mempty
cyc 1 = dot
cyc n
  = mconcat
    [ position (zip (polygon (with & polyType .~ PolyRegular n r )) (repeat dot))
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
  . symmLayout' (with & slHSep .~ 5 & slVSep .~ 4)
  . decorateLeaves
  $ t

pair :: Dia -> Dia -> Dia
pair d1 d2 =
  hcat
  [ d1 # centerXY <> halfBox (w1 + padding) (h + padding)
  , d2 # centerXY <> halfBox (w2 + padding) (h + padding) # reflectX
  ]
  # lc black # lw 0.01
  where
    w1 = width d1
    w2 = width d2
    h  = max (height d1) (height d2)
    padding = maximum [w1 * padFactor, w2 * padFactor, h * padFactor]
    padFactor = 0.3
    halfBox x y = roundedRect' x y (with & radiusTL .~ min x y / 8 & radiusBL .~ min x y / 8 )

allBinTrees :: [[BTree ()]]
allBinTrees = map binTreesK [0..]
  where
    binTreesK :: Int -> [BTree ()]
    binTreesK 0 = [Empty]
    binTreesK n = [ BNode () t1 t2 | k <- [(n-1), (n-2) .. 0], t1 <- binTreesK k, t2 <- binTreesK (n - 1 - k)]

allBinTreesD :: [[Dia]]
allBinTreesD = map (map binTree) allBinTrees

allTrees :: [[Tree ()]]
allTrees = map treesK [0..]
  where
    treesK :: Int -> [Tree ()]
    treesK 0 = []
    treesK n = [ Node () ts
               | part <- oPartitions (n-1)
               , ts <- mapM treesK part
               ]

oPartitions :: (Enum a, Num a, Ord a) => a -> [[a]]
oPartitions 0 = [[]]
oPartitions n | n < 0 = []
oPartitions n = concat [ map (k:) (oPartitions (n-k)) | k <- [n, n-1 .. 1] ]

allListsD :: [[Dia]]
allListsD = map ((:[]) . list) [0..]

------------------------------------------------------------
-- Bucketing
------------------------------------------------------------

-- XXX make bucket lines thicker

data BucketOpts
  = BucketOpts
  { _numBuckets      :: Int
  , _showEllipses    :: Bool
  , _showIndices     :: Bool
  , _flipIndices     :: Bool
  , _bucketSize      :: Double
  , _bucketSep       :: Double
  , _expandBuckets   :: Bool
  , _shrinkFactor    :: Maybe Double
  , _padding         :: Maybe Double
  , _bucketDir       :: V2 Double
  , _bucketOptsStyle :: Style V2 Double
  , _showX           :: Bool
  }

$(makeLenses ''BucketOpts)

instance Default BucketOpts where
  def = BucketOpts
    { _numBuckets        = 6
    , _showEllipses      = True
    , _showIndices       = True
    , _flipIndices       = False
    , _bucketSize        = 10
    , _bucketSep         = 1
    , _expandBuckets     = False
    , _shrinkFactor      = Just 0.8
    , _padding           = Just 1.3
    , _bucketDir         = unitX
    , _bucketOptsStyle   = mempty
    , _showX             = False
    }

bucketed' :: BucketOpts -> [[Dia]] -> Dia
bucketed' opts buckets
  = (if opts ^. showEllipses then (\d -> cat' (opts ^. bucketDir) (with & sep .~ opts^.bucketSep) (d:ellipses)) else id)
  . cat' (opts ^. bucketDir) (with & sep .~ opts ^. bucketSep)
  . take (opts ^. numBuckets)
  . zipWith (makeBucket opts) [0..]
  . map (padBucket opts)
  $ buckets
  where
    ellipses = replicate 3 (circle 0.5 # fc black)

padBucket :: BucketOpts -> [Dia] -> [Dia]
padBucket opts = maybe id (\p -> (map (pad p . centerXY))) (opts ^. padding)

makeBucket :: BucketOpts -> Int -> [Dia] -> Dia
makeBucket opts n elts
    = (if (opts ^. showIndices) then (\d -> cat' iDir (with & sep.~opts ^. bucketSep) [d,text' 5 (show n)]) else id)
      (wrapLayout (opts ^. shrinkFactor) s s elts <> bucketDia)
  where
    bucketDia :: Dia
    bucketDia = roundedRect s s (s / 8)
      # applyStyle (opts ^. bucketOptsStyle)
    s = opts ^. bucketSize
    iDir = (if opts^.flipIndices then negated else id) . perp $ opts^.bucketDir

wrapLayout :: Maybe Double -> Double -> Double -> [Dia] -> Dia
wrapLayout Nothing w h = layoutGrid w h . wrap w
wrapLayout (Just shrink) w h = go
  where
    go ds
      | {- traceShow totalH -} totalH <= h = layoutGrid w h dss
      | otherwise = go (map (scale shrink) ds)
      where
        dss = wrap w ds
        totalH = sum . map (maximum0 . map height) $ dss

wrap :: Double -> [Dia] -> [[Dia]]
wrap w ds = wrap' w ds'
  where
    maxW = maximum0 . map width $ ds
    ds'
      | maxW >= w = map (scale (0.99 * (w / maxW))) ds
      | otherwise = ds

wrap' :: Double -> [Dia] -> [[Dia]]
wrap' _ [] = []
wrap' w es = map snd this : wrap' w (map snd rest)
  where
    (this, rest) = span ((<w) . fst) esWeighted
    esWeighted :: [(Double, Dia)]
    esWeighted = snd $ mapAccumL (\wcur ecur -> let w' = wcur + width ecur in (w', (w', ecur))) 0 es

layoutGrid :: Double -> Double -> [[Dia]] -> Dia
layoutGrid w h es = centerY . spread unit_Y h $ map (centerX . spread unitX w) es
  where
    spread :: V2 Double -> Double -> [Dia] -> Dia
    spread v total ds = cat' v (with & sep .~ (total - sum (map (extent v) ds)) / (genericLength ds + 1)) ds
    extent v d
      = maybe 0 (negate . uncurry (-))
      $ (\f -> (-f (negated v), f v)) <$> (appEnvelope . getEnvelope $ d)

bucketed :: [[Dia]] -> Dia
bucketed = bucketed' def

data Grid a = Grid Species Species [[a]]

class IsBucket a where
  drawBucket :: BucketOpts -> a -> Dia

instance IsBucket Bucket where
  drawBucket opts (Bucket b) = makeBucket (opts & showIndices .~ False) 0 (padBucket opts $ map (drawStruct opts) b)

instance IsBucket (QDiagram B v n m) where
  drawBucket _ d = d

instance IsBucket a => IsBucket (Maybe a) where
  drawBucket opts = maybe (strutX (opts^.bucketSize) <> strutY (opts^.bucketSize)) (drawBucket opts)

instance IsBucket a => IsBucket (a, Style V2 Double) where
  drawBucket opts (b,sty) = drawBucket opts b # applyStyle sty

drawGrid :: IsBucket a => BucketOpts -> Grid a -> Dia
drawGrid opts (Grid col row bs) =
  hor
    [ drawSpecies' (opts & bucketDir .~ unit_Y & showEllipses .~ False) col
    , strutX 0
    , ver
      [ drawSpecies' (opts & bucketDir .~ unitX & showEllipses .~ False & flipIndices .~ True) row
      , strutY 0
      , grid
      ]
    ]
  where
    grid
      = bs
      # map (take n)
      # take n
      # map (map (drawBucket opts))
      # map hor
      # vcat' (with & sep .~ opts ^. bucketSep)
    n = opts ^. numBuckets
    hor = hcat' (with & sep .~ opts ^. bucketSep) . map alignB
    ver = vcat' (with & sep .~ opts ^. bucketSep) . map alignR

productGrid :: Species -> Species -> (((Int,Int),Bucket) -> Maybe (Bucket, Style V2 Double)) -> Dia
productGrid = productGrid' with

productGrid' :: BucketOpts -> Species -> Species -> (((Int,Int),Bucket) -> Maybe (Bucket, Style V2 Double)) -> Dia
productGrid' opts s1 s2 f = drawGrid opts (Grid s1 s2 grid)
  where
    grid = [ [ f ((r,c), (s1 !! r) %* (s2 !! c))
             | c <- [0..5]
             ]
           | r <- [0..5]
           ]

gridHighlightDiag :: Int -> Species -> Species -> Dia
gridHighlightDiag = gridHighlightDiag' with

gridHighlightDiag' :: BucketOpts -> Int -> Species -> Species -> Dia
gridHighlightDiag' opts n sp1 sp2 = productGrid' opts sp1 sp2 f
  where
    f ((r,c),b)
      | r + c == n = Just (b, highlight)
      | otherwise  = Just (b, mempty)

prodSum :: Int -> Species -> Species -> Dia
prodSum = prodSum' with

prodSum' :: BucketOpts -> Int -> Species -> Species -> Dia
prodSum' opts n sp1 sp2 = vcat' (with & sep .~ 5)
  [ gridHighlightDiag' opts n sp1 sp2
  , drawSpecies' (opts & numBuckets .~ (n+1)) (sp1 %* sp2)
    # translateX 12
  ]

drawGF :: [Int] -> Dia
drawGF = drawGF' with

drawGF' :: BucketOpts -> [Int] -> Dia
drawGF' opts = drawSpecies' opts . mkGF

------------------------------------------------------------

treeDef :: Dia
treeDef = vcat' (with & catMethod .~ Distrib & sep .~ 4)
  [ dot # named "parent"
  , hcat' (with & catMethod .~ Distrib & sep .~ 6)
    [ subtree # named "left"
    , subtree # named "right"
    ]
    # centerX
  ]
  # withNames ["parent", "left", "right"] (\[p,l,r] ->
      beneath (location p ~~ location l <> location p ~~ location r)
    )

subtree :: Dia
subtree = triangle 4 # scaleY 1.5 # atop (text' 3 "T") # alignT

------------------------------------------------------------

theTree :: Dia
theTree = tree (parseTree "((())()((()()())()(()())))") # centerXY

treeA, treeB :: BTree ()
treeA = (allBinTrees !! 3 !! 2)
treeB = (allBinTrees !! 5 !! 22)

theGraph :: Dia
theGraph = graph [(0,1), (0,2), (1,3), (2,3), (2,4), (2,5), (3,5), (3,6), (6,7), (6,8)] # centerXY

theList :: Dia
theList = list 5 # centerXY

theCycles :: Dia
theCycles = hcat' (with & sep .~ 2) [cyc 5, cyc 7] # centerXY # rotateBy (1/20)

------------------------------------------------------------

shape :: Int -> Located (Trail' Loop V2 Double)
shape 0 = square 1.5
shape 1 = circle 1
shape 2 = rect 4 2
shape n = polygon (with & polyType .~ PolyRegular n 2)

variant :: Int -> Located (Trail' Loop V2 Double) -> Dia
variant 0 = strokeLocLoop
variant 1 = fc white . strokeLocLoop
variant 2 = \t -> strokeLocLoop t # scale 0.5 <> strokeLocLoop t # fc white
variant 3 = \t -> strokeLocLoop t # scale 0.5 # fc white <> strokeLocLoop t # fc white
variant _ = variant 3

drawStruct :: BucketOpts -> Struct -> Dia
drawStruct _ (SDia d) = d
drawStruct _ (SAtom (Atom shp var c)) = shape shp # variant var # lc c # fc c
drawStruct opts (SPair s1 s2) = pair (drawStruct opts s1) (drawStruct opts s2)
drawStruct opts (SMono a n)
  | opts ^. showX = (||| (strutX 0.3 ||| text' 3 (show n) # translateY 1)) . text' 6 . (++"x") . show $ a
  | otherwise     = text' 8 (show a)

drawSpecies :: Species -> Dia
drawSpecies = drawSpecies' with

drawSpecies' :: BucketOpts -> Species -> Dia
drawSpecies' opts = bucketed' opts . map (map (drawStruct opts) . (^.bucket))

mkSpecies :: Colour Double -> [Int] -> Species
mkSpecies c = zipWith (\i n -> Bucket [SAtom (Atom i var c) | var <- [0..(n-1)]]) [0..]

speciesA, speciesB, speciesOne, speciesX :: Species
speciesA = mkSpecies schoolLight [1,1,1,3,4,2]
speciesB = mkSpecies schoolDark [1,1,2,0,3,1]
speciesOne = Bucket [SDia (square 1 # fc black)] : repeat zero
speciesX =  zero : Bucket [SDia dot] : repeat zero

mkGF :: [Int] -> Species
mkGF  = zipWith asBucket [0..]
  where
    asBucket n a = Bucket [SMono a n]

gf1, gf2 :: Species
gf1 = mkGF [1..]
gf2 = mkGF [1,1,2,5,14,42]

------------------------------------------------------------

class Semiring a where
  zero :: a
  one  :: a
  (%+) :: a -> a -> a
  (%*) :: a -> a -> a

instance Semiring Bucket where
  zero = Bucket []
  one  = Bucket [SAtom (Atom 0 0 black)]
  Bucket [SMono a1 n1] %+ Bucket [SMono a2 _] = Bucket [SMono (a1+a2) n1]  -- assume n1 == n2
  Bucket b1 %+ Bucket b2 = Bucket (b1 ++ b2)
  Bucket b1 %* Bucket b2 = Bucket [s1 %%* s2 | s1 <- b1, s2 <- b2]
    where
      (SMono i m) %%* (SMono j n) = SMono (i * j) (m + n)
      s1 %%* s2 = SPair s1 s2

instance Semiring a => Semiring [a] where
  zero = repeat zero
  one  = one : repeat zero
  (%+) = zipWith (%+)
  l1 %* l2 = map (foldl' (%+) zero . zipWith (%*) l1) . map reverse . tail . inits $ l2

instance Semiring Int where
  zero = 0
  one  = 1
  (%+) = (+)
  (%*) = (*)

--------------------------------------------------
-- misc util

hc3  = hcat' (with & sep .~ 3)
vc3  = vcat' (with & sep .~ 3)
vc3r = vc3 . map alignR
p1 = pad 1.1 . centerXY
