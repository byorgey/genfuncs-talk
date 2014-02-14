Today I’d like to talk about generating random trees. First, some imports and such (this post is literate Haskell).

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE PackageImports #-}
>
> module BoltzmannTrees where
>
> import           Control.Applicative
> import           Control.Lens                   ((??))
> import           Control.Monad.Random
> import "mtl"     Control.Monad.Reader
> import "mtl"     Control.Monad.State
> import           Control.Monad.Trans.Maybe
> import           Diagrams.TwoD.Layout.Tree

So here’s a simple type of binary tree shapes, containing no data:

> data Tree = Leaf | Branch Tree Tree
>   deriving Show
>
> toBTree :: Tree -> BTree ()
> toBTree Leaf = Empty
> toBTree (Branch l r) = BNode () (toBTree l) (toBTree r)

We’ll count each constructor (Leaf or Branch) as having a size of 1:

> size :: Tree -> Int
> size Leaf = 1
> size (Branch l r) = 1 + size l + size r

Now, suppose we want to randomly generate these trees. This is an entirely reasonable and useful thing to do: perhaps we want to, say, randomly test properties of functions over Tree using QuickCheck. Here’s the simplest, most naïve way to do it:

> randomTree :: (Applicative m, MonadRandom m) => m Tree
> randomTree = do
>   r <- getRandom
>   if r < (1/2 :: Double)
>     then return Leaf
>     else Branch <$> randomTree <*> randomTree

We choose each of the constructors with probability 1/2, and recurse in the Branch case.

Now, as is well-known, this works rather poorly. Why is that? Let’s generate 100 random trees and print out their sizes in descending order:

ghci> reverse . sort . map size <$> replicateM 100 randomTree
  [118331,7753,2783,763,237,203,195,163,159,73,65,63,49,41,39,29,29,23,23,21,19,19,15,11,9,9,9,9,7,7,7,5,5,5,5,5,5,5,5,5,3,3,3,3,3,3,3,3,3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

As you can see, this is a really weird distribution of sizes. For one thing, we get lots of trees that are very small—in fact, it’s easy to see that we expect about 50 of them to be single leaf nodes. The other weird thing, however, is that we also get some really humongous trees. The above output gets randomly regenerated every time I process this post—so I don’t know exactly what sizes you’ll end up seeing—but it’s a good bet that there is at least one tree with a size greater than 10^4. To get an intuitive idea of why this happens, imagine generating the tree in a breadth-first manner. At each new level we have a collection of “active” nodes corresponding to pending recursive calls to randomTree. Each active node generates zero or two new active nodes on the next level with equal probability, so on average the number of active nodes remains the same from level to level. So if we happen to make a lot of Branch choices right off the bat, it may take a long time before the tree “thins out” again. And if this distribution didn’t seem weird enough already, it turns out (though it is far from obvious how to prove this) that the expected size of the generated trees is infinite!

The usual solution with QuickCheck is to use the sized combinator to limit the size of generated structures, but this does not help with the problem of having too many very small trees.

Here’s a (seemingly!) stupid idea. Suppose we want to generate trees of size approximately 100 (say, within 10%). Let’s simply use the above algorithm, but with the following modifications:

    If we generate a tree of size < 90, throw it away and start over.
    If we generate a tree of size > 110, throw it away and start over. As an optimization, however, we will stop as soon as the size goes over 110; that is, we will keep track of the current size while generating and stop early if the size gets too big.

Here’s some code. First, a monad onion:

> newtype GenM a = GenM
>     { unGenM :: ReaderT (Int,Int) (StateT Int (MaybeT (Rand StdGen))) a }
>   deriving (Functor, Applicative, Monad, MonadPlus, MonadRandom,
>             MonadState Int, MonadReader (Int,Int))

The ReaderT holds the min and max allowed sizes; the StateT holds the current size; the MaybeT allows for possible failure (if the tree gets too big or ends up too small), and the Rand StdGen is, of course, for generating random numbers. To run a computation in this monad we take a target size and a tolerance and use them to compute minimum and maximum sizes. (The (??) in the code below is an infix version of flip, defined in the lens package.)

> runGenM :: Int -> Double -> GenM a -> IO (Maybe a)
> runGenM targetSize eps m = do
>   let wiggle  = floor $ fromIntegral targetSize * eps
>       minSize = targetSize - wiggle
>       maxSize = targetSize + wiggle
>   g <- newStdGen
>   return . (evalRand ?? g) . runMaybeT . (evalStateT ?? 0)
>          . (runReaderT ?? (minSize, maxSize)) . unGenM
>          $ m

Here’s the code to try generating a tree: we call the atom function to record the increase in size, and choose between the two constructors with equal probability. atom, in turn, handles failing early if the size gets too big.

> genTreeUB :: GenM Tree
> genTreeUB = do
>   r <- getRandom
>   atom
>   if r <= (1/2 :: Double)
>     then return Leaf
>     else Branch <$> genTreeUB <*> genTreeUB
>
> atom :: GenM ()
> atom = do
>   (_, maxSize) <- ask
>   curSize <- get
>   when (curSize >= maxSize) mzero
>   put (curSize + 1)

genTreeLB calls genTreeUB and then performs the lower bound check on the size.

> genTreeLB :: GenM Tree
> genTreeLB = do
>   put 0
>   t <- genTreeUB
>   tSize <- get
>   (minSize, _) <- ask
>   guard $ tSize >= minSize
>   return t

Finally, genTree just calls genTreeLB repeatedly until it succeeds.

> genTree :: GenM Tree
> genTree = genTreeLB `mplus` genTree
