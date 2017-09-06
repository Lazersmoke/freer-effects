{-# LANGUAGE GADTs #-}
-- | Fast type-aligned queue optimized to effectful functions.
--
-- * Constant-time append\/('><') and snoc\/('|>')
-- * Average constant-time 'viewL' (left-edge deconstruction).
--
-- Using <http://okmij.org/ftp/Haskell/extensible/FTCQueue1.hs> as a starting point.
--
-- A minimal version of FTCQueue from "Reflection w/o Remorse":
--
-- * Research: <http://okmij.org/ftp/Haskell/Reflection.html>
-- * <https://hackage.haskell.org/package/type-aligned type-aligned> (FTCQueue)
module Data.FTCQueue
  (FTCQueue
  ,singleton
  ,(|>)
  ,snoc
  ,(><)
  ,append
  ,ViewL(..)
  ,tviewl
  ) where

-- | An @'FTCQueue m a b'@ is an efficient representation of an effectful
-- arrow from @a@ to @b@ in the monad @m@. 
--
-- It is represented as a type-aligned, non-empty, binary tree where the @'Leaf'@s are 
-- smaller effectful arrows, and the @'Node'@s represent left-to-right composition of effectful arrows.
data FTCQueue m a b where
  -- | A single effectful arrow can stand on its own.
  Leaf :: (a -> m b) -> FTCQueue m a b
  -- | You can compose two trees with effectful composition.
  -- This will be turned into @'>>='@ chains later.
  Node :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b

-- | Create an @'FTCQueue'@ from a single effectful arrow.
--
-- > singleton = Leaf
--
-- /O(1)/
singleton :: (a -> m b) -> FTCQueue m a b
singleton = Leaf

-- | Append an effectful arrow to the right of the tree. 
--
-- /O(1)/
(|>) :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
t |> r = Node t (Leaf r)
{-# INLINE (|>) #-}

-- | An alias for @'|>'@
snoc :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
snoc = (|>)
{-# INLINE snoc #-}

-- | Compose two type-aligned trees.
--
-- /O(1)/
(><) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
t1 >< t2 = Node t1 t2
{-# INLINE (><) #-}

-- | An alias for '(><)'
append :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
append = (><)
{-# INLINE append #-}

-- | Destruct a @'FTCQueue'@ from the left.
data ViewL m a b where
  TOne  :: (a -> m b) -> ViewL m a b
  (:|)  :: (a -> m x) -> FTCQueue m x b -> ViewL m a b

-- | Left view deconstruction. 
--
-- /Average O(1)/
tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (Leaf r)   = TOne r
tviewl (Node t1 t2) = go t1 t2
  where
    go :: FTCQueue m a x -> FTCQueue m x b -> ViewL m a b
    go (Leaf r) tr = r :| tr
    go (Node tl1 tl2) tr = go tl1 (Node tl2 tr)
