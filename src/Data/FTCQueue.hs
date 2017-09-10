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
  ,singleton'
  ,snoc
  ,(><)
  ,ViewL(..)
  ,viewl
  ) where

-- | An @'FTCQueue p a b'@ is an efficient representation of an effectful
-- arrow from @a@ to @b@ in the arrow @p@. 
--
-- It is represented as a type-aligned, non-empty, binary tree where the @Leaf@s are 
-- smaller effectful arrows, and the @Node@s represent left-to-right composition of effectful arrows.
data FTCQueue p a b where
  -- | A single effectful arrow can stand on its own.
  Leaf :: p a b -> FTCQueue p a b
  -- | You can compose two trees with effectful composition.
  Node :: FTCQueue p a x -> FTCQueue p x b -> FTCQueue p a b

-- | Create an @'FTCQueue'@ from a single effectful arrow.
--
-- > singleton = Leaf
--
-- /O(1)/
singleton' :: p a b -> FTCQueue p a b
singleton' = Leaf

-- | Append an effectful arrow to the right of the tree. 
--
-- /O(1)/
snoc :: FTCQueue p a x -> p x b -> FTCQueue p a b
snoc ax xb = Node ax (Leaf xb)
{-# INLINE snoc #-}

-- | Compose two type-aligned trees.
--
-- /O(1)/
(><) :: FTCQueue p a x -> FTCQueue p x b -> FTCQueue p a b
(><) = Node
{-# INLINE (><) #-}

-- | Destruct a @'FTCQueue'@ from the left.
data ViewL p a b where
  One :: p a b -> ViewL p a b
  Cons :: p a x -> FTCQueue p x b -> ViewL p a b

-- | Left view deconstruction. 
--
-- /Average O(1)/
viewl :: FTCQueue p a b -> ViewL p a b
viewl (Leaf r) = One r
viewl (Node t1 t2) = merge t1 t2
  where
    merge :: FTCQueue p a x -> FTCQueue p x b -> ViewL p a b
    merge (Leaf x) xs = Cons x xs
    merge (Node a b) c = merge a (Node b c)
