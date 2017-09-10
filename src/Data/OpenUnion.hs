{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

-- Required to write most of the type level recursive functions
-- used in the Membership tests.
{-# LANGUAGE UndecidableInstances #-}

-- | /These are internal definitions and should be used with caution. There are no/
-- /guarantees that the API of this module will be preserved between minor/
-- /versions of this package./
--
-- This module provides @'Union'@s of type constructors, suitable for defining sets
-- of effects. See "Control.Monad.Freer.Internal" for more information on the application
-- of this type to extensible effects.
--
-- These are called \"Open unions" (or type-indexed co-products, or type-indexed sums)
-- because if you consider types as sets of possible values, then a @'Union'@ between types has
-- the union of each of those sets of possible values as its own set of possible values.
-- 
-- For example, the union of the types (sets) @'Bool' = {'True','False'}@ and 
-- @'Maybe' [()](https://www.youtube.com/watch?v=dQw4w9WgXcQ) = {'Just' [()](https://www.youtube.com/watch?v=dQw4w9WgXcQ),'Nothing'}@ is the set @{'True', 'False', 'Just' [()](https://www.youtube.com/watch?v=dQw4w9WgXcQ), 'Nothing'}@.
--
-- This module doesn't actually provide unions for concrete types, but rather unions 
-- of type constructors over a single type parameter. See @'Union'@ for more information.
--
-- Based (very loosely) on
-- <http://okmij.org/ftp/Haskell/extensible/OpenUnion51.hs OpenUnion51.hs>.
--
-- All operations are constant-time because they take place at compile-time.
module Data.OpenUnion where

import Data.Kind (Type,Constraint)
import Data.Word (Word)
import Unsafe.Coerce (unsafeCoerce)

import GHC.TypeLits (TypeError,ErrorMessage(..))

-- | A @'Union' r a@ is a value that is one of the type constructors in @r@ applied to @a@,
-- but we don't know which one. For example, a @'Union' '['Maybe',[],'IO'] 'Int'@ is either a
-- @'Maybe' 'Int'@, a @['Int']@, or a @'IO' 'Int'@.
--
-- Technically, a @'Union' r a@ is an index in @r@, represented by a @'Word'@,
-- and a value of type @t a@, where @r@ at the index is @t@. That @t@ is actually at that index
-- is /not/ checked by the compiler, so working with @'Union'@ directly is __unsafe__.
--
-- You can view a @'Union' r a@ as a type-level fold, where all the @'':'@ is replaced with @'Either'@s.
-- This means that a @'Union'@ is really just a nested sum type. 
-- This makes sense because saying \"One of a, b, or c" is the same as say \"Either a or (Either b or c)"
data Union (r :: [Type -> Type]) a where
  Union :: {-# UNPACK #-} !Word -> t a -> Union r a

-- | Creates a @'Union' r a@ with actual inhabitant @t a@.
--
-- The user of this function promises to ensure that the 
-- type of @t@ is in the @'Union'@s list at the specified index.
--
-- __This function is unsafe__ because that promise is not checked.
--
-- /O(1)/
unsafeInj :: 
  Word -- ^ The index in the @'Union'@s list to place the inhabitant at
  -> t a -- ^ The actual inhabitant
  -> Union r a
unsafeInj = Union
{-# INLINE unsafeInj #-}

-- | Get @'Just'@ the actual inhabitant of the @'Union'@ if it is at the
-- specified index, or @'Nothing'@ if that is not the index of the inhabitant.
--
-- The user of this function promises that the given @'Union'@ is a legitimate one,
-- meaning that the actual inhabitant is of the type it claims to be.
-- That is, @r@ at the given index is @t@.
--
-- __This function is unsafe__ because that promise is not checked.
--
-- /O(1)/
unsafePrj :: 
  Word -- ^ The index to check for the inhabitant at.
  -> Union r a -- ^ The @'Union'@ to check fro the inhabitant in.
  -> Maybe (t a) -- ^ @'Just'@ the inhabitant, or @'Nothing'@ for the wrong index.
unsafePrj n (Union n' x)
  -- If they provided the correct index, then they also promise
  -- that the type is correct, so we can unsafeCoerce here.
  | n == n'   = Just (unsafeCoerce x)
  | otherwise = Nothing
{-# INLINE unsafePrj #-}


-- | Type level list concatenation
type family (xs :: [a]) ++ (ys :: [a]) :: [a] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Peano
data Nat = S Nat | Z

-- | A single entry of a preorder traversal of a type
data PreordEntry = App | forall a. Con a

-- | A preorder traversal of a type:
--
-- >>> Preord (Maybe Int)
-- '[App, Con Maybe, Con Int]
-- 
-- >>> Preord (Maybe f)
-- '[App, Con Maybe, Con f]
type family Preord (x :: a) :: [PreordEntry] where
  -- The preorder traversal of an application is
  -- an @''App'@ of the constructor's preorder
  -- traveral to the contructee's.
  Preord (f x) = 'App ': (Preord f ++ Preord x)
  -- The preorder traversal of a non-application
  -- is just itself in a @''Con'@
  Preord x = '[ 'Con x]

-- | Type level equality test (structural)
type family (a :: x) == (b :: y) :: Bool where
  x == x = 'True
  _ == _ = 'False

-- | Map a list of types to a pair of its index in the list, and its preorder traversal.
-- We do both at once because its faster ;-)
type family PreordList (xs :: [a]) (i :: Nat) :: [(Nat,[PreordEntry])] where
  PreordList '[] _ = '[]
  PreordList (x ': xs) i = '(i,Preord x) ': PreordList xs ('S i)

-- | Narrow down our search by keeping only the preords with a first entry matching ours.
-- This preserves the /original/ indexes of the traversals so we can index to them later.
-- It also throws away the entry that was matched upon finding it, leaving the tail of the
-- traversal.
type family Narrow (e :: PreordEntry) (xs :: [(Nat,[PreordEntry])]) :: [(Nat,[PreordEntry])] where
  Narrow _ '[] = '[]
  Narrow e ('(i,e ': es) ': rest) = '(i,es) ': Narrow e rest
  Narrow e (_ ': rest) = Narrow e rest

-- | Find the inex of a preorder in a @'PreordList'@'d list.
type family Find' (origTarget :: [PreordEntry]) (origHaystack :: [(Nat,[PreordEntry])]) (target :: [PreordEntry]) (haystack :: [(Nat,[PreordEntry])]) :: Nat where
  -- If there is only one thing in the haystack, it must be the needle :P
  Find' _ _ _ '[ '(i,_)] = i
  -- Disambiguate by selecting the first exact match
  Find' _ _ e ('(i,e) ': _) = i
  -- Recursively narrow the haystack wrt the target until we end up with only one item
  Find' ot oh (e ': es) ess = Find' ot oh es (Narrow e ess)
  {-
  We disambiguate by selecting the first exact match now, might reverse this descision later
  if there turns out to be a better way of doing this.
  Find' ot oh '[] es = TypeError (
    'Text "Ran out of preorder entries while narrowing: " ':<>: 'ShowType es ':$$: 
    'Text "This means we have no further information with which to disambiguate the remaining possible matches :(" ':$$:
    'Text "Original target: " ':<>: 'ShowType ot ':$$:
    'Text "Original haystack: " ':<>: 'ShowType oh)
  -}
  Find' ot oh e '[] = TypeError (
    'Text "Exhausted all possible matches for: " ':<>: 'ShowType e ':$$:
    'Text "This means we have run out of possible matches, so the target is simply absent from the list :(" ':$$:
    'Text "Original target: " ':<>: 'ShowType ot ':$$:
    'Text "Original haystack: " ':<>: 'ShowType oh)

-- | Convert from a normal type in a normal list of types to preorder traversals
type Find x ys = Find' (Preord x) (PreordList ys 'Z) (Preord x) (PreordList ys 'Z)

-- | The constraint @'MemberAt' i t r@ means that @t@ is in @r@ at the index @i@.
class MemberAt (i :: Nat) (t :: Type -> Type) (r :: [Type -> Type]) where

-- | Base case. If @t@ is at the head of @r@, then the index is 0.
instance MemberAt 'Z t (t ': r) where

-- | Inductive case. Read: @t@ being a @'Member'@ of @r@ __implies that__
-- @t@ is also a @'Member'@ of the union of some @arbitrary@ value and @r@. 
-- The proof of this statement is that the index of @t@ in @arbitrary ': r@
-- will be the index of @t@ in @r@, plus 1.
instance MemberAt n t r => MemberAt ('S n) t (x ': r) where

-- | Easy way to have multiple @'Member'@s in a single effect.
--
-- > 'Members' '[A,B,C] r = ('Member' A r, 'Member' B r, 'Member' C r)
type family Members m r :: Constraint where
  Members (t ': c) r = (Member t r, Members c r)
  Members '[] r = ()

-- | Reify a @'Nat'@ into a @'Word'@. Will overflow with large @'Nat'@s.
class WordFor (n :: Nat) where
  wordFor :: Word

-- | @'Z' = 0@
instance WordFor 'Z where
  wordFor = 0

-- | @'S' n = succ n@
instance WordFor n => WordFor ('S n) where
  wordFor = succ (wordFor @n)

-- | The constraint @'Member' t r@ means that @t@ is in @r@ at the index @'Find' t r@,
-- and that you can reify that index into a @'Word'@ with @'wordFor' \@('Find' t r)@.
--
-- This is a compile-time computation without run-time overhead because everything
-- here takes place on the type level.
type Member t r = (MemberAt (Find t r) t r,WordFor (Find t r))

-- | Base case. If @t@ is at the head of @r@, then the index is 0.
--instance Member t (t ': tail'r) where
  --elemNo = 0

-- | Inductive case. Read: @t@ being a @'Member'@ of @r@ __implies that__
-- @t@ is also a @'Member'@ of the union of some @arbitrary@ value and @r@. 
-- The proof of this statement is that the index of @t@ in @arbitrary ': r@
-- will be the index of @t@ in @r@, plus 1.
--instance {-# OVERLAPPABLE #-} Member t r => Member t (arbitrary ': r) where
  --elemNo = 1 + elemNo @t @r

-- | If we have proof that @t@ is a @'Member'@ of @r@ at the index @'elemNo'@,
-- then we can safely make a @'Union' r a@ out of a @t a@ by using the index.
inj :: forall t r a. Member t r => t a -> Union r a
inj = unsafeInj (wordFor @(Find t r))

-- | If we have proof that @t@ is a @'Member'@ of @r@ at the index @'elemNo'@,
-- then we can attempt to project the actual inhabitant of type @t a@ from a
-- @'Union' r a@. 
--
-- Note that @t@ being in @r@ doesn't mean that the actual inhabitant of any given
-- @'Union' r a@ is a @t a@. It just means that it /might/ be, which is why
-- the return type of @'prj'@ is @'Maybe' (t a)@ and not @t a@.
prj :: forall t r a. Member t r => Union r a -> Maybe (t a)
prj = unsafePrj (wordFor @(Find t r))

-- | A @'Union'@ is like a nested @'Either'@, so we can peel back
-- one layer of @'Either'@ at a time to get @'Either'@ a reduced
-- @'Union'@ (without @t@ in it at all), or the actual inhabitant
-- if @t@ was the head of @r@.
--
-- Note that this is a very general case analysis function for
-- @'Union'@, so you can it to implement almost any other function involving
-- @'Union'@.
--
-- /O(1)/
--
-- TODO: A more elegant implementation
decomp :: Union (t ': r) a -> Either (Union r a) (t a)
decomp (Union 0 a) = Right $ unsafeCoerce a
decomp (Union n a) = Left  $ Union (n - 1) a
{-# INLINE [2] decomp #-}

-- | Specialized version of 'decomp' for efficiency.
--
-- /O(1)/
--
-- TODO: Check that it actually adds on efficiency.
decomp0 :: Union '[t] a -> Either (Union '[] a) (t a)
decomp0 (Union _ a) = Right $ unsafeCoerce a
{-# INLINE decomp0 #-}
{-# RULES "decomp/singleton"  decomp = decomp0 #-}

-- | A @'Union' '[t] a@ has only one possible type for the actual inhabitant:
-- @t a@. It's like having an @'Either' Void (t a)@, since there are no values
-- for @'Union' '[] a@.
--
-- /O(1)/
extract :: Union '[t] a -> t a
extract (Union _ a) = unsafeCoerce a
{-# INLINE extract #-}

-- | @'Weakens' q@ means we can weaken a @'Union'@ by prepending @q@ to it.
class Weakens q where
  -- | How to perform the weakening
  weakens :: Union r a -> Union (q ++ r) a

-- | Base case: A @'Union'@ can be weakened by an empty list by simply returning
-- the same @'Union'@ as we started with.
instance Weakens '[] where
  weakens = id

-- | Inductive case: If a @'Union'@ can be weakened by @xs@, then it can also
-- be weakened by @(x ': xs)@.
instance Weakens xs => Weakens (x ': xs) where
  -- Weaken as required for @xs@, the once more for @x@.
  weakens = weaken . weakens @xs

-- | We can add an additonal possible type constructor to any @'Union' r a@.
-- The actual inhabitant is still in @r@, of course.
--
-- /O(1)/
weaken :: Union r a -> Union (arbitrary ': r) a
weaken (Union n a) = Union (n + 1) a
{-# INLINE weaken #-}
