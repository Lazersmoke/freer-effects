{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Freer effects utilities:
--
-- * 'Eff' data type, for expressing effects.
-- * Functions for facilitating the construction of effects and their handlers.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer
  (
  -- * Effect Monad
  Eff(..)
  ,Arrs
  -- ** Open Union
  --
  -- | Open Union (type-indexed co-product) of effects.
  ,module Data.OpenUnion
  -- ** Fast Type-aligned Queue
  --
  -- | Fast type-aligned queue optimized to effectful functions of type
  -- @(a -> m b)@.
  ,module Data.FTCQueue
  -- ** Sending Arbitrary Effect
  ,send
  -- ** Lifting Effect Stacks
  ,raise

  -- * Handling Effects
  ,run
  ,runM
  -- ** Building Effect Handlers
  ,handleRelay
  ,handleRelayS
  ,interpose
  ,replaceRelay
  ,replaceRelayS
  ,runNat
  ,runNatS
  -- *** Low-level Functions for Building Effect Handlers
  ,linearize
  ,cataEff
  ,cataEffMember
  ,prodEff
  ,unEff
  )
  where

import Data.Function (fix)

import Data.FTCQueue
import Data.OpenUnion

-- | An efficient representation of a function that will take an @a@ to a @b@ while
-- doing some effects from @effs@. This is isomorphic to a normal effectful arrow,
-- as witnessed by @'linearize'@.
type Arrs r a b = FTCQueue (Eff r) a b

-- | An @'Eff' r a@ is an action that does some of the effects described in
-- @r@, then returns an @a@. @'Eff'@s can be composed using the @'Functor'@,
-- @'Applicative'@, and @'Monad'@ instances, provided they are all have the same
-- @r@.
data Eff r a
  = Pure a 
  -- ^ Just return a result; don't do any effects.
  | forall x. Eff (Arrs r x a) (Union r x)
  -- ^ Do one of the effects in @r@, which will return an @x@,
  -- then do the continuation, which might do more effects to turn that
  -- @x@ into an @a@.

-- | CPS case analysis for @'Eff'@, up to its constructors.
unEff :: (a -> q) -> (forall x. (x -> Eff r a) -> Union r x -> q) -> Eff r a -> q
unEff p e = \case
  Pure a -> p a
  Eff q u -> e (linearize q) u

-- | CPS case analysis for @'Eff'@, plus CPS case analysis for @'Union'@.
-- This is a very general combinator, and most other @'Eff'@ consumption
-- functions are written in terms of it.
--
-- While this may appear to be not fully general at first glance, because it throws
-- out the @'Union'@ in the @'Eff'@ constructor after @'decomp'@osing it, it is
-- actually recoverable by injection in the first case, or weakening in the second.
cataEff
  :: (a -> q) -- ^ Handle the @'Pure'@ case
  -> (forall x. (x -> Eff (e ': es) a) -> e x -> q) -- ^ Handle the @'Eff'@/effect case
  -> (forall x. (x -> Eff (e ': es) a) -> Union es x -> q) -- ^ Handle the @'Eff'@/union case
  -> Eff (e ': es) a 
  -> q
cataEff p h uf = unEff p $ \q u -> case decomp u of
  Right ex -> h q ex
  Left u' -> uf q u'

prodEff
  :: Member e es 
  => (a -> q) -- ^ Handle the @'Pure'@ case
  -> (forall x. (x -> Eff es a) -> e x -> q) -- ^ Handle the @'Eff'@/effect case
  -> (forall x. (x -> Eff es a) -> Union (Delete e es) x -> q) -- ^ Handle the @'Eff'@/union case
  -> Eff es a 
  -> q
prodEff p h uf = unEff p $ \q u -> case prod u of
  Right ex -> h q ex
  Left u' -> uf q u'

-- | A version of @'cataEff'@ for working with a @'Member'@ constraint. It uses @'prj'@
-- instead of @'decomp'@ to use the additional information in the @'Member'@
-- dictionary.
cataEffMember
  :: Member t es
  => (a -> q) -- ^ Handle the @'Pure'@ case
  -> (forall x. (x -> Eff es a) -> t x -> q) -- ^ Handle the @'Eff'@/effect case
  -> (forall x. (x -> Eff es a) -> Union es x -> q) -- ^ Handle the @'Eff'@/union case
  -> Eff es a
  -> q
cataEffMember p e uf = unEff p $ \q u -> case prj u of
  Just tx -> e q tx
  Nothing -> uf q u


-- | Turn an efficient representation of an effectful function into a normal
-- representation of an effectful function. It's called @'linearize'@ because
-- it converts from an efficient tree to a linear function application.
linearize :: Arrs r a b -> a -> Eff r b
linearize q' x = case tviewl q' of
  TOne k  -> k x
  k :| t -> unEff (linearize t) (\q -> Eff (singleton q >< t)) (k x)

instance Functor (Eff r) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Eff q u) = Eff (q |> (Pure . f)) u
  {-# INLINE fmap #-}

instance Applicative (Eff r) where
  pure = Pure
  {-# INLINE pure #-}

  -- An Applicative law!
  Pure f <*> e = f <$> e
  Eff q u <*> m   = Eff (q |> (<$> m)) u
  {-# INLINE (<*>) #-}

instance Monad (Eff r) where
  -- Future versions of GHC will consider any other definition as error.
  return = pure
  {-# INLINE return #-}

  Pure x >>= k = k x
  Eff q u >>= k = Eff (q |> k) u
  {-# INLINE (>>=) #-}

-- | Create an effectful action that can be composed easily from a single 
-- effectful action. This is the most common way to create an @'Eff'@.
send :: Member eff r => eff a -> Eff r a
send = Eff (singleton Pure) . inj

-- | An @'Eff' '[] a@ is an effectful action without any effects, which
-- means it is just a @'Pure'@ value, so we can extract that value for free.
--
-- Since @'Union' '[] a@ is uninhabitable, it's impossible to have
-- an @'Eff' '[] a@ that /isn't/ @'Pure'@. This means @'run'@ is total
-- despite only matching the @'Pure'@ case.
--
-- Typical usage:
--
-- > 'run' . runEff1 . runEff2 $ someProgram
run :: Eff '[] a -> a
run (Pure x) = x
run _     = error "Internal:run - This (Eff) should never happen"

-- | An @'Eff' '[m] a@, where @m@ is a @'Monad'@ can be converted to an @m a@ by
-- replacing @'Pure'@ with @'return'@, and @'E'@ with @'>>='@ (plus some plumbing).
-- This is useful for plugging in traditional transformer stacks.
--
-- Typical usage:
--
-- > 'runM' . convertAToIO $ (someProgram :: 'Eff' '[A] a)
runM :: Monad m => Eff '[m] a -> m a
runM = fix $ \y -> unEff
  pure
  (\q u -> extract u >>= y . q)


-- | Just like @'replaceRelay'@, but this time you can pass some state along
replaceRelayS
  :: (s -> a -> Eff (v ': effs) w)
  -> (forall x. s -> (s -> x -> Eff (v ': effs) w) -> t x -> Eff (v ': effs) w)
  -> s
  -> Eff (t ': effs) a
  -> Eff (v ': effs) w
replaceRelayS p h = fix $ \y -> \s -> cataEff
  (p s)
  (\q -> h s (\s' -> y s' . q))
  (\q u -> Eff (singleton $ y s . q) (weaken u))

-- | Convert one effect @e@ to another effect @f@ at the top of the effect stack.
-- You can also modify the return type.
replaceRelay
  :: (a -> Eff (f ': es) b) -- ^ Convert @'Pure'@
  -> (forall x. (x -> Eff (f ': es) b) -> e x -> Eff (f ': es) b)
  -- ^ Convert @'Eff'@
  -> Eff (e ': es) a
  -> Eff (f ': es) b
replaceRelay = consumeEffect weaken
--replaceRelay p h = fix $ \y -> cataEff
  --p
  --(\ex q -> h ex (y . q))
  --(\u q -> Eff (weaken u) (singleton $ y . q))

-- ^ Consume an effect of type @e@ by handling every possible case.
consumeEffect
  :: (forall x. Union es x -> Union es' x) 
  -- ^ Convert a @'Union'@ of the remaning effects to a @'Union'@ of the final effects.
  -> (a -> Eff es' b) 
  -- ^ Convert a @'Pure'@ value to an @'Eff'@ of the final effects.
  -> (forall x. (x -> Eff es' b) -> e x -> Eff es' b) 
  -- ^ Convert an effect that we are consuming into an @'Eff'@ of the final effects,
  -- given a continuation.
  -> Eff (e ': es) a -- ^ The input @'Eff'@, with the effect we are consuming.
  -> Eff es' b
consumeEffect uf p h = fix $ \y -> cataEff
  p
  (h . (y .))
  (\q -> Eff (singleton $ y . q) . uf)

-- | Handle (remove) a single effect (@e@) by providing handling functions for it in terms of the remaining effects (@es@).
-- You are also allowed to change the resulting return type from @a@ to @b@, and to introduce effects in @es@
-- for @'Pure'@ values in @e@.
--
-- Alternate definition in terms of @'handleRelayS'@, with the state as @()@:
--
-- > 'handleRelay' p h = 'handleRelayS' (\() -> p) (\() ex q -> h ex (q ())) ()
handleRelay
  :: (a -> Eff es b) -- ^ Handle a @'Pure'@
  -> (forall x. (x -> Eff es b) -> e x -> Eff es b) -- ^ Handle an effect, given a continuation
  -> Eff (e ': es) a -- ^ The input effect, including the @e@ we are handling.
  -> Eff es b -- ^ The output effect, without the @e@ we just handled.
handleRelay = consumeEffect id
--handleRelay p h = fix $ \y -> cataEff 
  --p 
  --(\ex q -> h ex (y . q)) 
  --(\u q -> Eff u (singleton $ y . q))


-- | Just like @'handleRelay'@, but this time you can pass some state
-- along as well as just recurse.
handleRelayS
  :: (s -> a -> Eff es b) -- ^ Handle a @'Pure'@, parameterized by the state.
  -> (forall x. s -> (s -> x -> Eff es b) -> e x -> Eff es b)
  -> s -- ^ Initial state
  -- ^ Handle an effect, parameterized by the state, and given a continuation.
  -- You should pass the updated state to the continuation.
  -> Eff (e ': es) a -- ^ The input effect, with the @e@ we are handling
  -> Eff es b -- ^ The output effect, without the @e@ we just handled
handleRelayS p h = fix $ \y -> \s -> cataEff 
  (p s) 
  (\q -> h s (\s' -> y s' . q)) 
  (\q -> Eff (singleton $ y s . q))

-- | Variant of 'handleRelay' simplified for the common case where you don't modify
-- pure values, and you simply bind effects together instead of doing other things with them
runNat 
  :: (forall x. e x -> Eff es x) -- ^ Natural transformation from @e@ to @'Eff' es (s,-)@, parameterized by state
  -> Eff (e ': es) a -- ^ The input effect, with the @e@ we are transforming
  -> Eff es a -- ^ The output effect, without the @e@ we just transformed
runNat f = handleRelay pure (\q ex -> q =<< f ex)

-- | Just like @'runNat'@, but this time you can pass some state along
runNatS
  :: (forall a. s -> e a -> Eff es (s, a)) -- ^ Natural transformation from @e@ to @'Eff' es (s,-)@, parameterized by state
  -> s -- ^ Initial state
  -> Eff (e ': es) b -- ^ The input effect, with the @e@ we are transforming
  -> Eff es b -- ^ The output effect, without the @e@ we just transformed
runNatS f = handleRelayS 
  (\_s -> pure) 
  (\s' q e -> (f s' e >>=) . uncurry $ q)

-- | Intercept the request and possibly reply to it, but leave it unhandled.
-- This is black magic, with no obvious use case, but I'm leaving it here anyway.
interpose
  :: Member e es
  => (a -> Eff es b)
  -> (forall x. (x -> Eff es b) -> e x -> Eff es b)
  -> Eff es a
  -> Eff es b
interpose p h = fix $ \y -> cataEffMember
  p
  (h . (y .))
  (\q u -> Eff (singleton $ y . q) u)

-- | Inserts a bogus effect that will never be called into an @'Eff'@ effect list.
raise :: Eff effs a -> Eff (any ': effs) a
raise = onUnion weaken

onUnion :: (forall x. Union e x -> Union f x) -> Eff e a -> Eff f a
onUnion f = unEff
  pure
  (\q u -> Eff (singleton $ onUnion f . q) (f u))

