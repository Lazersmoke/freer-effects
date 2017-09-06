{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
--
-- TODO: Remove once GHC can deduce the decidability of this instance.
{-# LANGUAGE UndecidableInstances #-}

-- | Internal machinery for this effects library. This includes:
--
-- * 'Eff' data type, for expressing effects.
-- * 'NonDet' data type, for nondeterministic effects.
-- * Functions for facilitating the construction of effects and their handlers.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Internal
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
  -- ** Nondeterminism Effect
  ,NonDet(..)
  )
  where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))

import Data.FTCQueue
import Data.OpenUnion

-- | An efficient representation of a function that will take an @a@ to a @b@ while
-- doing some effects from @effs@. This is isomorphic to a normal effectful arrow,
-- as witnessed by @'linearize'@.
type Arrs effs a b = FTCQueue (Eff effs) a b

-- | An @'Eff' effs a@ is an action that does some of the effects described in
-- @effs@, then returns an @a@. @'Eff'@s can be composed using the @'Functor'@,
-- @'Applicative'@, and @'Monad'@ instances, provided they are all have the same
-- @effs@.
data Eff effs a
  = Pure a 
  -- ^ Just return a result; don't do any effects.
  | forall x. Eff (Union effs x) (Arrs effs x a)
  -- ^ Do one of the effects in @effs@, which will return an @x@,
  -- then do the continuation, which might do more effects to turn that
  -- @x@ into an @a@.

-- | CPS case analysis for @'Eff'@, up to its constructors.
unEff :: (a -> q) -> (forall x. Union r x -> (x -> Eff r a) -> q) -> Eff r a -> q
unEff p e = \case
  Pure a -> p a
  Eff u q -> e u (linearize q)

-- | CPS case analysis for @'Eff'@, plus CPS case analysis for @'Union'@.
-- This is a /fully general/ combinator, and all other @'Eff'@ consumption
-- functions are written in terms of it, unless they need a @'Member'@ constraint.
-- See @'cataEffMember'@ for details.
--
-- While this may appear to be not fully general at first glance, because it throws
-- out the @'Union'@ in the @'Eff'@ constructor after @'decomp'@osing it, it is
-- actually recoverable by injection in the first case, or weakening in the second.
cataEff
  :: (a -> q) -- ^ Handle the @'Pure'@ case
  -> (forall x. e x -> (x -> Eff (e ': es) a) -> q) -- ^ Handle the @'Eff'@/effect case
  -> (forall x. Union es x -> (x -> Eff (e ': es) a) -> q) -- ^ Hanlde the @'Eff'@/union case
  -> Eff (e ': es) a 
  -> q
cataEff p e uf = unEff p $ \u q -> case decomp u of
  Right ex -> e ex q
  Left u' -> uf u' q

-- | A version of @'cataEff'@ for working with a member constraint. It uses @'prj'@
-- instead of @'decomp'@ to use the additional information in the @'Member'@
-- dictionary.
cataEffMember
  :: Member t es
  => (a -> q) -- ^ Handle the @'Pure'@ case
  -> (forall x. t x -> (x -> Eff es a) -> q) -- ^ Handle the @'Eff'@/effect case
  -> (forall x. Union es x -> (x -> Eff es a) -> q) -- ^ Handle the @'Eff'@/union case
  -> Eff es a
  -> q
cataEffMember p e uf = unEff p $ \u q -> case prj u of
  Just tx -> e tx q
  Nothing -> uf u q


-- | Turn an efficient representation of an effectful function into a normal
-- representation of an effectful function.
linearize :: Arrs effs a b -> a -> Eff effs b
linearize q' x = case tviewl q' of
  TOne k  -> k x
  k :| t -> case k x of
    Pure y -> linearize t y
    Eff u q -> Eff u (q >< t)

instance Functor (Eff effs) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Eff u q) = Eff u (q |> (Pure . f))
  {-# INLINE fmap #-}

instance Applicative (Eff effs) where
  pure = Pure
  {-# INLINE pure #-}

  Pure f <*> Pure x = Pure $ f x
  Pure f <*> Eff u q = Eff u (q |> (Pure . f))
  Eff u q <*> m   = Eff u (q |> (`fmap` m))
  {-# INLINE (<*>) #-}

instance Monad (Eff effs) where
  -- Future versions of GHC will consider any other definition as error.
  return = pure
  {-# INLINE return #-}

  Pure x >>= k = k x
  Eff u q >>= k = Eff u (q |> k)
  {-# INLINE (>>=) #-}

-- | Create an effectful action that can be composed easily from a single 
-- effectful action. This is the most common way to create an @'Eff'@.
send :: Member eff effs => eff a -> Eff effs a
send t = Eff (inj t) (singleton Pure)

-- | An @'Eff' '[] a@ is an effectful action without any effects, which
-- means it is just a @'Pure'@ value, so we can extract that value for free.
--
-- Technically, since @'Union' '[] a@ is uninhabitable, it's impossible to have
-- an @'Eff' '[] a@ that /isn't/ @'Pure'@.
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
runM (Pure x) = return x
runM (Eff u q) = case extract u of
  mb -> mb >>= runM . linearize q
  -- The other case is unreachable since Union '[] a cannot be constructed.
  -- Therefore, run is a total function if its argument terminates.

-- | Like 'replaceRelay', but with support for an explicit state to help
-- implement the interpreter.
replaceRelayS
  :: s
  -> (s -> a -> Eff (v ': effs) w)
  -> (forall x. s -> t x -> (s -> x -> Eff (v ': effs) w) -> Eff (v ': effs) w)
  -> Eff (t ': effs) a
  -> Eff (v ': effs) w
replaceRelayS s' pure' bind = loop s'
  where
    loop s (Pure x)  = pure' s x
    loop s (Eff u' q) = case decomp u' of
      Right x -> bind s x k
      Left  u -> Eff (weaken u) (singleton (k s))
      where
        k s'' x = loop s'' $ linearize q x

-- | Interpret an effect by transforming it into another effect on top of the
-- stack. The primary use case of this function is allow interpreters to be
-- defined in terms of other ones without leaking intermediary implementation
-- details through the type signature.
replaceRelay
  :: (inA -> Eff (outEff ': effs) outA)
  -- ^ Provide a custom pure
  -> (forall x. inEff x -> (x -> Eff (outEff ': effs) outA) -> Eff (outEff ': effs) outA)
  -- ^ Provide a custom bind
  -> Eff (inEff ': effs) inA
  -> Eff (outEff ': effs) outA
replaceRelay pure' bind = loop
  where
    loop (Pure x)  = pure' x
    loop (Eff u' q) = case decomp u' of
      Right x -> bind x k
      Left  u -> Eff (weaken u) (singleton k)
      where
        k = loop . linearize q

-- | Handle (remove) a single effect (@e@) by providing handling functions for it in terms of the remaining effects (@es@).
-- You are also allowed to change the resulting return type from @a@ to @b@, and to introduce effects in @es@
-- for @'Pure'@ values in @e@.
--
-- Alternate definition in terms of @'handleRelayS'@, with the state as @()@:
--
-- > 'handleRelay' ret h = 'handleRelayS' () (\() -> ret) (\() ex q -> h ex (q ()))
handleRelay
  :: (a -> Eff es b) -- ^ Introduce an effect for @'Pure'@ values
  -> (forall x. e x -> (x -> Eff es b) -> Eff es b) -- ^ What to do about effects, given a continuation
  -> Eff (e ': es) a -- ^ The input effect, including the @e@ we are handling.
  -> Eff es b -- ^ The output effect, without the @e@ we just handled.
handleRelay ret h = cataEff 
  ret 
  (\ex q -> h ex (handleRelay ret h . q)) 
  (\u q -> Eff u (singleton $ handleRelay ret h . q))


-- | Just like @'handleRelay'@, but this time you can pass some state
-- along as well as just recurse.
handleRelayS
  :: s -- ^ Initial state
  -> (s -> a -> Eff es b) -- ^ Handle a @'Pure'@, parameterized by the state.
  -> (forall x. s -> e x -> (s -> x -> Eff es b) -> Eff es b)
  -- ^ Handle an effect, parameterized by the state, and given a continuation.
  -- You should pass the updated state to the continuation.
  -> Eff (e ': es) a -- ^ The input effect, with the @e@ we are handling
  -> Eff es b -- ^ The output effect, without the @e@ we just handled
handleRelayS s ret h = cataEff 
  (ret s) 
  (\ex q -> h s ex (\s' -> handleRelayS s' ret h . q)) 
  (\u q -> Eff u (singleton $ handleRelayS s ret h . q))

-- | Variant of 'handleRelay' simplified for the common case where you don't modify
-- pure values, and you simply bind effects together instead of 
runNat :: (forall a. e a -> Eff es a) -> Eff (e ': es) b -> Eff es b
runNat f = handleRelay pure ((>>=) . f)

-- | Just like @'runNat'@, but this time you can pass some state along
runNatS
  :: s -- ^ Initial state
  -> (forall a. s -> e a -> Eff es (s, a)) -- ^ Natural transformation from @e@ to @'Eff' es (s,-)@, parameterized by state
  -> Eff (e ': es) b -- ^ The input effect, with the @e@ we are transforming
  -> Eff es b -- ^ The output effect, without the @e@ we just transformed
runNatS s f = handleRelayS s (const pure) $ \s' e -> (f s' e >>=) . uncurry

-- | Intercept the request and possibly reply to it, but leave it unhandled.
-- This is black magic, with no obvious use case, but I'm leaving it here anyway.
interpose
  :: Member e es
  => (a -> Eff es b)
  -> (forall x. e x -> (x -> Eff es b) -> Eff es b)
  -> Eff es a
  -> Eff es b
interpose ret h = cataEffMember
  ret
  (\ex q -> h ex (interpose ret h . q))
  (\u q -> Eff u (singleton $ interpose ret h . q))

-- | Embeds a less-constrained 'Eff' into a more-constrained one. Analogous to
-- MTL's 'lift'.
raise :: Eff effs a -> Eff (e ': effs) a
raise = unEff
  pure
  (\u q -> Eff (weaken u) (singleton $ raise . q))

--------------------------------------------------------------------------------
          -- Nondeterministic Choice --
--------------------------------------------------------------------------------

-- | A data type for representing nondeterminstic choice.
data NonDet a where
  MZero :: NonDet a
  MPlus :: NonDet Bool

instance Member NonDet effs => Alternative (Eff effs) where
  empty = mzero
  (<|>) = mplus

instance Member NonDet effs => MonadPlus (Eff effs) where
  mzero     = send MZero
  mplus m1 m2 = send MPlus >>= \x -> if x then m1 else m2
