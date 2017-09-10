{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | This the main module of @freer-effects@, and the one you will import most often.
--
-- This module provides:
--
-- * The @'Eff'@ type, representing an effectful computation
-- * Functions for writing effectful computations, like @'send'@
-- * Functions for creating effect handlers, like @'interpret'@
--
-- Generally, your code will look like this:
--
-- @
--   main :: IO ()
--   main = runM . runReader deafultConfig . evalState initialAppState $ app
--
--   app :: Members '[IO,Reader AppConfig,State AppState] r => Eff r ()
--   app = do
--     config <- ask -- Doing 'ask' here will get the AppConfig
--     send $ putStrLn $ "The foo option in the config is: " ++ show (foo config)
--     appState <- get -- Doing 'get' or 'put' here will access the AppState
--     send $ putStrLn "I'm in an effect :D" -- Prints the message
--     ...
--
--   defaultConfig :: AppConfig
--   defaultConfig = ...
--
--   initialAppState :: AppState
--   initialAppState = ...
-- @
module Control.Monad.Freer
  (
  -- * Building Effects
  Eff(..)
  -- ** Creating @'Eff'@s
  ,send
  -- ** Lifting Effect Stacks
  ,raise

  -- * Handling Effects
  ,run
  ,runM
  -- ** Building Effect Handlers
  ,interpret
  ,reinterpret
  ,handleRelay
  ,replaceRelay
  ,consumeEffect
  ,interpose
  -- *** Low-level Functions for Building Effect Handlers
  ,linearize
  ,unEff
  ,cataEff
  ,singleton
  ,(|>)

  -- * Open Union
  --
  -- | Provides @'Member'@
  ,module Data.OpenUnion
  )
  where

import Data.Function (fix)
import Control.Arrow

import Data.FTCQueue
import Data.OpenUnion


-- | An @'Eff' r a@ is an action that can perform the effects described in
-- @r@, and returns an @a@. @'Eff'@s can be composed using the @'Functor'@,
-- @'Applicative'@, and @'Monad'@ instances, provided they all have the same
-- @r@.
--
-- You should construct @'Eff'@s using @'send'@, not with the constructors directly.
-- They /are/ safe to use, just very annoying.
data Eff (r :: [* -> *]) (a :: *) where
  -- | Just return a result; don't do any effects.
  Pure :: a -> Eff r a
  -- | Do one of the effects in @r@, which will return an @x@,
  -- then do the continuation, which might do more effects to turn that
  -- @x@ into an @a@.
  Eff :: (FTCQueue (Kleisli (Eff r)) x a) -> (Union r x) -> Eff r a

-- | CPS case analysis for @'Eff'@, up to its constructors.
unEff :: (a -> q) -> (forall x. (x -> Eff r a) -> Union r x -> q) -> Eff r a -> q
unEff p e = \case
  Pure a -> p a
  Eff q u -> e (linearize q) u

-- | CPS case analysis for @'Eff'@, plus CPS case analysis for @'Union'@.
-- This is a very general combinator, and most other @'Eff'@ consumption
-- functions are written in terms of it.
cataEff
  :: (a -> q) -- ^ Handle the @'Pure'@ case
  -> (forall x. (x -> Eff (e ': es) a) -> e x -> q) -- ^ Handle the @'Eff'@/effect case
  -> (forall x. (x -> Eff (e ': es) a) -> Union es x -> q) -- ^ Handle the @'Eff'@/union case
  -> Eff (e ': es) a 
  -> q
cataEff p h uf = unEff p $ \q u -> case decomp u of
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
linearize :: FTCQueue (Kleisli (Eff r)) a b -> a -> Eff r b
linearize q' x = case viewl q' of
  One k -> runKleisli k x
  Cons k t -> unEff (linearize t) (\q -> Eff (singleton q >< t)) (runKleisli k x)

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

-- | Create an effectful action from a single effect action.
-- Notice that the type allows the result of @'send'@ to be composed
-- easily with other effects, since it only requires that the provided
-- effect be a member of the overall set of effects, but does not limit
-- the other effects that may be present.
send :: Member e r => e a -> Eff r a
send = Eff (singleton Pure) . inj

-- | Generalize an effect to work on a larger effect stack.
-- It does this by inserting a bogus effect that will never be used into the effect list.
raise :: Eff r a -> Eff (arbitrary ': r) a
raise = unEff 
  pure
  (\q u -> Eff (singleton $ raise . q) (weaken u))

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

-- | The simplest way to write an effect handler.
-- Given a way to rewrite a given effect @e@ in terms of the rest of the 
-- effects in the the list @es@, interpret @e@ in terms of @es@.
interpret 
  :: (forall x. e x -> Eff es x) -- ^ Natural transformation from @e@ to @'Eff' es@
  -> Eff (e ': es) a -- ^ The input effect, with the @e@ we are interpreting
  -> Eff es a -- ^ The output effect, without the @e@ we just interpreted
interpret n = handleRelay pure (\q ex -> q =<< n ex)

-- | A simple way to write an effect handler.
-- Given a way to rewrite an effect @e@ as an effect @f@, replace
-- @e@ with @f@ at the top of the effect stack.
reinterpret
  :: forall fs es e a. Weakens fs
  => (forall x. e x -> Eff (fs ++ es) x) -- ^ Natural transformation from @e@ to @'Eff' (fs ++ es) x@
  -> Eff (e ': es) a -- ^ The input effect, with the @e@ we are reinterpreting as @fs@
  -> Eff (fs ++ es) a -- ^ The output effect, with the @fs@ we just reinterpreted @e@ into
reinterpret n = replaceRelay @fs pure (\q ex -> q =<< n ex)

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

-- | Convert one effect @e@ to another effect @f@ at the top of the effect stack.
-- You can also modify the return type.
replaceRelay
  :: forall fs es e a b. Weakens fs
  => (a -> Eff (fs ++ es) b) -- ^ Convert @'Pure'@
  -> (forall x. (x -> Eff (fs ++ es) b) -> e x -> Eff (fs ++ es) b)
  -- ^ Convert @'Eff'@
  -> Eff (e ': es) a
  -> Eff (fs ++ es) b
replaceRelay = consumeEffect (weakens @fs)

-- | Consume an effect of type @e@ by handling every possible case.
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

-- | @'singleton''@ specialized to the correct type for working with @'Eff'@
singleton :: (a -> Eff r b) -> FTCQueue (Kleisli (Eff r)) a b
singleton = singleton' . Kleisli

-- | @'snoc'@ specialized to the correct type for working with @'Eff'@
(|>) :: FTCQueue (Kleisli (Eff r)) a x -> (x -> Eff r b) -> FTCQueue (Kleisli (Eff r)) a b
(|>) = (. Kleisli) . snoc
