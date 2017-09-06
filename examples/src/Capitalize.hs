{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module Capitalize
  (Capitalize
  ,capitalize
  ,runCapitalize
  ) where

import Data.Char (toUpper)

import Control.Monad.Freer


data Capitalize v where
    Capitalize :: String -> Capitalize String

capitalize :: Member Capitalize r => String -> Eff r String
capitalize = send . Capitalize

runCapitalize :: Eff (Capitalize ': r) w -> Eff r w
runCapitalize = runNat $ \case
  Capitalize s -> pure (map toUpper s)

--runCapitalize' :: Eff (Capitalize ': r) w -> Eff r w
--runCapitalize' = handleRelay pure $ \k -> \case
  --(Capitalize s) -> k (map toUpper s)
