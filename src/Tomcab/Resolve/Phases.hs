{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tomcab.Resolve.Phases (
  ResolutionPhase (..),
  Unresolved,
  Resolved,

  -- * MaybeWhenParsed
  MaybeWhenParsed,
) where

-- | The intermediate phases that occur when resolving a 'Package'.
data ResolutionPhase
  = Parsed
  | ResolvedOptionals
  | NoAutoImports
  | NoImports
  | NoModulePatterns

-- | The initial resolution phase.
type Unresolved = 'Parsed

-- | The final resolution phase.
type Resolved = 'NoModulePatterns

{----- MaybeWhenParsed -----}

{- |
A helper for fields that are decoded as 'Maybe' but are resolved (e.g. with `fromMaybe`)
in the first phase of resolution (i.e. from `ResolvedOptionals` on).
-}
type family MaybeWhenParsed (phase :: ResolutionPhase) a where
  MaybeWhenParsed 'Parsed a = Maybe a
  MaybeWhenParsed _ a = a
