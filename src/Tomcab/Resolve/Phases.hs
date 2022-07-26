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

  -- * UnsetFrom
  UnsetFrom,
  Unset,
  unset,
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

{----- UnsetFrom -----}

{- |
A helper for fields that are unset from the given phase on

e.g. @foo :: UnsetFrom 'NoAutoImports phase Text@ means the field @foo@ will have
type 'Text' up to 'NoAutoImports', then 'Unset' starting on 'NoAutoImports' on.
-}
type family UnsetFrom (phaseRef :: ResolutionPhase) (phase :: ResolutionPhase) a where
-- Parsed
  UnsetFrom 'Parsed _ _ = Unset
-- ResolvedOptionals
  UnsetFrom 'ResolvedOptionals 'Parsed a = a
  UnsetFrom 'ResolvedOptionals _ _ = Unset
-- NoAutoImports
  UnsetFrom 'NoAutoImports 'Parsed a = a
  UnsetFrom 'NoAutoImports 'ResolvedOptionals a = a
  UnsetFrom 'NoAutoImports _ _ = Unset
-- NoImports
  UnsetFrom 'NoImports 'Parsed a = a
  UnsetFrom 'NoImports 'ResolvedOptionals a = a
  UnsetFrom 'NoImports 'NoAutoImports a = a
  UnsetFrom 'NoImports _ _ = Unset
-- NoModulePatterns
  UnsetFrom 'NoModulePatterns 'Parsed a = a
  UnsetFrom 'NoModulePatterns 'ResolvedOptionals a = a
  UnsetFrom 'NoModulePatterns 'NoAutoImports a = a
  UnsetFrom 'NoModulePatterns 'NoImports a = a
  UnsetFrom 'NoModulePatterns _ _ = Unset

data Unset

instance Show Unset where
  show _ = "<unset>"

unset :: Unset
unset = error "Trying to evaluate an unset field"
