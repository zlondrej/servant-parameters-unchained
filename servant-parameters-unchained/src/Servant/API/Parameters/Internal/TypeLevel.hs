{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.Parameters.Internal.TypeLevel where

import Data.Kind
import GHC.TypeError
import Data.Type.Bool

-- | Requires that the type is one of the types in the list.
type OneOf (ts :: [Type]) a = If (IsElem a ts)
  (() :: Constraint)
  (TypeError ('Text "Type " :<>: 'ShowType a :<>: 'Text " is not one of the supported types: " :<>: 'ShowType ts))

type family IsElem (t :: Type) (ts :: [Type]) :: Bool where
  IsElem t '[] = 'False
  IsElem t (t : ts) = 'True
  IsElem t (t1 : ts) = IsElem t ts

-- | Applies type parameter to a list of type-level functions.
type family Apply (filters :: [Type -> Type]) (t :: Type) :: [Type] where
  Apply '[] t = '[]
  Apply (f : fs) t = f t : Apply fs t

type family IsSubsetOf (sub :: [Type]) (super :: [Type]) :: Bool where
  IsSubsetOf '[] _ = 'True
  IsSubsetOf (s : ss) ts = IsElem s ts && IsSubsetOf ss ts

-- | Requires that the first list is a subset of the second list.
type family SubsetOf (sub :: [Type]) (super :: [Type]) :: Constraint where
  SubsetOf sub super = If (IsSubsetOf sub super)
    (() :: Constraint)
    (TypeError ('ShowType sub :<>: 'Text " is not a subset of " :<>: 'ShowType super))

type family IsUnique (ts :: [Type]) :: Bool where
  IsUnique '[] = 'True
  IsUnique (t : ts) = (Not (IsElem t ts)) && IsUnique ts

type family Unique (ts :: [Type]) :: Constraint where
  Unique ts = If (IsUnique ts)
    (() :: Constraint)
    (TypeError ('Text "Types in the list are not unique: " :<>: 'ShowType ts))
