{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.Parameters.Internal.TypeLevel where

import Data.Kind
import GHC.TypeError

-- | Requires that the type is one of the types in the list.
type OneOf (ts :: [Type]) a = OneOf' ts ts a ~ a

-- | Implementation of `OneOf`.
type family OneOf' (ts :: [Type]) (init :: [Type]) a :: Type where
  OneOf' '[] init a =
    TypeError
      ( ('Text "No matching type: " :<>: 'ShowType a)
          ':$$: ('Text "Supported types: " :<>: 'ShowType init)
      )
  OneOf' (a : ts) init a = a
  OneOf' (t : ts) init a = OneOf' (t : ts) init a

-- | Applies type parameter to a list of type-level functions.
type family Apply (filters :: [Type -> Type]) (t :: Type) :: [Type] where
  Apply '[] t = '[]
  Apply (f : fs) t = f t : Apply fs t
