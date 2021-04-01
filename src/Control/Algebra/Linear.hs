{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Control.Algebra.Linear where

import Prelude ()
import Prelude.Linear
import Control.Functor.Linear qualified as C
import Control.Algebra.Linear.Handler
import Data.Kind

class C.Monad m => Algebra sig m | m -> sig where
  alg :: C.Functor ctx => Handler ctx n m -> sig n a -> ctx () %1 -> m (ctx a)

data Reader r m k where
  Ask   ::                             Reader r m r
  Local :: (r %1 -> r) %1 -> m a %1 -> Reader r m a

ourRunReader :: Dupable r => r %1 -> C.ReaderT r m a %1 -> m (a, r)
ourRunReader env r = let (a, b) = dup env in fmap (a,) (C.runReaderT r b)

instance (Dupable r, Algebra sig m) => Algebra (Reader r :+: sig) (C.ReaderT r m) where
  alg hdl sig ctx = case sig of
    L Ask         -> C.asks (C.<$ ctx)
    L (Local f m) -> C.local f (hdl (m C.<$ ctx))
    R other       -> C.ReaderT (\r -> alg ((uncurry ourRunReader r) . hdl) other ctx)


data (f :+: g) (m :: Type -> Type) k where
  L :: f m k %1 -> (f :+: g) m k
  R :: g m k %1 -> (f :+: g) m k

infixr 4 :+:
