{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class



-- 26.2 MaybeT
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT mMa) = MaybeT $ (fmap . fmap) f mMa

instance Applicative m => Applicative (MaybeT m) where
  pure a = MaybeT $ (pure . pure) a
  MaybeT mMf <*> MaybeT mMa = MaybeT $ (<*>) <$> mMf <*> mMa

instance Monad m => Monad (MaybeT m) where
  return = pure
  MaybeT mMa >>= f = MaybeT $ do
    ma <- mMa
    case ma of
      Nothing -> pure Nothing
      Just x -> runMaybeT (f x)

-- 26.3 EitherT
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- 1
instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mEea) = EitherT $ (fmap .fmap) f mEea

-- 2
instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ (pure . pure) a
  EitherT emf <*> EitherT ema = EitherT $ (<*>) <$> emf <*> ema

-- 3
instance Monad m => Monad (EitherT e m) where
  return = pure
  EitherT ema >>= f = EitherT $ do
    v <- ema
    case v of
      Left e -> pure (Left e)
      Right a -> runEitherT (f a)

-- 4
swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mEea) = EitherT $ swapEither <$> mEea

-- 5
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa gb (EitherT ab) = do
  v <- ab
  case v of
    Left a -> fa a
    Right b -> gb b

-- 26.4 ReaderT
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ \r -> fmap f (rma r)
  --                     ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT $ (pure . pure) a

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  ReaderT rmab <*> ReaderT rma = ReaderT $ \r -> rmab r <*> rma r
  --                             ReaderT $ (<*>) <$> rmab <*> rma

instance Monad m => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  ReaderT rma >>= f = ReaderT $ \r -> rma r >>= \a -> runReaderT (f a) r

-- 26.5 StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT smas) = StateT $ \s -> fmap (\(a, s') -> (f a, s')) $ smas s

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT smfs <*> StateT smas = StateT $ \s -> do
    (f, s') <- smfs s
    (a, s'') <- smas s'
    pure (f a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sma >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'

-- 26.8 Lexically inner is structurally outer
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = pure 1
           -- We only need to use return once because it's one big Monad

unwrapMaybe :: ExceptT String (ReaderT () IO) (Maybe Int)
unwrapMaybe = runMaybeT embedded

unwrapExcept :: ReaderT () IO (Either String (Maybe Int))
unwrapExcept = runExceptT unwrapMaybe

unwrapReader :: () -> IO (Either String (Maybe Int))
unwrapReader = runReaderT unwrapExcept

-- Then if we’d like to evaluate this code, we feed the unit value to the
-- function:  unwrapReader ()
-- Right (Just 1)
-- So, while in the type signature the Maybe came first, in the result the
-- Just is the innermost value

-- Exercise: Wrap It Up
embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT . ExceptT . ReaderT $ const (pure (Right (Just 1)))

-- Exercise: Lift more
instance MonadTrans (EitherT e) where
  lift ma = EitherT $ Right <$> ma

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    pure (a, s)

instance MonadTrans MaybeT where
  lift ma = MaybeT $ Just <$> ma

instance MonadTrans (ReaderT r) where
  lift ma = ReaderT $ \r -> ma

-- Exercises: Some Instances
-- 1
instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO
-- 2
instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO
-- 3
instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO
