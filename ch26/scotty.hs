-- scotty.hs

{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Web.Scotty
import Web.Scotty.Internal.Types
 (ActionT(..))

import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
  hiding (get)
import Data.Monoid (mconcat)

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (ActionT
      . (ExceptT . liftM Right)
      . ReaderT . const
      . \m -> StateT (\s -> do
                         a <- m
                         return (a, s))
     ) (putStrLn "hello")
    html $
      mconcat ["<h1>Scotty, " ,
               beam,
               " me up!</h1>"]
