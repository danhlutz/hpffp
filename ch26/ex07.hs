-- ch26/ex07.hs
-- chapter exercises

module Ex where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

-- 1 & 2

rDec :: Num a => Reader a a
rDec = reader $ \x -> x - 1

rDec' :: Num a => Reader a a
rDec' = reader $ flip (-) 1

rDec'' :: Num a => Reader a a
rDec'' = ReaderT $ \x -> return (x - 1)

-- 3 & 4
rShow :: Show a
      => ReaderT a Identity String
rShow = ReaderT $ return . show

-- 5
rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
rPrintAndInc = ReaderT $ \x -> do
  putStrLn $ "Hi: " ++ (show x)
  return (x + 1)

-- 6
sPrintIncAccum :: (Num a, Show a)
               => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  putStrLn $ "Hi: " ++ (show s)
  return (show s, s + 1)
