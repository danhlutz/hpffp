-- ch15/ex08.hs
-- Monoid exercises

module Exercises where

main :: IO ()
main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (mappend f' mempty) 0
      rmright = runMem (mappend mempty f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0



f' = Mem $ \s -> ("hi", s + 1)

newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x))
  mappend x y =
    Mem $ (\n -> ( (mappend (fst $ runMem x n) (fst $ runMem y n) ),
                   snd $ runMem x (snd $ runMem y n) ) )
