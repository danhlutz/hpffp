-- ch27-02.hs

module OutsideIn where

hypo :: IO ()
hypo = do
  let x :: Int
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _    -> putStrLn "hello"

hypo' :: IO ()
hypo' = do
  let x :: Integer
      x = undefined
  s <- getLine
  case seq x s of
    "hi" -> print x
    _    -> putStrLn "Hello"

wc x z =
  let y = seq undefined 'y'
  in seq y x

notGonnaHappen :: Int
notGonnaHappen =
  let x = undefined
      y = 2
      z = (x `seq` y `seq` 10, 11)
  in snd z
