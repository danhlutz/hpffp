-- ch27-05.hs

module Practice where

hypo'' :: IO ()
hypo'' = do
  let x :: Integer
      x = undefined
  s <- x `seq` getLine
  case s of
    "hi" -> print x
    _    -> putStrLn "hello"
