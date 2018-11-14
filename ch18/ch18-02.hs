-- ch18/ch18-02.hs

module Practice where

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls: "
  name <- getLine
  putStrLn ("y helo there: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls: " >>
  getLine >>=
  \name -> putStrLn ("y hello there" ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y hello there: "
           ++ name ++ " who is "
           ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
  \ name ->
    putStrLn "age pls:" >>
    getLine >>=
    \ age ->
      putStrLn ("y hello "
               ++ name ++ " aged " ++ age)
