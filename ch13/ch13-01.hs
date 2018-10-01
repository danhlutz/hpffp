-- ch13-01.hs
module Practice where

twoo :: IO Bool
twoo = do c  <- getChar
          c' <- getChar
          return (c == c')

main :: IO ()
main = do c  <- getChar
          c' <- getChar
          if c == c'
            then putStrLn "True"
            else return ()
