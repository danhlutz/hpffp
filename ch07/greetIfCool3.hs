-- greetIfCool3.hs
module GreetIfCool3 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True  ->
      putStrLn "eyyyyyy. What's shakin'?"
    False ->
      putStrLn "pssh."
  where cool = 
          coolness == "downright frosty yo"
