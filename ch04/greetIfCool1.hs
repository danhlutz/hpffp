--greetIfCool.hs
module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness = 
        if cool
          then putStrLn "eyyyyyy. What's shakin'?"
        else
          putStrLn "psssh."
        where cool =
                coolness == "downright frosty yo"
