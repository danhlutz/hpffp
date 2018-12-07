-- ch24/ex02.hs

module Exercise where

import Text.Trifecta

parseNum :: Parser Integer
parseNum = do
  myNum <- decimal
  eof
  return myNum
