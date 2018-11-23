-- ch21-01.hs

module Practice where

data Query    = Query deriving (Eq, Show)
data SomeObj  = SomeObj deriving (Eq, Show)
data IoOnlyOb = IoOnlyOb deriving (Eq, Show)
data Err      = Err deriving (Eq, Show)

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyOb)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyOb)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err)  -> return $ Left $ err
    (Right res) -> do
      b <- makeIoOnlyObj res
      return $ Right b

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyOb)])
pipelineFn' query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (mapM decodeFn a)

pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyOb)])
pipelineFn'' =
  (traverse makeIoOnlyObj
  . traverse decodeFn =<<) . fetchFn

