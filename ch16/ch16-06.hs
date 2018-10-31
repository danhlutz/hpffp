-- ch16/ch16-06.hs

module Practice where

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a
           => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s

incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' = fmap (+1)

showMaybe'' :: Show a => Maybe a -> Maybe String
showMaybe'' = fmap show

liftedInc :: (Functor f, Num a)
          => f a -> f a
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a)
           => f a -> f String
liftedShow = fmap show
