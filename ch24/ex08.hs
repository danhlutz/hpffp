-- ch24/ex08.hs

module Exercise where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.LargeWord
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Word
import Test.Hspec
import Text.Trifecta

digToInt :: Char -> Word32
digToInt c =
  case (M.lookup c digs) of
    Nothing -> 0
    (Just x) -> x
  where digs = M.fromList $ zip ['0'..'9'] [0..9]

digitsToInt :: String -> Word32
digitsToInt str = go 0 str
  where go :: Word32 -> String -> Word32
        go sofar "" = sofar
        go sofar (x:xs) = go (sofar * 10 + (digToInt x)) xs

--

data IPAddress =
  IPAddress Word32
  deriving Eq

instance Show IPAddress where
  show (IPAddress x) = "IPv4 " ++ showIPv4 x

parseIPv4 :: Parser IPAddress
parseIPv4 = do
  a <- some digit
  _ <- char '.'
  b <- some digit
  _ <- char '.'
  c <- some digit
  _ <- char '.'
  d <- some digit
  return $
    IPAddress $
              ((digitsToInt a) * 256 * 256 * 256)
            + ((digitsToInt b) * 256 * 256)
            + ((digitsToInt c) * 256)
            + (digitsToInt d)

deconIPv4 :: Word32 -> [Word32]
deconIPv4 x
  | x < 256 = [x]
  | otherwise = (mod x 256) : (deconIPv4 (div x 256))

deconIPv4' :: Word32 -> [String]
deconIPv4' x = fmap show $ reverse (deconIPv4 x)

showIPv4 :: Word32 -> String
showIPv4 x = mconcat (intersperse "." (deconIPv4' x))

--

data IPAddress6 =
  IPAddress6 Word128
  deriving (Eq, Ord)

instance Show IPAddress6 where
  show (IPAddress6 x) = "IPv6 " ++ (shIPv6 x)

deconIPv6 :: Word128 -> [Word128]
deconIPv6 x
  | x < 65536 = [x]
  | otherwise = (mod x 65536) : deconIPv6 (div x 65536)

deconIPv6' :: Word128 -> [Word128]
deconIPv6' = addZeros . reverse . deconIPv6

shIPv6 :: Word128 -> String
shIPv6 x = mconcat (intersperse ":" (map mkHex (deconIPv6' x)))

addZeros :: [Word128] -> [Word128]
addZeros items =
  if (length items) < 8
  then addZeros (0 : items)
  else items

--

hexToNum :: Char -> Word128
hexToNum c =
  case (M.lookup (toLower c) digs) of
    Nothing -> 0
    Just x  -> x
  where digs =
          M.fromList $ zip (['0'..'9'] ++ ['a'..'f']) [0..15]

hexesToNum :: String -> Word128
hexesToNum str = go 0 str
  where go :: Word128 -> String -> Word128
        go r "" = r
        go r (x:xs) = go (r * 16 + (hexToNum x)) xs

hexHelp :: Word128 -> [Word128]
hexHelp x
  | x < 16 = [x]
  | otherwise = (mod x 16) : hexHelp (div x 16)

hexHelp' :: Word128 -> [Word128]
hexHelp' x = reverse (hexHelp x)

getHexDig :: Word128 -> Char
getHexDig c =
  case (M.lookup c digs) of
    Nothing -> ' '
    Just x -> x
  where digs =
          M.fromList $ zip [0..15] (['0'..'9'] ++ ['a'..'f'])

mkHex :: Word128 -> String
mkHex w = map getHexDig (hexHelp' w)

--

parseHex :: Parser Char
parseHex =
  foldr ((<|>)) (char 'F') (fmap char hexDigs)
  where hexDigs :: [Char]
        hexDigs = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'E']

parseIPv6Section :: Parser Word128
parseIPv6Section = do
  x <- some parseHex
  return $ hexesToNum x

parseLastIPv6Section :: Parser [Word128]
parseLastIPv6Section = do
  x <- parseIPv6Section
  return [x]

parseIPv6Sections :: Parser [Word128]
parseIPv6Sections = do
  x <- parseIPv6Section
  _ <- char ':'
  rest <- try parseIPv6Sections <|> parseLastIPv6Section
  return $ x : rest

parseSimpleIPv6 :: Parser ([Word128], [Word128])
parseSimpleIPv6 = do
  a <- parseIPv6Section
  _ <- char ':'
  b <- parseIPv6Section
  _ <- char ':'
  c <- parseIPv6Section
  _ <- char ':'
  d <- parseIPv6Section
  _ <- char ':'
  e <- parseIPv6Section
  _ <- char ':'
  f <- parseIPv6Section
  _ <- char ':'
  g <- parseIPv6Section
  _ <- char ':'
  h <- parseIPv6Section
  return ([a, b, c, d, e, f, g, h], [])

parseComplexIPv6 :: Parser ([Word128], [Word128])
parseComplexIPv6 = do
  x <- try parseIPv6Sections <|> parseLastIPv6Section
  _ <- char ':'
  _ <- char ':'
  y <- try parseIPv6Sections <|> parseLastIPv6Section
  return (x, y)

parseIPv6 :: Parser IPAddress6
parseIPv6 = do
  x <- try parseComplexIPv6 <|> parseSimpleIPv6
  return $ mkIPv6 x

makeZeroList :: Int -> [Word128]
makeZeroList 0 = []
makeZeroList n = 0 : makeZeroList (n - 1)

convertToNum :: [Word128] -> Word128
convertToNum s = go 0 s
  where go :: Word128 -> [Word128] -> Word128
        go r [] = r
        go r (x:xs) =
          go (r * 65536 + x) xs

mkIPv6 :: ([Word128], [Word128]) -> IPAddress6
mkIPv6 (x, y) = IPAddress6 (convertToNum sections)
  where sections :: [Word128]
        sections = x ++ (makeZeroList diff) ++ y
        diff :: Int
        diff = 8 - ((length x) + (length y))

--

iPv4ToIPv6 :: IPAddress -> IPAddress6
iPv4ToIPv6 x =
  case (p parseIPv6 (toIPv6Str x)) of
    (Success x) -> x
    (Failure x) -> error "Could not convert"

toIPv6Str :: IPAddress -> String
toIPv6Str (IPAddress x) = "0::" ++ a ++ b
  where first = div x 65536
        second = mod x 65536
        a = mkHex32 first
        b = mkHex32 second

mkHex32 :: Word32 -> String
mkHex32 x = map hexDig32 (deconWord32' x)

hexDig32 :: Word32 -> Char
hexDig32 x =
  case (M.lookup x digs) of
    Nothing -> '0'
    (Just r) -> r
  where digs =
          M.fromList $ zip [0..15] (['0'..'9'] ++ ['a'..'f'])

deconWord32 :: Word32 -> [Word32]
deconWord32 x
  | x < 16 = [x]
  | otherwise = (div x 16) : (deconWord32 (mod x 16))

deconWord32' :: Word32 -> [Word32]
deconWord32' = reverse . deconWord32

--

p :: Parser a -> String -> Result a
p f i = parseString f mempty i

main :: IO ()
main = hspec $ do
  describe "IPv4 parsing" $
    it "can convert IPv4 to an integer" $ do
      let (Success x) = p parseIPv4 "172.16.254.1"
      shouldBe x (IPAddress 2886794753)
  describe "convert hex char to Word128" $
    it "can convert a hex char" $ do
      shouldBe (hexToNum 'E') 14
  describe "convert hex string to Word128" $
    it "can convert hex string to Word128" $ do
      shouldBe (hexesToNum "Ff") 255
  describe "parseIPv6 section" $
    it "can parse an IPv6 section to Word128" $ do
      let (Success x) = p parseIPv6Section "fff1"
      shouldBe x 65521
  describe "parseIPv6 sections" $
    it "can convert sections to list of Word128s" $ do
      let (Success x) = p parseIPv6 "0:0:0:0:0:ffff:ac10:fe01"
      shouldBe x (IPAddress6 281473568538113)
  describe "parseIPv6 with abbreviation" $
    it "can convert an IPv6 with a missing section" $ do
      let (Success x) = p parseIPv6 "2001:DB8::8:800:200C:417A"
      shouldBe x (IPAddress6 42540766411282592856906245548098208122)
  describe "collapsed IPs are equal" $
    it "collapsed IPs return the same value" $ do
      let (Success a) = p parseIPv6 "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
          (Success b) = p parseIPv6 "FE80::0202:B3FF:FE1E:8329"
      shouldBe a b
  describe "show IPv6" $
    it "show an IPv6 address" $ do
      let (Success x) = p parseIPv6 "FE80::0202:B3FF:Fe1e:8329"
      shouldBe (show x) ("IPv6 fe80:0:0:0:202:b3ff:fe1e:8329")
