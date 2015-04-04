import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Control.Monad

parser1 :: ByteString -> (Word8, Word16, Word16)
parser1 bs = (byte0, word1, word2)
  where
    byte0 = BS.index bs 0
    word1 = fromIntegral (BS.index bs 1) + 265 * fromIntegral (BS.index bs 2)
    word2 = fromIntegral (BS.index bs 3) + 265 * fromIntegral (BS.index bs 4)

type Index = Int

getWord8At :: ByteString -> Index -> Word8
getWord8At bs i = BS.index bs i

getWord16At :: ByteString -> Index -> Word16
getWord16At bs i = fromIntegral b1 + 265 * fromIntegral b2
  where b1 = getWord8At bs i
        b2 = getWord8At bs (i+1)

parser2 :: ByteString -> (Word8, Word16, Word16)
parser2 bs = (byte0, word1, word2)
  where
    byte0 = getWord8At  bs 0
    word1 = getWord16At bs 1
    word2 = getWord16At bs 3

getWord8AtI :: ByteString -> Index -> (Word8, Index)
getWord8AtI bs i = (BS.index bs i, i+1)

getWord16AtI :: ByteString -> Index -> (Word16, Index)
getWord16AtI bs i0 = (fromIntegral b1 + 265 * fromIntegral b2, i2)
  where (b1, i1) = getWord8AtI bs i0
        (b2, i2) = getWord8AtI bs i1

parser3 :: ByteString -> (Word8, Word16, Word16)
parser3 bs = (byte0, word1, word2)
  where
    i0 = 0
    (byte0, i1) = getWord8AtI  bs i0
    (word1, i2) = getWord16AtI bs i1
    (word2, i3) = getWord16AtI bs i2

newtype Parser a = Parser (ByteString -> Index -> (a, Index))

runParser :: Parser a -> ByteString -> Index -> (a,Index)
runParser (Parser p) = p

evalParser :: Parser a -> ByteString -> a
evalParser p bs = fst $ runParser p bs 0

instance Monad Parser where
    return x = Parser (\bs i -> (x, i))
    p1 >>= p2 = Parser (\bs i0 ->
        let (x, i1) = runParser p1 bs i0
            (y, i2) = runParser (p2 x) bs i1
        in (y, i2))

getWord8P :: Parser Word8
getWord8P = Parser (\bs i -> (BS.index bs i, i+1))

getWord16P :: Parser Word16
getWord16P = do
    b1 <- getWord8P
    b2 <- getWord8P
    return (fromIntegral b1 + 265 * fromIntegral b2)

parser4 :: ByteString ->  (Word8, Word16, Word16)
parser4 = evalParser $ do
    byte0 <- getWord8P
    word1 <- getWord16P
    word2 <- getWord16P
    return (byte0, word1, word2)

getWord16ListP :: Parser [Word16]
getWord16ListP = do
    n <- getWord8P
    entries <- replicateM (fromIntegral n) getWord16P
    return entries

lookAt :: Int -> Parser a -> Parser a
lookAt offset p = Parser $ \bs i ->
    let (x,_) = runParser p bs offset
    in  (x,i)

getInd :: Parser a -> Parser a
getInd p = do
    offset <- getWord16P
    lookAt (fromIntegral offset) p

parser5 :: ByteString -> ([Word16], [Word16])
parser5 = evalParser $ do
	list1 <- getInd getWord16ListP
	list2 <- getInd getWord16ListP
	return (list1, list2)
