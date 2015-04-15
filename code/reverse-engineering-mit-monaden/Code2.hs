import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Control.Monad

type Index = Int
type Seg = (String, Index, Index)

newtype Parser a = Parser (ByteString -> Index -> (a, [Seg], Index))

runParser :: Parser a -> ByteString -> Index -> (a, [Seg], Index)
runParser (Parser p) = p

evalParser :: Parser a -> ByteString -> (a, [Seg])
evalParser p bs =
    let (x,s,_) = runParser p bs 0
    in (x,s)

instance Monad Parser where
    return x = Parser (\bs i -> (x, [], i))
    p1 >>= p2 = Parser (\bs i0 ->
        let (x, s1, i1) = runParser p1 bs i0
            (y, s2, i2) = runParser (p2 x) bs i1
        in (y, s1++s2, i2))

getWord8P :: Parser Word8
getWord8P = Parser (\bs i -> (BS.index bs i, [], i+1))

getWord16P :: Parser Word16
getWord16P = do
    b1 <- getWord8P
    b2 <- getWord8P
    return (2^8 * fromIntegral b1 + fromIntegral b2)

getWord16ListP :: Parser [Word16]
getWord16ListP = do
    n <- getWord8P
    entries <- replicateM (fromIntegral n) getWord16P
    return entries

lookAt :: Int -> Parser a -> Parser a
lookAt offset p = Parser $ \bs i ->
    let (x,s,_) = runParser p bs offset
    in  (x,s,i)

getInd :: Parser a -> Parser a
getInd p = do
    offset <- getWord16P
    lookAt (fromIntegral offset) p

parser5 :: ByteString -> (([Word16], [Word16]), [Seg])
parser5 = evalParser $ do
	list1 <- getInd getWord16ListP
	list2 <- getInd getWord16ListP
	return (list1, list2)

named :: String -> Parser a -> Parser a
named n p = Parser $ \bs i0 ->
    let (x, segments, i1) = runParser p bs i0
    in  (x, (n,i0,i1):[(n ++ "/" ++ n', i0', i1') | (n',i0',i1') <- segments], i1)

parser6 :: ByteString -> (([Word16], [Word16]), [Seg])
parser6 = evalParser $ named "Header" $ do
	list1 <- getInd $ named "Liste1" getWord16ListP
	list2 <- getInd $ named "Liste2" getWord16ListP
	return (list1, list2)
