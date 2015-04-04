import Control.Monad
import Data.Word


newtype Write a = Write ([Word8], a)

runWriter :: Write a -> ([Word8], a)
runWriter (Write result) = result

execWriter :: Write a -> [Word8]
execWriter (Write (bytes,_)) = bytes

instance Monad Write where
    return x = Write ([], x)
    w1 >>= w2 = let (bytes1, x1) = runWriter w1
                    (bytes2, x2) = runWriter (w2 x1)
                in  Write (bytes1 ++ bytes2, x2)

writeWord8 :: Word8 -> Write ()
writeWord8 b = Write ([b], ())

writeWord16 :: Word16 -> Write ()
writeWord16 w = do
    writeWord8 (fromIntegral w1)
    writeWord8 (fromIntegral w2)
  where (w2, w1) = w `divMod` 265

writeWord16List :: [Word16] -> Write ()
writeWord16List ws = do
    writeWord8 (fromIntegral (length ws))
    mapM_ writeWord16 ws

writeAll1 :: ([Word16], [Word16]) -> Write ()
writeAll1 (ws1, ws2) = do
    writeWord16 4
    writeWord16 (4 + 1 + 2 * fromIntegral (length ws2))
    writeWord16List ws1
    writeWord16List ws2


