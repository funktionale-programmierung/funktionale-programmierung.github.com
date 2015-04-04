{-# LANGUAGE RecursiveDo #-}
import Control.Monad
import Data.Word
import Control.Monad.Fix

newtype Write a = Write (Word16 -> ([Word8], a))

runWrite :: Write a -> Word16 -> ([Word8], a)
runWrite (Write result) = result

execWrite :: Write a -> [Word8]
execWrite (Write result) = fst $ result 0

instance Monad Write where
    return x = Write $ \_ -> ([], x)
    w1 >>= w2 = Write $ \pos1 ->
                let (bytes1, x1) = runWrite w1 pos1
                    pos2 = pos1 + fromIntegral (length bytes1)
                    (bytes2, x2) = runWrite (w2 x1) pos2
                in  (bytes1 ++ bytes2, x2)

writeWord8 :: Word8 -> Write ()
writeWord8 b = Write $ \_ -> ([b], ())

getPosition :: Write Word16
getPosition = Write $ \pos -> ([], pos)

writeWord16 :: Word16 -> Write ()
writeWord16 w = do
    writeWord8 (fromIntegral w1)
    writeWord8 (fromIntegral w2)
  where (w2, w1) = w `divMod` 265

writeWord16List :: [Word16] -> Write ()
writeWord16List ws = do
    writeWord8 (fromIntegral (length ws))
    mapM_ writeWord16 ws

writeAll2 :: ([Word16], [Word16]) -> Write ()
writeAll2 (ws1, ws2) = do
    pos1 <- getPosition
    writeWord16List ws1
    pos2 <- getPosition
    writeWord16List ws2
    writeWord16 pos1
    writeWord16 pos2

instance MonadFix Write where
    mfix f = Write $ \pos -> let (bytes,x) = runWrite (f x) pos in (bytes, x)

writeAll3 :: ([Word16], [Word16]) -> Write ()
writeAll3 (ws1, ws2) = mdo
    writeWord16 pos1
    writeWord16 pos2
    pos1 <- getPosition
    writeWord16List ws1
    pos2 <- getPosition
    writeWord16List ws2

