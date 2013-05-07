import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Network

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.Encoding as T

processLine :: Monad m => Conduit BS.ByteString m BS.ByteString
processLine =
    do mBs <- await
       case mBs of
         Nothing -> return () -- eof
         Just bs
             | bs == BSC.pack "end" ->
                 do yield (BSC.pack "Thank you for using the Haskell doubling service.\n")
                    return ()
             | otherwise ->
                 do case parseInt (T.decodeUtf8 bs) of
                      Right i -> yield (BSC.pack (show (2 * i) ++ "\n"))
                      Left _ -> yield (BSC.pack "not an integer\n")
                    processLine
    where
      parseInt x =
          case T.decimal x of
            Left err -> Left err
            Right (i, rest) ->
                if T.null rest
                then Right i
                else Left ("parse error: " ++ T.unpack rest)

main =
    do let settings = serverSettings 44444 HostAny
       runTCPServer settings doubleApp
    where
      doubleApp x =
          appSource x $= (CB.lines =$= processLine) $$ appSink x
