{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Network hiding (accept)
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import Network.BSD

import Control.Exception
import Control.Concurrent
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.Encoding as T

_RECEIVE_TIMEOUT_ :: Int
_RECEIVE_TIMEOUT_ = 2 * 1000 * 1000 -- in microseconds

parseInt :: T.Text -> Either String Int
parseInt x =
    case T.decimal x of
      Left err -> Left err
      Right (i, rest) ->
          if T.null rest
          then Right i
          else Left ("parse error: " ++ T.unpack rest)

recvText sock n =
    do bs <- recv sock n
       return $ T.decodeUtf8 bs

sendText sock t =
    send sock (T.encodeUtf8 (t `T.append` "\n"))

forLines :: Socket -> (T.Text -> IO Bool) -> IO ()
forLines sock fun = loop T.empty
    where
      loop hangover =
          do t <- recvText sock 1024
             let fullText = hangover `T.append` t
             case T.lines fullText of
               [] -> loop fullText
               lines ->
                   do mRest <- iterLines (isNewline (T.last fullText)) lines
                      case mRest of
                        Nothing -> return ()
                        Just rest -> loop rest
      iterLines _ [] = return (Just T.empty)
      iterLines False [last] = return (Just last)
      iterLines lastComplete (first:rest) =
          do cont <- fun first
             if cont
             then iterLines lastComplete rest
             else return Nothing
      isNewline c = c == '\n' || c == '\r'

talk :: Socket -> IO ()
talk sock = forLines sock handleLine
    where
      handleLine line =
          if line == "end"
          then do sendText sock "Thank you for using the Haskell doubling service."
                  return False
          else do case parseInt line of
                    Right i -> sendText sock $ T.pack (show (2 * i))
                    Left _ -> sendText sock "not an integer"
                  return True

myListenOn port =
    do proto <- getProtocolNumber "tcp"
       bracketOnError
        (socket AF_INET Stream proto)
        (sClose)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (SockAddrInet port iNADDR_ANY)
            listen sock 10000
            return sock)

ignoreExc :: IO () -> IO ()
ignoreExc action =
   action `catch` (\(e::SomeException) -> return ())

main =
    do s <- myListenOn 44444
       forever $ do (sock, _) <- accept s
                    forkIO (talk sock `finally` ignoreExc (sClose sock))
