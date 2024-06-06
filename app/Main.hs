-- Echo server program
module Main (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as DS
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Lib (negotiate, request)

main :: IO ()
main = runTCPServer Nothing "4242" talk
  where
    talk s = do
      putStrLn ">>> Negotiation"
      msg <- recv s 1024
      print $ S.unpack msg
      print $ map DS.w2c $ S.unpack msg

      let d = negotiate msg
      sendAll s d

      putStrLn ">>> Request"
      msg <- recv s 1024
      print $ S.unpack msg
      let r = request msg
      sendAll s r
      -- sendAll s msg

      putStrLn ">>> Forward"
      msg <- recv s 4096
      print $ S.unpack msg
      runTCPClient "142.250.178.132" "80" $ \ss -> do
        putStrLn "HERE"
        sendAll ss msg
        resp <- recv ss 4096
        print $ S.unpack resp
        print $ map DS.w2c $ S.unpack resp
        sendAll s resp

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock