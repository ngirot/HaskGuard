{-# LANGUAGE TypeApplications #-}

module Network (runTCPServer, runTCPClient) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void, mfilter)
import Errors
import Network.Socket

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      head <$> getAddrInfo (Just hints) realHost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
          void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)
    realHost = mfilter (/= "0.0.0.0") mhost

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO (Either NetworkError a)
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve2
  case addr of
    Right r -> fmap Right $ ooo r
    Left x -> pure $ Left x
  where
    ooo a = E.bracket (open a) close client
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      fmap Right $ head <$> getAddrInfo (Just hints) (Just host) (Just port)
    resolve2 = do
      let hints = defaultHints {addrSocketType = Stream}
      e <- E.try @E.IOException $ getAddrInfo (Just hints) (Just host) (Just port)
      case e of
        Right b -> pure $ Right $ head b
        Left _ -> pure $ Left NameOrServiceNotKnown
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock
