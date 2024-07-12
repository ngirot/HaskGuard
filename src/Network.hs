{-# LANGUAGE TypeApplications #-}

module Network (runTCPServer, runTCPClient, IpType (..)) where

import qualified Control.Arrow as A
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Errors
import Network.Socket

data IpType = IpV6 | IpV4

runTCPServer :: IpType -> HostName -> ServiceName -> (Either String String -> IO ()) -> (Socket -> IO a) -> IO (Either String a)
runTCPServer ipType mhost port onConnect server = withSocketsDo $ do
  addr <- resolve
  if length addr >= 1
    then do
      launchResult <- E.try @E.IOException $ E.bracket (open $ head addr) close loop
      case launchResult of
        Right r -> pure $ Right r
        Left err -> do
          let errorMessage = "Unable to listen on '" ++ (show mhost) ++ ":" ++ port ++ "' : " ++ show err
          onConnect $ Left errorMessage
          pure $ Left errorMessage
    else do
      let errorMessage = "Unable to launche server"
      onConnect $ Left errorMessage
      return $ Left errorMessage
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      filter (filterIpByVersion ipType) <$> getAddrInfo (Just hints) realHost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      if (addrFamily addr) == AF_INET6
        then setSocketOption sock IPv6Only 1
        else return ()

      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      onConnect $ Right $ formatConnection addr
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
    realHost = case mhost of
      "0.0.0.0" -> Nothing
      "::" -> Nothing
      _ -> Just mhost

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO (Either NetworkError a)
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  case addr of
    Right r -> A.left mapCon <$> launchConnect r
    Left x -> pure $ Left x
  where
    mapCon _ = ConnectionRefused
    launchConnect a = E.try @E.IOException $ E.bracket (open a) close client
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      e <- E.try @E.IOException $ getAddrInfo (Just hints) (Just host) (Just port)
      case e of
        Right b -> pure $ Right $ head b
        Left _ -> pure $ Left NameOrServiceNotKnown
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock

filterIpByVersion :: IpType -> AddrInfo -> Bool
filterIpByVersion ipType addr = case ipType of
  IpV4 -> (addrFamily addr) == AF_INET
  IpV6 -> (addrFamily addr) == AF_INET6

formatConnection :: AddrInfo -> String
formatConnection addr = show $ addrAddress addr
