{-# LANGUAGE TypeApplications #-}

module Network (runTCPServer, runTCPClient, IpType (..)) where

import qualified Control.Arrow as A
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Errors
import Network.Socket

data IpType = IpV6 | IpV4

runTCPServer :: IpType -> HostName -> ServiceName -> (Either String String -> IO ()) -> (Socket -> SockAddr -> IO a) -> IO (Either String a)
runTCPServer ipType host port onConnect server = withSocketsDo $ do
  addrResolved <- resolve
  case addrResolved of
    Right addr -> do
      if length addr >= 1
        then do
          launchResult <- E.try @E.IOException $ E.bracket (open $ head addr) close loop
          case launchResult of
            Right r -> pure $ Right r
            Left err -> finalize $ "Unable to listen on '" ++ (show host) ++ ":" ++ port ++ "' : " ++ show err
        else finalize ""
    Left _ -> finalize $ "Not network interface found for " ++ (show host) ++ ":" ++ port
  where
    finalize message = do
      onConnect $ Left message
      return $ Left message
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      E.try @E.IOException $ filter (filterIpByVersion ipType) <$> getAddrInfo (Just hints) realHost (Just port)
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
            forkFinally (server conn _peer) (const $ gracefulClose conn 5000)
    realHost = case host of
      "0.0.0.0" -> Nothing
      "::" -> Nothing
      _ -> Just host

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO (Either NetworkError a)
runTCPClient host port client = withSocketsDo $ do
  resolvedAddr <- resolve
  case resolvedAddr of
    Right addr -> A.left mapCon <$> launchConnect addr
    Left err -> pure $ Left err
  where
    mapCon _ = ConnectionRefused
    launchConnect addr = E.try @E.IOException $ E.bracket (open addr) close client
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      addrResult <- E.try @E.IOException $ getAddrInfo (Just hints) (Just host) (Just port)
      case addrResult of
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
