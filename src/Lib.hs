module Lib (serve) where

import Config
import Control.Concurrent
import Control.Concurrent.Async
import Data.Either
import Data.UUID
import Network
import Network.Socket
import Step
import System.Random

serve :: ApplicationConfiguration -> (String -> IO ()) -> ([String] -> [String] -> IO ()) -> IO ()
serve configuration logger onStartup = do
  threads <- mapM (startOne serverConfiguration talk) [IpV4, IpV6]

  hostsStarted <- mapM takeMVar $ snd <$> threads
  onStartup (lefts hostsStarted) (rights hostsStarted)

  mapM_ wait $ fst <$> threads
  where
    serverConfiguration = acServer configuration
    authenticationConfiguration = acAuthentication configuration
    talk sock clientAddr = do
      uuid <- newUUID
      onConnectionReceived authenticationConfiguration logger uuid sock clientAddr

onConnectionReceived :: AuthenticationConfiguration -> (String -> IO ()) -> UUID -> Socket -> SockAddr -> IO ()
onConnectionReceived authConf logger requestId sock clientAddr = do
  let firstStep = Just $ OPEN $ show clientAddr
  onProcessStep firstStep authConf sock requestIdLogger
  where
    requestIdLogger msg = logger $ "[" ++ show requestId ++ "] " ++ msg

onProcessStep :: Maybe Step -> AuthenticationConfiguration -> Socket -> (String -> IO ()) -> IO ()
onProcessStep Nothing _ _ _ = return ()
onProcessStep (Just step) authConf sock logger = do
  nextStep <- processStep step authConf sock logger
  onProcessStep nextStep authConf sock logger

normalizeListen :: IpType -> String -> String
normalizeListen IpV6 "0.0.0.0" = "::"
normalizeListen IpV4 "::" = "0.0.0.0"
normalizeListen _ host = host

startOne :: ServerConfiguration -> (Socket -> SockAddr -> IO ()) -> IpType -> IO ((Async (Either String ()), MVar (Either String String)))
startOne configuration fn ipType = do
  mVar <- newEmptyMVar
  threadId <- async $ runTCPServer ipType (normalizeListen ipType $ scListen configuration) (show $ scPort configuration) (putMVar mVar) fn
  pure $ (threadId, mVar)

newUUID :: IO UUID
newUUID = randomIO
