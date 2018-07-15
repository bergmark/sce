{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Prelude

import Control.Concurrent
import Data.Aeson
import Data.Aeson.TH
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Servant
import Servant.Checked.Exceptions
import Servant.Client

data BadReq = BadReq deriving Show

deriveJSON defaultOptions ''BadReq

instance ErrStatus BadReq where toErrStatus BadReq = badRequest400

type API = Throws BadReq :> Get '[JSON] Value

api :: Proxy API
api = Proxy

server :: Server API
server = pureErrEnvelope BadReq

main :: IO ()
main = do
  tid <- forkIO (run 8080 $ serve api server)
  manager' <- newManager defaultManagerSettings
  print =<< runClientM (client api) (mkClientEnv manager' (BaseUrl Http "localhost" 8080 ""))
  killThread tid
