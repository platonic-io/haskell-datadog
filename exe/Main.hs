{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

-- | A mock server implementing the Agent API.
--
-- Unlike the real agent, input is not validated, and an additional endpoint is
-- exposed that allows downloading all the recorded data in a format compatible
-- with Jaeger tracing.

import           Control.Monad            (void)
import           Control.Monad.IO.Class   (liftIO)
import           Data.IORef
import qualified Data.Map.Strict          as M
import           Network.Wai.Handler.Warp (run)
import           Servant

import           Datadog.Agent
import           Datadog.Jaeger

type Services = (Traces3 :<|> Traces4 :<|> Dump)

main :: IO ()
main = do
  ref <- newIORef []
  let services = (postTraces3 ref) :<|> (postTraces4 ref) :<|> (getTraces ref)
  run 8126 (serve (Proxy @ Services) services)

postTraces3 :: IORef [Trace] -> [Trace] -> Handler ()
postTraces3 r t = void $ postTraces4 r t

postTraces4 :: IORef [Trace] -> [Trace] -> Handler TraceResponse
postTraces4 ref traces = liftIO $ (atomicModifyIORef' ref update)
  where update store = ((reverse traces) <> store, TraceResponse M.empty)

getTraces :: IORef [Trace] -> Handler Jaeger
getTraces ref = liftIO $ toJaeger <$> readIORef ref
