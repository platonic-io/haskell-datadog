{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-
This tests the client against a real Datadog Agent but is manual because there
is no way to get an HTTP error code from their API:

1. get a DD_API_KEY from datadog (sigh)
2. start the agent using the docker command (below)
3. cabal v2-run -O0 --constraint "datadog-tracing +manual" manual-test
4. check that the trace shows up in datadog, grep for "INFO (api.go"
5. docker stop datadog && docker rm datadog

docker run -d --name datadog \
  -p 8126:8126 \
  --network host \
  -e DD_API_KEY=${DD_API_KEY} \
  -v /sys/fs/cgroup/:/host/sys/fs/cgroup:ro \
  -v /proc/:/host/proc/:ro \
  -e DD_APM_ENABLED=true \
  -e DD_LOG_LEVEL=trace \
  datadog/agent:6.10.3-logs-api-key-rc.1
-}
module Main where

import           Crypto.Random
import           Data.Aeson.Text
import           Data.Binary           (decode)
import qualified Data.ByteString.Lazy  as LBS
import           Data.Int              (Int64)
import qualified Data.Text.Lazy        as LT
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Typeable
import           Data.Word             (Word64)
import           Datadog.Agent
import           Network.HTTP.Client   hiding (Proxy)
import           Servant.Client

main :: IO ()
main = do
  mgr   <- newManager defaultManagerSettings
  base  <- parseBaseUrl "http://localhost:8126"
  start <- toNanos <$> getPOSIXTime
  traceid <- randomWord64
  spanid <- randomWord64
  let http   = client (Proxy @ Traces3)
      env    = mkClientEnv mgr base
      metrics = Just $ Metrics 2
      traces = [ (Trace
                  [ Span "tester" "span" "time" traceid spanid Nothing start 800971
                    Nothing Nothing metrics Nothing
                  ])
               ]
  putStrLn (LT.unpack . encodeToLazyText $ traces)
  res <- runClientM (http traces) env
  putStrLn (show res)

toNanos :: POSIXTime -> Int64
toNanos nominal =
  let (nanos, _) = properFraction (1000000000 * nominal)
  in  nanos

randomWord64 :: MonadRandom m => m Word64
randomWord64 = decode . LBS.fromStrict <$> (getRandomBytes 8)
