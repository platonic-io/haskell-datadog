{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

-- | Mimicks the Jaeger `api/traces` API, as used by `jaeger-flamegraph`, but
-- without requiring service names to be provided.
module Datadog.Jaeger where

import           Data.Aeson
import           Data.List       (nub)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (mapMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Generics    (Generic)
import           Servant.API

import qualified Datadog.Agent   as Agent

type Dump = "dump" :> Get '[JSON] Jaeger

toJaeger :: [Agent.Trace] -> Jaeger
toJaeger traces = Jaeger $ mapMaybe traceToData traces
  where
    traceToData (Agent.Trace []) = Nothing
    traceToData (Agent.Trace spans) =
      let Agent.Span{..} = head spans
      in  Just $ Data
            (TraceID . showt $ spanTraceId)
            (spanToSpan <$> spans)
            (M.fromList $ (\a -> (ProcessID a, Process a)) <$> services spans)
    spanToSpan Agent.Span{..} =
      let traceId = (TraceID . showt $ spanTraceId)
          parents = if spanParentId == 0 then [] else [spanParentId]
      in Span (SpanID . showt $ spanId)
              traceId
              (Name spanName)
              ((Reference traceId) . (SpanID . showt) <$> parents)
              (toInteger spanStart)
              (toInteger spanDuration)
              (mkTag <$> (concat $ M.toList <$> spanMeta))
              (ProcessID spanService)
    services spans = nub $ Agent.spanService <$> spans
    showt = T.pack . show
    mkTag (k, v) = Tag $ T.concat [k, ":", v]

newtype Jaeger = Jaeger [Data]
instance ToJSON Jaeger where
  toJSON (Jaeger dat) = object ["data" .= dat]

newtype TraceID   = TraceID Text deriving newtype (Eq, Ord, ToJSON)
newtype SpanID    = SpanID  Text deriving newtype (Eq, Ord, ToJSON)
newtype ProcessID = ProcessID Text deriving newtype (Eq, Ord, ToJSON, ToJSONKey)
newtype Name      = Name Text deriving newtype (Eq, ToJSON)

data Data = Data
  { traceID   :: TraceID
  , spans     :: [Span]
  , processes :: Map ProcessID Process
  } deriving (Generic, ToJSON)

data Process = Process
  { serviceName :: Text
  } deriving (Generic, ToJSON)

data Span = Span
  { spanID        :: SpanID
  , traceID       :: TraceID
  , operationName :: Name
  , references    :: [Reference]
  , startTime     :: Integer
  , duration      :: Integer
  , tags          :: [Tag]
  , processID     :: ProcessID
  } deriving (Generic, ToJSON)

data Reference = Reference
  { traceID :: TraceID
  , spanID  :: SpanID
  } deriving (Eq, Ord, Generic, ToJSON)

newtype Tag = Tag
  { key :: Text
  } deriving (Eq, Generic)
    deriving anyclass (ToJSON)
