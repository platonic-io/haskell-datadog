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
import qualified Data.Map.Strict as M
import           Data.Maybe      (mapMaybe, maybeToList)
import qualified Data.Text       as T
import           Jaeger.Data
import           Servant.API

import qualified Datadog.Agent   as Agent

type Dump = "dump" :> Get '[JSON] Jaeger
type Reset = "dump" :> DeleteNoContent '[JSON] NoContent

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
          mkSpan s = SpanID $ (showt spanTraceId) <> "-" <> (showt s)
      in Span (mkSpan spanId)
              traceId
              (Name spanName)
              ((Reference traceId) . mkSpan <$> maybeToList spanParentId)
              (toInteger spanStart)
              (toInteger spanDuration)
              (mkTag <$> (concat $ M.toList <$> spanMeta))
              (ProcessID spanService)
    services spans = nub $ Agent.spanService <$> spans
    showt = T.pack . show
    mkTag (k, v) = Tag k (String v)
