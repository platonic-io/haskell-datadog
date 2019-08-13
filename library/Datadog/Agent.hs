{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeOperators              #-}

-- | A model of the API provided by https://github.com/DataDog/datadog-agent
module Datadog.Agent where

import           Data.Aeson
import           Data.Int               (Int32, Int64)
import           Data.Map.Strict        (Map)
import           Data.Maybe             (catMaybes)
import           Data.MessagePack       (MessagePack, fromObject, toObject)
import           Data.MessagePack.Aeson
import           Data.Text              (Text)
import           Data.Text.Arbitrary    ()
import           Data.Word              (Word64)
import           Generic.Random         (genericArbitraryU)
import           GHC.Generics           (Generic)
import           Servant.API
import           Servant.MsgPack
import           Test.QuickCheck        (Arbitrary, arbitrary)

-- defined in pkg/trace/api/api.go
--
-- Clients post a list of traces. The datadog-agent (based on version
-- e34402aeae95a619303c0cb624ba5b3908f5c358) silently rejects invalid traces
-- under any of the following conditions:
--
-- * if there is a trace ID discrepancy between 2 spans
-- * if two spans have the same span_id
-- * if it is empty
-- * if trace_id or span_id is 0
-- * if service is empty or longer than 100 chars after applying
--   `normalizeTag` rules (tl;dr letters and colons are safe)
-- * if name is empty, doesn't have any alphabetic chars, or is longer than
--   100 chars after applying `normMetricNameParse` rules (tl;dr underscores
--   only allowed in certain places)
-- * if type is empty or longer than 100 chars after attempting to correct
--   any UTF-8 errors.
-- * if the span duration is less than 0 or longer than 10 minutes
-- * if the resource is not valid UTF-8 and cannot be fixed. The upstream
--   server has a 5000 character limit.
-- * if the start is before 2000-01-01
--
-- and will silently rewrite invalid data, e.g. the normalisation routines, when
-- ParentID == TraceID == SpanID the parent is reset to 0.
type Traces4 = "v0.4" :> "traces"
              :> ReqBody '[MsgPack, JSON] [Trace]
              :> Put '[MsgPack, JSON] TraceResponse
              -- WARNING: agent incorrectly sets the response type to be plain text
              -- https://github.com/DataDog/datadog-agent/issues/3207#issuecomment-476716966
-- the go client uses POST instead of the documented PUT :slowclap:
type Traces4' = "v0.4" :> "traces"
              :> ReqBody '[MsgPack, JSON] [Trace]
              :> Post '[MsgPack, JSON] TraceResponse

-- backcompat
type Traces3 = "v0.3" :> "traces"
              :> ReqBody '[MsgPack, JSON] [Trace]
              :> Put '[PlainText] NoContent

newtype Trace = Trace [Span] deriving (ToJSON, FromJSON)

instance MessagePack Trace where
  fromObject = viaFromJSON
  toObject = unsafeViaToJSON

-- | https://docs.datadoghq.com/api/?lang=python#tracing
data Span = Span
  { spanService  :: Text
  , spanName     :: Text
  , spanResource :: Text
  , spanTraceId  :: Word64
  , spanId       :: Word64
  , spanParentId :: Maybe Word64
  , spanStart    :: Int64
  , spanDuration :: Int64
  , spanError    :: Maybe Int32
  , spanMeta     :: Maybe (Map Text Text)
  , spanMetrics  :: Maybe (Map Text Double)
  , spanType     :: Maybe Text
  } deriving (Generic)

instance Arbitrary Span where
  arbitrary = genericArbitraryU

instance ToJSON Span where
  toJSON Span{..} = object'
    [ "service"   .= spanService
    , "name"      .= spanName
    , "resource"  .= spanResource
    , "trace_id"  .= spanTraceId
    , "span_id"   .= spanId
    , "start"     .= spanStart
    , "duration"  .= spanDuration
    ]
    [ "parent_id" .=? spanParentId
    , "error"     .=? spanError
    , "meta"      .=? spanMeta
    , "metrics"   .=? spanMetrics
    , "type"      .=? spanType
    ]
    where
      key .=? value = (key .=) <$> value
      object' required optional = object $ required <> catMaybes optional

instance FromJSON Span where
  parseJSON = withObject "Span" $ \v ->
    Span <$> v .: "service"
         <*> v .: "name"
         <*> v .: "resource"
         <*> v .: "trace_id"
         <*> v .: "span_id"
         <*> v .:? "parent_id" .!= Nothing
         <*> v .: "start"
         <*> v .: "duration"
         <*> v .:? "error" .!= Nothing
         <*> v .:? "meta" .!= Nothing
         <*> v .:? "metrics" .!= Nothing
         <*> v .:? "type" .!= Nothing

-- "rate" is a number between `[0.0, 1.0]` indicating the desired percentage of
-- traces that the agent wishes to downsample for a given service (`0.0` meaning
-- dropping everything, `1.0` meaning keeping everything).
--
-- Clients should act upon `rate` by setting a `_sampling_priority_v1` field in
-- `metrics` of the root span, which is an enum `[-1, 0, 1, 2]` indicating:
--
--    -1) the agent should drop the trace
--     0) the agent may drop the trace
--     1) the agent should try to keep the trace
--     2) the agent must keep the trace (subject to account limits).
data TraceResponse = TraceResponse
  { trRateByService :: Map Text Double
  }

instance ToJSON TraceResponse where
  toJSON (TraceResponse rates) = object
    [ "rate_by_service" .= rates
    ]

instance FromJSON TraceResponse where
  parseJSON = withObject "TraceResponse" $ \v ->
    TraceResponse <$> v .: "rate_by_service"
