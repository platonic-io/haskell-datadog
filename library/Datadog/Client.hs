{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | An HTTP Client to post data to a datadog agent.
--
-- Many of our refinements are stricter than the actual requirements, to err on
-- the side of caution, and because the actual requirements are very complex.
module Datadog.Client where

import           Control.Monad             (unless, void)
import           Data.Char                 (isAlpha, isAlphaNum)
import           Data.Int                  (Int64)
import qualified Data.List.NonEmpty        as NEL
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Arbitrary       ()
import           Data.Text.Prettyprint.Doc (viaShow)
import           Data.Time                 (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX     (utcTimeToPOSIXSeconds)
import           Data.Typeable             (Proxy (..), Typeable, typeOf)
import           Data.Word                 (Word64)
import           Refined
import           Servant.Client            (ClientM, client)

import qualified Datadog.Agent             as API

type DDText = NonEmpty && (SizeLessThan 101)

newtype SpanId = SpanId (Refined NonZero Word64)
newtype TraceId = TraceId (Refined NonZero Word64)
newtype ServiceName = ServiceName (Refined (DDText && AlphaNum) Text)

data Trace = Trace
  { tService :: ServiceName
  , tId      :: TraceId
  , tSpans   :: (Refined NonEmpty (Map SpanId Span))
  }

newtype SpanName = SpanName (Refined (DDText && HasAlpha) Text)
newtype MetaKey = MetaKey (Refined (DDText && AlphaNum) Text)
newtype MetaValue = MetaValue (Refined DDText Text)

data Span = Span
  { sName     :: SpanName
  , sParentId :: Maybe SpanId
  , sStart    :: UTCTime
  , sDuration :: NominalDiffTime
  , sMeta     :: Maybe (Map MetaKey MetaValue)
  }

traces :: NEL.NonEmpty Trace -> ClientM ()
traces (NEL.toList -> ts) = void . raw $ toAPI <$> ts
  where
    raw = client (Proxy @ API.Traces3)

    toAPI :: Trace -> API.Trace
    toAPI (trace@(Trace _ _ (M.toList . unrefine -> spans))) =
      API.Trace $ (mkSpan trace) <$> spans

    mkSpan :: Trace -> (SpanId, Span) -> API.Span
    mkSpan (Trace (ServiceName (unrefine -> serviceName))
                  (TraceId (unrefine -> traceId))
                  _)
           ((SpanId (unrefine -> spanId)),
            (Span (SpanName (unrefine -> spanName))
             parent
             start
             duration
             meta)) =
      API.Span serviceName
               spanName
               "time" -- not using resource, but it is required
               traceId
               spanId
               ((\(SpanId (unrefine -> p)) -> p) <$> parent)
               (timeToNanos start)
               (nominalToNanos duration)
               Nothing -- not using error
               ((\m -> (M.map unValue) . (M.mapKeys unKey) $ m) <$> meta)
               Nothing -- not using metrics
               Nothing -- not using type

    unKey (MetaKey (unrefine -> k)) = k
    unValue (MetaValue (unrefine -> v)) = v

    timeToNanos :: UTCTime -> Int64
    timeToNanos time = nominalToNanos $ utcTimeToPOSIXSeconds time

    nominalToNanos :: NominalDiffTime -> Int64
    nominalToNanos time =
      let (nanos, _) = properFraction (1000000000 * time)
      in  nanos

data AlphaNum
instance Predicate AlphaNum Text where
   validate p txt = validate' p (T.all isAlphaNum) txt

data HasAlpha
instance Predicate HasAlpha Text where
   validate p txt = validate' p (T.any isAlpha) txt

validate' :: (Typeable t, Monad m, Show a) => t -> (a -> Bool) -> a -> RefineT m ()
validate' t p a =
  unless (p a) $
  throwRefineOtherException (typeOf t) ("failed predicate: " <> (viaShow a))
