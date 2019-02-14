{-# LANGUAGE TypeApplications #-}

module Datadog.AgentTest where

import           Test.Aeson.GenericSpecs
import           Test.Tasty.Hspec

import           Datadog.Agent

spec_json :: Spec
spec_json = roundtripAndGoldenSpecs (Proxy @Span)
