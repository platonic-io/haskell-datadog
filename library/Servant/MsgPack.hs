{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | msgpack support for Servant, piggy backing off Aeson.
--
--   Note: if we wish to improve performance and/or control over the msgpack
--   formats, we should avoid the Aeson intermediate format and create direct
--   MessagePack a => Mime(R,Unr)ender instances.
module Servant.MsgPack where

import           Data.Aeson
import           Data.List.NonEmpty
import           Data.MessagePack         (pack, unpack)
import           Data.MessagePack.Aeson
import           Network.HTTP.Media       ((//))
import           Servant.API.ContentTypes

data MsgPack

instance Accept MsgPack where
  contentTypes _ = fromList [ "application" // "msgpack"
                            , "application" // "x-msgpack"
                            , "application" // "vnd.msgpack"]

instance ToJSON a => MimeRender MsgPack a where
  mimeRender _ = pack . fromAeson . toJSON

instance FromJSON a => MimeUnrender MsgPack a where
  mimeUnrender _ b = case fromJSON =<< toAeson =<< unpack b of
    Error str -> Left str
    Success a -> Right a
