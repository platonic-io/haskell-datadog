{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Servant.MsgPack where

import           Data.List.NonEmpty
import           Data.MessagePack
import           Network.HTTP.Media       ((//))
import           Servant.API.ContentTypes

data MsgPack

instance Accept MsgPack where
  contentTypes _ = fromList [ "application" // "msgpack"
                            , "application" // "x-msgpack"
                            , "application" // "vnd.msgpack"]

instance MessagePack a => MimeRender MsgPack a where
  mimeRender _ = pack

instance MessagePack a => MimeUnrender MsgPack a where
  mimeUnrender _ = unpack
