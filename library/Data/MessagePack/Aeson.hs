-- | This file is copied and pasted from
--   http://hackage.haskell.org/package/msgpack-aeson-0.1.0.0/docs/src/Data-MessagePack-Aeson.html#toAeson
--   with only minor changes to use the data-msgpack fork (mostly changing Vector to list).

{-# LANGUAGE LambdaCase #-}

-- | Aeson bridge for MessagePack
--
-- Copyright: (c) 2015 Hideyuki Tanaka
module Data.MessagePack.Aeson where

import           Control.Arrow
import           Data.Aeson           as A
import           Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict  as HM
import           Data.MessagePack     as MP
import           Data.Scientific
import qualified Data.Text.Encoding   as T
import qualified Data.Vector          as V

toAeson :: MP.Object -> A.Result Value
toAeson = \case
  ObjectNil      -> pure Null
  ObjectBool b   -> pure . Bool $ b
  ObjectInt n    -> pure . Number $ fromIntegral n
  ObjectFloat f  -> pure . Number $ realToFrac f
  ObjectDouble d -> pure . Number $ realToFrac d
  ObjectStr t    -> pure . String $ t
  ObjectBin b    -> String <$> either (fail . show) pure (T.decodeUtf8' b)
  ObjectArray v  -> Array <$> V.mapM toAeson v
  ObjectMap m    ->
    A.Object . HM.fromList . V.toList
      <$> V.mapM (\(k, v) -> (,) <$> from k <*> toAeson v) m
      where from = maybe (fail "bad object") pure . MP.fromObject
  ObjectExt _ _  -> fail "ObjectExt is not supported"

fromAeson :: Value -> MP.Object
fromAeson = \case
  Null        -> ObjectNil
  Bool b      -> ObjectBool b
  Number s ->
    case floatingOrInteger s of
      Left f  -> ObjectDouble f
      Right n -> ObjectInt n
  String t    -> ObjectStr t
  Array v     -> ObjectArray $ V.map fromAeson v
  A.Object o  -> ObjectMap $ V.fromList $ map (toObject *** fromAeson) $ HM.toList o

packToJSON :: ToJSON a => a -> ByteString
packToJSON = pack . fromAeson . toJSON

unpackFromJSON :: FromJSON a => ByteString -> Result a
unpackFromJSON b = fromJSON =<< toAeson =<< maybe (fail "msgpack") pure (unpack b)
