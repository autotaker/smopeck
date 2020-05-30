{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Smopeck.Spec.Route(RouteElem(..), splitRoute, substRoute) where
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy    as LBS
import           Data.List
import qualified Data.Map                as M
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import           Network.HTTP.Types.URI
import           Smopeck.Spec.TypeExp

data RouteElem = Elem String | Param String
    deriving (Show)
splitRoute :: Route -> [RouteElem]
splitRoute "/" = []
splitRoute route = unfoldr (\case
    '/':':':s ->
        let (param, s') = span (/= '/') s in
        Just (Param param, s')
    '/':s ->
        let (e, s') = span (/= '/') s in
        Just (Elem e, s')
    [] -> Nothing) route

substRoute :: M.Map String T.Text -> Route -> BS.ByteString
substRoute param = LBS.toStrict . BS.toLazyByteString . encodePathSegmentsRelative . map f . splitRoute
    where
    f (Elem s)  = T.pack s
    f (Param p) = param M.! p

