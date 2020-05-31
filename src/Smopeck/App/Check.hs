{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
module Smopeck.App.Check (
    runApp
)where

import           Control.Lens
import           Control.Monad.Except
import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as A
import           Data.Aeson.Lens
import           Data.Bifunctor
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.CaseInsensitive     (CI)
import qualified Data.CaseInsensitive     as CI
import           Data.Either
import qualified Data.Map                 as M
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Lazy.Builder   as LT
import qualified Data.Text.Lazy.IO        as LT
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Paths_smopeck
import           Smopeck.Config
import qualified Smopeck.Mock.Constraint  as Constraint
import           Smopeck.Spec.Preprocess
import           Smopeck.Spec.Route
import           Smopeck.Spec.TypeExp     (Route)
import qualified Smopeck.Spec.Validator   as Validator
import           System.IO

import           Debug.Trace
printJson :: MonadIO m => String -> A.Value -> m ()
printJson key val = liftIO $ do
    putStr $ key ++ ": "
    LT.putStrLn $ LT.toLazyText $ A.encodePrettyToTextBuilder val



runApp :: CheckConfig -> IO ()
runApp CheckConfig{ targetURL = base, checkSmopeckFile = file} = do
    putStrLn "Hello World Check App!"
    manager <- newManager defaultManagerSettings
    res <- runExceptT $ do
        (typeEnv, endpoints) <- preprocess file
        forM_ endpoints $ \DesugarEndpoint{..} -> do
            liftIO $ putStrLn $ "Endpoint: " ++ show endpointMethod  ++ " " ++ endpointRoute
            param <- liftIO $ Constraint.mockJson typeEnv endpointParam
            printJson "Param" param
            let env = M.fromList [ ("parameter", param) ]
            request <- liftIO $ Constraint.mockJsonWithEnv typeEnv env endpointRequest
            printJson "Request" request
            let req = buildRequest base endpointRoute endpointMethod param request
            liftIO $ putStrLn $ "Request: " ++ show req
            res <- liftIO $ httpLbs req manager
            let response = fromResponse res
                env' = M.fromList [ ("parameter", param)
                                 , ("request", request)
                                 , ("response", response)]
            printJson "Response" response
            ExceptT $ pure $ Validator.validateJson typeEnv env' "response" endpointResponse
    case res of
        Left err -> hPutStrLn stderr err
        Right _  -> pure ()

fromResponse :: Response LBS.ByteString -> A.Value
fromResponse resp = A.object [
        "header" A..= headerObj,
        "body" A..= bodyObj
    ]
    where
    headers = responseHeaders resp
    headerObj :: A.Value
    headerObj =
        A.object $ do
            (key, value) <- headers
            let keyS = T.decodeUtf8 $ CI.original key
                valueS = T.decodeUtf8 value
            pure $ keyS A..= valueS
    contentType =
        headerObj ^? key "Content-Type"
         & fromMaybe "text/plain"
    body = responseBody resp
    bodyObj :: A.Value
    bodyObj =
        case contentType of
            "application/json" ->
                case A.eitherDecode' body of
                    Left err  -> error err
                    Right obj -> obj
            _ -> A.String $ T.decodeUtf8 $ LBS.toStrict body

buildRequest :: String -> Route -> StdMethod -> A.Value -> A.Value -> Request
buildRequest base route method paramObj reqObj =
    parseRequest_ (show method ++ " " ++ base)
     & setQueryString queryEnv
     & (\req -> req { requestBody = body,
                      requestHeaders = headers,
                      path = path req <> pathRoute })
    where
    body = RequestBodyLBS $ A.encode bodyObj
    pathRoute = substRoute paramEnv route
    paramEnv = M.fromList $ map (first T.unpack) pathParams
    queryEnv = map (bimap T.encodeUtf8 (Just . T.encodeUtf8)) queryParams
    headers :: [(CI BS.ByteString, BS.ByteString)]
    headers =
        reqObj ^.. key "header"
                 . members
                 . _String
                 . withIndex
                 . to (bimap (CI.mk . T.encodeUtf8) T.encodeUtf8)
    bodyObj = fromMaybe A.Null $ reqObj ^? key "body"
    pathParams =
        paramObj ^.. key "path"
                   . members
                   . _Primitive
                   . to toText
                   . withIndex
    queryParams =
        paramObj ^.. key "query"
                   . members
                   . _Primitive
                   . to toText
                   . withIndex
    toText (StringPrim t)   = t
    toText (NumberPrim v)   =
        case floatingOrInteger v of
            Left float -> T.pack $ show @Double float
            Right int  -> T.pack $ show @Integer int
    toText (BoolPrim True)  = "true"
    toText (BoolPrim False) = "false"
    toText NullPrim         = "null"


