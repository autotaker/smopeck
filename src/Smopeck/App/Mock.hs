{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Smopeck.App.Mock
    ( runApp
    , app
    )
where
import           Control.Lens
import           Control.Monad.Except
import qualified Data.Aeson                 as A
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive       as CI
import           Data.Function
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (sortOn)
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.String                (fromString)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp   as Warp
import           Paths_smopeck
import           Smopeck.Config
import           Smopeck.Mock.Constraint
import           Smopeck.Spec.Desugar
import           Smopeck.Spec.Lexer
import           Smopeck.Spec.Parser
import           Smopeck.Spec.Syntax        hiding (Method, TypeEnv, TypeExp)
import           Smopeck.Spec.TypeExp       (BindName (..), TypeExpF (..),
                                             TypeName (..), evalTypeEnv,
                                             evalTypeExp)
import           Smopeck.Spec.Validator
import           System.IO


runApp :: MockConfig -> IO ()
runApp MockConfig{ listenAddr = TcpConfig {..}, mockSmopeckFile = file } = do
    let settings =
            Warp.defaultSettings
                & Warp.setPort tcpPort
                & Warp.setHost (fromString tcpHost)
    prelude <- getDataFileName "data/assets/prelude.spec"
    res <- runExceptT $ do
        preludeContent <- liftIO $ readFile prelude
        content <- liftIO $ readFile file :: ExceptT String IO String
        defs <- ExceptT (pure $ runAlex (preludeContent ++ content) $ runLexer parse)
        let typeEnv = M.fromList [ (tyName, def) | TypeDef tyName def <- defs ]
                & desugarTypeEnv
                & evalTypeEnv
        let fType ext =
                LElem (TypeExpF (User "Endpoint") (BindName "it") (M.fromList ext) [])
                  & desugarTypeExp []
                  & evalTypeExp typeEnv
                  & (\case
                    LElem TypeExpF{ typeExpExt = ext } ->
                        let tyRes = evalTypeExp typeEnv $ ext M.! FieldString "response"
                            tyParam = evalTypeExp typeEnv $ ext M.! FieldString "parameter"
                            tyReq = evalTypeExp typeEnv $ ext M.! FieldString "request" in
                        (tyParam, tyReq, tyRes))
            fMethod = BS.pack
        let endpoints = [ (route, fMethod method, fType ext) | EndpointDef route method ext <- defs ]
        liftIO $ Warp.runSettings settings (app typeEnv endpoints)
    case res of { Left err -> hPrint stderr err; Right () -> pure ()}

app :: TypeEnv -> [(Route, Method, (TypeExp, TypeExp, TypeExp))] -> Application
app env defs req respond = go (sortOn (\(a,_,_) -> -length a) defs)
    where
        go [] = responseNotFound
        go ((route,method,(tyParam, tyReq, tyRes)):defs)
            | Just param <- match route method tyParam = responseOk tyReq tyRes param
            | otherwise = go defs
        responseOk tyReq tyRes param = do
            let reqHeaders = A.toJSON headers
                headers = M.fromList [ (T.decodeUtf8 (CI.original key), T.decodeUtf8 val) | (key, val) <- requestHeaders req ]
                request = A.object [ "header" A..= reqHeaders ]
                valEnv = M.fromList [ ("request", request),
                                      ("parameter", param) ]
            reqBody <- case M.lookup "Content-Type" headers of
                Just "application/json" ->
                    fromJust . A.decodeStrict . LBS.toStrict <$> strictRequestBody req
                _ -> pure A.Null
            let request = A.object [ "header" A..= reqHeaders,
                                     "body" A..= (reqBody :: A.Value) ]
                valEnv = M.fromList [ ("request", request),
                                      ("parameter", param) ]
            case runExcept $ validateJson env valEnv "request" tyReq of
                Left err -> responseInvalid $ err ++ "\ntyReq: " ++ show tyReq ++ "\nenv: " ++ show valEnv
                Right () -> do
                    v <- mockJsonWithEnv env valEnv tyRes
                    let code = fromMaybe 200 $ v ^? key "status" . key "code" . _Integral
                        status =
                            maybe (toEnum code) (mkStatus code) $
                                v ^? key "status"
                                    . key "reason"
                                    . _String
                                    . to T.encodeUtf8
                        headers =
                                v ^.. key "header"
                                    . members
                                    . _String
                                    . withIndex
                                    . to (\(a,b) -> (fromString $ T.unpack a, T.encodeUtf8 b))
                        body = fromMaybe "null" $ v ^? key "body" . to A.encode
                    respond
                        $ responseLBS
                                status
                                headers
                                body
        responseNotFound =
            respond
              $ responseLBS status404 [("Content-Type", "text/plain")] "Not Found"
        responseInvalid err =
            respond
              $ responseLBS status400  [("Content-Type", "text/plain")] $ LBS.pack err


        match route method tyParam = do
                guard pathMatch
                guard methodMatch
                l <- forM (M.assocs queryExt) $ \(FieldString field, ty) -> do
                    let ty' = evalTypeExp env ty
                    mv <- M.lookup (T.pack field) param
                    v <- case mv of
                        Nothing -> pure ""
                        Just s  -> pure $ T.unpack s
                    case runExcept (parseParam v ty') of
                        Left _  -> Nothing
                        Right v -> pure $ (T.pack field) A..= v
                pure $ A.object [ "query" A..= A.object l ]
            where
            param = M.fromList [ (key , mvalue) | (key, mvalue) <- queryToQueryText $ queryString req ]
            path = rawPathInfo req
            LElem TypeExpF{ typeExpExt = ext } = tyParam
            LElem TypeExpF{ typeExpExt = queryExt } = evalTypeExp env $ ext  M.! FieldString "query"
            pathMatch = BS.pack route `BS.isPrefixOf` path
            methodMatch = requestMethod req == method


