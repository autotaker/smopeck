{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Smopeck.App.Mock
    ( runApp
    , app
    )
where
import           Control.Monad.Except
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Function
import           Data.List                  (sortOn)
import qualified Data.Map                   as M
import           Data.String                (fromString)
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
                        let tyRes = ext M.! FieldString "response"
                            tyReq = ext M.! FieldString "request" in
                        (evalTypeExp typeEnv tyReq, evalTypeExp typeEnv tyRes))
            fMethod = BS.pack
        let endpoints = [ (route, fMethod method, fType ext) | EndpointDef route method ext <- defs ]
        liftIO $ Warp.runSettings settings (app typeEnv endpoints)
    case res of { Left err -> hPrint stderr err; Right () -> pure ()}

app :: TypeEnv -> [(Route, Method, (TypeExp, TypeExp))] -> Application
app env defs req respond = go (sortOn (\(a,_,_) -> -length a) defs)
    where
        go [] = responseNotFound
        go ((route,method,(tyReq, tyRes)):defs)
            | match route method = responseOk tyReq tyRes
            | otherwise = go defs
        responseOk tyReq tyRes = do
            v <- mockJson env tyRes
            respond
              $ responseLBS
                    status200
                    [("Content-Type", "application/json")]
                    (A.encode v)
        responseNotFound =
            respond
              $ responseLBS status404 [("Content-Type", "text/plain")] "Not Found"

        match route method = pathMatch && methodMatch
            where
            path = rawPathInfo req
            pathMatch = BS.pack route `BS.isPrefixOf` path
            methodMatch = requestMethod req == method


