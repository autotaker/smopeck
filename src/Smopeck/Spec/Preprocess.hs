{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
module Smopeck.Spec.Preprocess where

import           Control.Monad.Except
import           Control.Monad.Logger
import qualified Data.ByteString.Char8 as BS
import           Data.Function
import qualified Data.Map              as M
import           Network.HTTP.Types
import           Paths_smopeck
import           Smopeck.Spec.Desugar
import           Smopeck.Spec.Lattice
import           Smopeck.Spec.Lexer
import           Smopeck.Spec.Parser
import           Smopeck.Spec.Syntax   hiding (Method, TypeExp)
import           Smopeck.Spec.TypeExp
import           System.IO

data DesugarEndpoint = DesugarEndpoint {
        endpointRoute    :: Route,
        endpointMethod   :: StdMethod,
        endpointParam    :: TypeExp Desugar WHNF,
        endpointRequest  :: TypeExp Desugar WHNF,
        endpointResponse :: TypeExp Desugar WHNF
    }

preprocess :: FilePath -> ExceptT String (LoggingT IO) (WHNFTypeEnv Desugar, [DesugarEndpoint])
preprocess file = do
    prelude <- liftIO $ getDataFileName "data/assets/prelude.spec"
    preludeContent <- liftIO $ readFile prelude
    content <- liftIO $ readFile file
    defsPrelude <- ExceptT (pure $ runAlex preludeContent $ runLexer parse)
    defsUser <- ExceptT (pure $ runAlex content $ runLexer parse)
    let defs = defsPrelude ++ defsUser
        typeEnv = M.fromList [ (tyName, def) | TypeDef tyName def <- defs ]
            & desugarTypeEnv
            & evalTypeEnv
        fType route method ext =
            LElem (TypeExpF (User "Endpoint") (BindName "it") (M.fromList ext) [] NoCond)
                & desugarTypeExp []
                & evalTypeExp typeEnv
                & (\case
                LElem TypeExpF{ typeExpExt = ext } ->
                    let tyRes = evalTypeExp typeEnv $ fst $ ext M.! FieldString "response"
                        tyParam = evalTypeExp typeEnv $ fst $ ext M.! FieldString "parameter"
                        tyReq = evalTypeExp typeEnv $ fst $ ext M.! FieldString "request"
                        stdMethod = case parseMethod (BS.pack method) of
                            Left err -> error $ show err
                            Right v  -> v
                        in
                    DesugarEndpoint route stdMethod tyParam tyReq tyRes)
        endpoints = [ fType route method ext | EndpointDef route method ext <- defs ]
    pure (typeEnv, endpoints)

