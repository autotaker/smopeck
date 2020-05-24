{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Smopeck.App.Check (
    runApp
)where

import           Control.Monad.Except
import qualified Data.Map                as M
import           Network.HTTP.Client
import           Paths_smopeck
import           Smopeck.Config
import qualified Smopeck.Mock.Constraint as Constraint
import           Smopeck.Spec.Preprocess
import           System.IO


runApp :: CheckConfig -> IO ()
runApp CheckConfig{ targetAddr = TcpConfig {..}, checkSmopeckFile = file} = do
    putStrLn "Hello World Check App!"
    res <- runExceptT $ do
        (typeEnv, endpoints) <- preprocess file
        forM_ endpoints $ \DesugarEndpoint{..} -> do
            param <- liftIO $ Constraint.mockJson typeEnv endpointParam
            liftIO $ putStrLn $ "Param: " ++ show param
            let env = M.fromList [ ("parameter", param) ]
            request <- liftIO $ Constraint.mockJsonWithEnv typeEnv env endpointRequest
            liftIO $ putStrLn $ "Request: " ++ show request
    case res of
        Left err -> hPrint stderr err
        Right _  -> pure ()
