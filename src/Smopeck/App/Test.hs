{-# LANGUAGE LambdaCase #-}
module Smopeck.App.Test(
    runApp

)where

import           Control.Monad.Except
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Function
import qualified Data.Map                   as M
import           Smopeck.Config
import           Smopeck.Mock.Constraint
import           Smopeck.Spec.Desugar
import           Smopeck.Spec.Lexer
import           Smopeck.Spec.Parser
import           Smopeck.Spec.Syntax
import           Smopeck.Spec.TypeExp       (evalTypeEnv)
import           System.IO


runApp :: TestConfig -> IO ()
runApp config = runExceptT doit >>= \case
        Left err -> hPutStrLn stderr err
        Right () -> pure ()
    where
    doit :: ExceptT String IO ()
    doit = do
        liftIO $ hPrint stderr config
        content <- liftIO $ readFile (smopeckFile config)
        defs <- ExceptT (pure $ runAlex content (runLexer parse))

        liftIO $ mapM_ (hPrint stderr) defs
        let typeEnv = M.fromList [ (tyName, def) | TypeDef tyName def <- defs ]
                & desugarTypeEnv
                & evalTypeEnv
        liftIO $ mapM_ (hPrint stderr) $ M.assocs typeEnv
        mainType <- case M.lookup "Main" typeEnv of
            Nothing  -> throwError "Main type is not defined"
            Just def -> pure def
        v <- liftIO $ mockJson typeEnv mainType
        liftIO $ LBS.putStrLn $ A.encode v


