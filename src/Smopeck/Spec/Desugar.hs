{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Smopeck.Spec.Desugar where

import           Data.List
import qualified Data.Map              as M
import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import           Smopeck.Spec.TypeExp

desugarExpF :: ExpF Parsed a -> ExpF Desugar a
desugarExpF (Literal l)   = desugarLiteral l
desugarExpF (Var a)       = Var a
desugarExpF (App op args) = App op (map desugarExpF args)

desugarLiteral :: Literal Parsed -> ExpF Desugar a
desugarLiteral LNull         = Literal LNull
desugarLiteral (LBool b)     = Literal (LBool b)
desugarLiteral (LNumber n)   = Literal (LNumber n)
desugarLiteral (LString s)   = Literal (LString s)
desugarLiteral (LDQString s) = error "not yet implemented"
desugarLiteral (LRegex x)    = Literal (LRegex x)

type BindEnv = [String]

desugarTypeExp :: BindEnv -> TypeExp Parsed head -> TypeExp Desugar head
desugarTypeExp env = fmap (desugarTypeExpF env)

desugarTypeExpF :: BindEnv -> TypeExpF Parsed head -> TypeExpF Desugar head
desugarTypeExpF env ty = ty {
    typeExpBind = BindDebrujin,
    typeExpExt = desugarTypeExt (name:env) (typeExpExt ty),
    typeExpRef = desugarTypeRef (name:env) (typeExpRef ty)
    } where BindName name = typeExpBind ty

desugarTypeExt :: BindEnv -> TypeExtension Parsed -> TypeExtension Desugar
desugarTypeExt env = fmap (desugarTypeExp env)

desugarTypeRef :: BindEnv -> TypeRefine Parsed -> TypeRefine Desugar
desugarTypeRef env = map (\(op, Exp e) ->
    (op, Exp $ desugarExpF $ fmap (desugarLocationExp env) e))

desugarExp :: BindEnv -> Exp Parsed -> Exp Desugar
desugarExp env (Exp e) = Exp $ desugarExpF $ fmap (desugarLocationExp env) e

desugarLocationExp :: BindEnv -> LocationExp Parsed -> LocationExp Desugar
desugarLocationExp env = go
    where
        go (Root (Absolute s))
            | Just i <- elemIndex s env = Root (Relative i)
            | otherwise = Root (Absolute s)
        go (Root (Relative i)) = Root (Relative i)
        go (loc `Field` f) = go loc `Field` f
        go (loc `Get` e) = go loc `Get` desugarExp env e

