{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Smopeck.Spec.Desugar where

import           Data.List
import qualified Data.Map              as M
import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import qualified Smopeck.Spec.Lexer    as Lexer
import qualified Smopeck.Spec.Parser   as Parser
import           Smopeck.Spec.TypeExp

parseExpr :: String -> (Exp Parsed, String)
parseExpr s = case Lexer.runAlex s ((,) <$> Parser.runLexer Parser.parseExpr <*> (Lexer.remaining <$> Lexer.alexGetInput)) of
    Left err     -> error err
    Right result -> result

desugarExpF :: ExpF Parsed a -> ExpF Desugar a
desugarExpF (Literal l)   = desugarLiteral l
desugarExpF (Var a)       = Var a
desugarExpF (App op args) = App op (map desugarExpF args)

desugarString :: BindEnv -> String -> Exp Desugar
desugarString env = collect . go []
    where
    collect xs = Exp (App Add xs)
    go :: String -> String -> [ExpF Desugar (LocationExp Desugar)]
    go acc ('$':'{':xs) =
        case parseExpr xs of
            (expr, '}': xs') ->
                let Exp exprF = desugarExp env expr in
                Literal (LString (reverse acc)) : exprF : go [] xs'
            (_, xs') -> error $ "syntax error expected '}' but found " ++ show xs'
    go acc (x:xs) = go (x:acc) xs
    go [] [] = []
    go acc [] = [Literal (LString (reverse acc))]

desugarLiteral :: Literal Parsed -> ExpF Desugar a
desugarLiteral LNull         = Literal LNull
desugarLiteral (LBool b)     = Literal (LBool b)
desugarLiteral (LNumber n)   = Literal (LNumber n)
desugarLiteral (LString s)   = Literal (LString s)
desugarLiteral (LDQString s) = error "not yet implemented"
desugarLiteral (LRegex x)    = Literal (LRegex x)

type BindEnv = [String]

desugarTypeEnv :: DefaultTypeEnv Parsed -> DefaultTypeEnv Desugar
desugarTypeEnv = fmap (desugarTypeExp [])

desugarTypeExp :: BindEnv -> TypeExp Parsed head -> TypeExp Desugar head
desugarTypeExp env = fmap (desugarTypeExpF env)

desugarTypeExpF :: BindEnv -> TypeExpF Parsed head -> TypeExpF Desugar head
desugarTypeExpF env ty = ty {
    typeExpBind = BindDebrujin,
    typeExpExt = desugarTypeExt (name:env) (typeExpExt ty),
    typeExpRef = desugarTypeRef (name:env) (typeExpRef ty)
    } where BindName name = typeExpBind ty

desugarTypeExt :: BindEnv -> TypeExtension Parsed -> TypeExtension Desugar
desugarTypeExt env = M.fromList . map f . M.toList
    where
    f (FieldString s, ty) = (FieldString s, desugarTypeExp env ty)
    f (FieldIndex (BindName i), ty) =
        (FieldIndex BindDebrujin, desugarTypeExp (i:env) ty)

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

