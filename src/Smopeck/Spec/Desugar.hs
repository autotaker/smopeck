{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module Smopeck.Spec.Desugar where

import           Data.List
import qualified Data.Map              as M
import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import qualified Smopeck.Spec.Lexer    as Lexer
import qualified Smopeck.Spec.Parser   as Parser
import           Smopeck.Spec.TypeExp

parseExpr :: String -> (Exp Parsed, String)
parseExpr s = case Lexer.runAlex s ((,) <$> Parser.runLexer Parser.parseExpr <*> (Lexer.remaining <$> Lexer.alexGetInput)) of
    Left err     -> error err
    Right result -> result

desugarExpF :: BindEnv -> ExpF Parsed (LocationExp Parsed) -> ExpF Desugar (LocationExp Desugar)
desugarExpF env = go
    where
        go (Literal l)   = desugarLiteral env l
        go (Var a)       = Var (desugarLocationExp env a)
        go (App op args) = App op (map go args)

desugarString :: BindEnv -> String -> ExpF Desugar (LocationExp Desugar)
desugarString env = App Add . go []
    where
    go :: String -> String -> [ExpF Desugar (LocationExp Desugar)]
    go acc ('$':'{':xs) =
        case parseExpr xs of
            (Exp expr, '}': xs') ->
                let exprF = desugarExpF env expr in
                Literal (LString (reverse acc)) : App (Func "str") [exprF] : go [] xs'
            (_, xs') -> error $ "syntax error expected '}' but found " ++ show xs'
    go acc (x:xs) = go (x:acc) xs
    go [] [] = []
    go acc [] = [Literal (LString (reverse acc))]

desugarLiteral :: BindEnv -> Literal Parsed -> ExpF Desugar (LocationExp Desugar)
desugarLiteral _ LNull           = Literal LNull
desugarLiteral _ (LBool b)       = Literal (LBool b)
desugarLiteral _ (LNumber n)     = Literal (LNumber n)
desugarLiteral _ (LString s)     = Literal (LString s)
desugarLiteral env (LDQString s) = desugarString env s
desugarLiteral _ (LRegex x)      = Literal (LRegex x)

type BindEnv = [String]

desugarTypeEnv :: DefaultTypeEnv Parsed -> DefaultTypeEnv Desugar
desugarTypeEnv = fmap (desugarTypeExp [])

desugarTypeExp :: BindEnv -> TypeExp Parsed HDefault -> TypeExp Desugar HDefault
desugarTypeExp env =
    fmapWithExt (desugarTypeExpF env) (\(HasCondF e a) -> HasCondF (desugarExp env e) a)




typeOfLiteral :: Literal mode -> Primitive
typeOfLiteral = \case
    LNumber _   -> PNumber
    LString _   -> PString
    LBool _     -> PBool
    LRegex _    -> PString
    LDQString _ -> PString
    LNull       -> PNull

desugarTypeExpF :: BindEnv -> TypeExpF Parsed head -> TypeExpF Desugar head
desugarTypeExpF env (LiteralType l) =
    TypeExpF {
        typeExpName = Prim (typeOfLiteral l),
        typeExpBind = BindDebrujin,
        typeExpExt = M.empty,
        typeExpRef = ref,
        typeExpCond = NoCond
    }
    where
        f op = [(op, Exp (desugarLiteral (".":env) l))]
        ref = case l of
            LNumber _   -> f Eq
            LString _   -> f Eq
            LDQString _ -> f Eq
            LBool _     -> f Eq
            LNull       -> []
            LRegex _    -> f Match
desugarTypeExpF env ty@TypeExpF{} = ty {
    typeExpBind = BindDebrujin,
    typeExpExt = desugarTypeExt (name:env) (typeExpExt ty),
    typeExpRef = desugarTypeRef (name:env) (typeExpRef ty),
    typeExpCond = NoCond
    } where BindName name = typeExpBind ty

desugarTypeExt :: BindEnv -> TypeExtension Parsed -> TypeExtension Desugar
desugarTypeExt env = M.fromList . map f . M.toList
    where
    f (FieldString s, ty) = (FieldString s, desugarTypeExp env ty)
    f (FieldIndex (BindName i), ty) =
        (FieldIndex BindDebrujin, desugarTypeExp (i:env) ty)

desugarTypeRef :: BindEnv -> TypeRefine Parsed -> TypeRefine Desugar
desugarTypeRef env = map (\(op, e) -> (op, desugarExp env e))

desugarExp :: BindEnv -> Exp Parsed -> Exp Desugar
desugarExp env (Exp e) = Exp $ desugarExpF env e

desugarLocationExp :: BindEnv -> LocationExp Parsed -> LocationExp Desugar
desugarLocationExp env = go
    where
        go (Root (Absolute s))
            | Just i <- elemIndex s env = Root (Relative i)
            | otherwise = Root (Absolute s)
        go (Root (Relative i)) = Root (Relative i)
        go (loc `Field` f) = go loc `Field` f
        go (loc `Get` e) = go loc `Get` desugarExp env e

