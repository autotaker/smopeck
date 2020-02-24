{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Smopeck.Spec.Desugar where

import qualified Data.Map             as M
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

desugarTypeExp :: TypeExp Parsed head -> TypeExp Desugar head
desugarTypeExp = fmap desugarTypeExpF

desugarTypeExpF :: TypeExpF Parsed head -> TypeExpF Desugar head
desugarTypeExpF ty = ty {
    typeExpBind = BindDebrujin,
    typeExpExt = desugarTypeExt (typeExpExt ty),
    typeExpRef = desugarTypeRef (typeExpRef ty)
}

desugarTypeExt :: TypeExtension Parsed -> TypeExtension Desugar
desugarTypeExt = undefined

desugarTypeRef :: TypeRefine Parsed -> TypeRefine Desugar
desugarTypeRef = undefined
