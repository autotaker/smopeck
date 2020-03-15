# Validation (Type Checking)

```haskell
validate ::  TypeEnv -> Map ALocation Literal -> ALocation -> TypeExp -> Except String ()
parsePrim :: String -> Primitive -> Except String Literal
parsePrim "hoge" PString `shouldBe` pure (LString "hoge")
parsePrim "1.0" PNumber `shouldBe` pure (LNumber 1.0)
parsePrim "42" PInt `shouldBe` pure (LNumber 42.0)
parsePrim "hoge" PNumber `shouldBe` throwError "cannot parse \"hoge\" as a number"
```