{-# language CPP               #-}
{-# language DataKinds         #-}
{-# language FlexibleInstances #-}
{-# language TemplateHaskell   #-}
{-# language TypeOperators     #-}
{-|
Description : (Deprecated) Generate a set of Haskell types from a 'Schema'

This module is deprecated. Haskell types
corresponding to schema types should be
written manually.
-}
module Mu.Schema.Conversion.SchemaToTypes (
  generateTypesFromSchema
, Namer
) where

import           Control.Applicative
import           Data.Char
import qualified Data.Map                     as M
import           Data.SOP
import           GHC.Generics                 (Generic)
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype

import           Mu.Schema.Definition

-- | Generate the name from each new Haskell type
--   from the name given in the schema.
type Namer = String -> String

-- | Generates types to represent each of the types
--   in a given schema. You should call it as:
--   > $(generateTypesFromSchema f 'Schema)
--   where @f@ is a function @String -> String@
--   which obtains the Haskell name for a type
--   given the name in the schema. The second argument
--   is simply the name of the schema.
generateTypesFromSchema :: Namer -> Name -> Q [Dec]
generateTypesFromSchema namer schemaTyName
  = do let schemaTy = ConT schemaTyName
       schDef <- typeToSchemaDef schemaTy
       case schDef of
         Left e   -> fail $ "schema cannot be parsed: " ++ e
         Right sd -> concat <$> traverse (typeDefToDecl schemaTy namer) sd

-- Generation of types
-- ===================

typeDefToDecl :: Type -> Namer -> TypeDefB Type String String -> Q [Dec]
-- Records with one field
typeDefToDecl _schemaTy namer (DRecord name [f])
  = do let complete = completeName namer name
       fVar <- newName "f"
       d <- newtypeD (pure [])
                     (mkName complete)
                     [PlainTV fVar]
                     Nothing
                     (recC (mkName complete) [fieldDefToDecl namer complete fVar f])
                     [derivClause Nothing [conT ''Generic]]
       _wTy <- VarT <$> newName "w"
       -- let hsi = generateHasSchemaInstance wTy schemaTy name complete (fieldMapping complete [f])
       return [d] -- , hsi]
-- Records with more than one field
typeDefToDecl _schemaTy namer (DRecord name fields)
  = do let complete = completeName namer name
       fVar <- newName "f"
       d <- dataD (pure [])
                  (mkName complete)
                  [PlainTV fVar]
                  Nothing
                  [recC (mkName complete) (map (fieldDefToDecl namer complete fVar) fields)]
                  [derivClause Nothing [conT ''Generic]]
       _wTy <- VarT <$> newName "w"
       -- let hsi = generateHasSchemaInstance wTy schemaTy name complete (fieldMapping complete fields)
       return [d] -- , hsi]
-- Enumerations
typeDefToDecl _schemaTy namer (DEnum name choices)
  = do let complete = completeName namer name
       fVar <- newName "f"
       d <- dataD (pure [])
                  (mkName complete)
                  [PlainTV fVar]
                  Nothing
                  [ recC (mkName (choiceName complete choicename)) []
                    | ChoiceDef choicename <- choices]
                  [derivClause Nothing [conT ''Eq, conT ''Ord, conT ''Show, conT ''Generic]]
       _wTy <- VarT <$> newName "w"
       -- let hsi = generateHasSchemaInstance wTy schemaTy name complete (choiceMapping complete choices)
       return [d] --, hsi]
-- Simple things
typeDefToDecl _ _ (DSimple _)
  = fail "DSimple is not supported"

fieldDefToDecl :: Namer -> String -> Name -> FieldDefB Type String String -> Q (Name, Bang, Type)
fieldDefToDecl namer complete fVar (FieldDef name [] ty)
  = return ( mkName (fieldName complete name)
           , Bang NoSourceUnpackedness NoSourceStrictness
            , AppT (VarT fVar) (fieldTypeToDecl namer fVar ty) )
fieldDefToDecl _ _ _ (FieldDef name args _)
  = fail $ "fields with arguments are not supported: " ++ name ++ " " ++ show (length args)

{- broken for now
generateBuiltinInstance :: Bool -> Type -> String -> Name -> Dec
generateBuiltinInstance withPrereq wTy complete className
#if MIN_VERSION_template_haskell(2,12,0)
  = StandaloneDerivD Nothing ctx ty
#else
  = StandaloneDerivD ctx ty

#endif
  where
    me  = ConT (mkName complete)
    ctx = [AppT (ConT className) (AppT wTy (AppT me wTy)) | withPrereq]
    ty  = AppT (ConT className) (AppT me wTy)
-}

{-
generateHasSchemaInstance :: Type -> Type -> String -> String -> Type -> Dec
generateHasSchemaInstance wTy schemaTy schemaName complete mapping
  = InstanceD Nothing [AppT (ConT ''Applicative) wTy]
              (AppT (AppT (AppT (AppT (ConT ''HasSchema)
                                      wTy )
                                      schemaTy )
                                      (LitT (StrTyLit schemaName)))
                                      (AppT (ConT (mkName complete)) wTy) )
#if MIN_VERSION_template_haskell(2,15,0)
              [TySynInstD (TySynEqn Nothing
                                    (AppT (AppT (AppT (AppT (ConT ''FieldMapping)
                                                      wTy )
                                                      schemaTy )
                                                      (LitT (StrTyLit schemaName)) )
                                                      (AppT (ConT (mkName complete)) wTy))
                                    mapping) ]
#else
              [TySynInstD ''FieldMapping
                          (TySynEqn [ wTy, schemaTy, LitT (StrTyLit schemaName)
                                    , AppT (ConT (mkName complete)) wTy ]
                                     mapping) ]
#endif
-}

{-
fieldMapping :: String -> [FieldDefB Type String String] -> Type
fieldMapping _complete [] = PromotedNilT
fieldMapping complete (FieldDef name _ : rest)
  = AppT (AppT PromotedConsT thisMapping) (fieldMapping complete rest)
  where thisMapping
          = AppT (AppT (PromotedT '(:->))
                       (LitT (StrTyLit (fieldName complete name))))
                       (LitT (StrTyLit name))

choiceMapping :: String -> [ChoiceDef String] -> Type
choiceMapping _complete [] = PromotedNilT
choiceMapping complete (ChoiceDef name : rest)
  = AppT (AppT PromotedConsT thisMapping) (choiceMapping complete rest)
  where thisMapping
          = AppT (AppT (PromotedT '(:->))
                       (LitT (StrTyLit (choiceName complete name))))
                       (LitT (StrTyLit name))
-}

-- Name manipulation
-- =================

completeName :: Namer -> String -> String
completeName namer name = firstUpper (namer (firstUpper name))

choiceName :: String -> String -> String
choiceName complete cname = firstUpper (complete ++ firstUpper cname)

fieldName :: String -> String -> String
fieldName complete fname = firstLower (complete ++ firstUpper fname)

firstUpper :: String -> String
firstUpper []       = error "Empty names are not allowed"
firstUpper (x:rest) = toUpper x : rest

firstLower :: String -> String
firstLower []       = error "Empty names are not allowed"
firstLower (x:rest) = toLower x : rest

fieldTypeToDecl :: Namer -> Name -> FieldTypeB Type String -> Type
fieldTypeToDecl _namer _fVar TNull
  = ConT ''()
fieldTypeToDecl _namer _fVar (TPrimitive t)
  = t
fieldTypeToDecl namer fVar (TSchematic nm)
  = AppT (ConT (mkName $ completeName namer nm)) (VarT fVar)
fieldTypeToDecl namer fVar (TOption t)
  = AppT (ConT ''Maybe) (fieldTypeToDecl namer fVar t)
fieldTypeToDecl namer fVar (TList t)
  = AppT ListT (fieldTypeToDecl namer fVar t)
fieldTypeToDecl namer fVar (TMap k v)
  = AppT (AppT (ConT ''M.Map) (fieldTypeToDecl namer fVar k)) (fieldTypeToDecl namer fVar v)
fieldTypeToDecl namer fVar (TUnion ts)
  = AppT (AppT (ConT ''NS) (ConT ''I)) (fieldTypeUnion namer fVar ts)

fieldTypeUnion :: Namer -> Name -> [FieldTypeB Type String] -> Type
fieldTypeUnion _ _fVar [] = PromotedNilT
fieldTypeUnion namer fVar (t:ts)
  = AppT (AppT PromotedConsT (fieldTypeToDecl namer fVar t)) (fieldTypeUnion namer fVar ts)

-- Parsing
-- =======

typeToSchemaDef :: Type -> Q (Either String (SchemaB Type String String))
typeToSchemaDef toplevelty
  = typeToSchemaDef' <$> resolveTypeSynonyms toplevelty
  where
    typeToSchemaDef' :: Type -> Either String (SchemaB Type String String)
    typeToSchemaDef' expanded
      = do types <- tyList expanded
           mapM typeToTypeDef types

    typeToTypeDef, typeToRecordDef, typeToEnumDef, typeToSimpleType
      :: Type -> Either String (TypeDefB Type String String)
    typeToTypeDef t
      = typeToRecordDef t <|> typeToEnumDef t <|> typeToSimpleType t
    typeToRecordDef t
      = do (nm, fields) <- tyD2 'DRecord t
           DRecord <$> tyString nm
                   <*> (mapM typeToFieldDef =<< tyList fields)
    typeToEnumDef t
      = do (nm, choices) <- tyD2 'DEnum t
           DEnum <$> tyString nm
                 <*> (mapM typeToChoiceDef =<< tyList choices)
    typeToSimpleType t
      = do innerT <- tyD1 'DSimple t
           DSimple <$> typeToFieldType innerT

    typeToFieldDef :: Type -> Either String (FieldDefB Type String String)
    typeToFieldDef t
      = do (nm, args, innerTy) <- tyD3 'FieldDef t
           case args of
             PromotedNilT
               -> FieldDef <$> tyString nm
                           <*> pure []
                           <*> typeToFieldType innerTy
             SigT PromotedNilT _
               -> FieldDef <$> tyString nm
                           <*> pure []
                           <*> typeToFieldType innerTy
             t -> Left $ "fields with arguments are not supported: " ++ show t

    typeToChoiceDef :: Type -> Either String (ChoiceDef String)
    typeToChoiceDef t
      = do nm <- tyD1 'ChoiceDef t
           ChoiceDef <$> tyString nm

    typeToFieldType :: Type -> Either String (FieldTypeB Type String)
    typeToFieldType t
      =     TNull <$ tyD0 'TNull t
        <|> TPrimitive <$> tyD1 'TPrimitive t
        <|> (do sch <- tyD1 'TSchematic t
                TSchematic <$> tyString sch)
        <|> (do inner <- tyD1 'TOption t
                TOption <$> typeToFieldType inner)
        <|> (do inner <- tyD1 'TList t
                TList <$> typeToFieldType inner)
        <|> (do (k,v) <- tyD2 'TMap t
                TMap <$> typeToFieldType k <*> typeToFieldType v)
        <|> (do inners <- tyD1 'TUnion t
                TUnion <$> (mapM typeToFieldType =<< tyList inners))

tyString :: Type -> Either String String
tyString (SigT t _)
  = tyString t
tyString (LitT (StrTyLit s))
  = return s
tyString _
  = Left "expected string"

tyList :: Type -> Either String [Type]
tyList (SigT t _)
  = tyList t
tyList PromotedNilT
  = return []
tyList (AppT (AppT PromotedConsT ty) rest)
  = (ty :) <$> tyList rest
tyList _ = Left "expected list"

tyD0 :: Name -> Type -> Either String ()
tyD0 name (SigT t _) = tyD0 name t
tyD0 name (PromotedT c)
  | c == name = return ()
  | otherwise = Left $ "expected head " ++ show name ++ " but got " ++ show c
tyD0 _ _ = Left "expected type with no arguments"

tyD1 :: Name -> Type -> Either String Type
tyD1 name (SigT t _) = tyD1 name t
tyD1 name (AppT (PromotedT c) x)
  | c == name = return x
  | otherwise = Left $ "expected head " ++ show name ++ " but got " ++ show c
tyD1 _ _ = Left "expected type with 1 argument"

tyD2 :: Name -> Type -> Either String (Type, Type)
tyD2 name (SigT t _) = tyD2 name t
tyD2 name (AppT (AppT (PromotedT c) x) y)
  | c == name = return (x, y)
  | otherwise = Left $ "expected head " ++ show name ++ " but got " ++ show c
tyD2 _ _ = Left "expected type with 2 arguments"

tyD3 :: Name -> Type -> Either String (Type, Type, Type)
tyD3 name (SigT t _) = tyD3 name t
tyD3 name (AppT (AppT (AppT (PromotedT c) x) y) z)
  | c == name = return (x, y, z)
  | otherwise = Left $ "expected head " ++ show name ++ " but got " ++ show c
tyD3 _ _ = Left "expected type with 3 arguments"

instance Alternative (Either String) where
  empty = Left "error"
  Left x <|> Left y = Left (x ++ "\n" ++ y)
  Right x <|> _ = Right x
  _ <|> Right y = Right y
