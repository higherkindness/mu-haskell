{-# language TemplateHaskell, TypeOperators, DataKinds #-}
module Mu.Schema.TH where

import Control.Applicative
import Data.Char
import qualified Data.Map as M
import Data.SOP
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

import Mu.Schema.Definition
import Mu.Schema.Class

type Namer = String -> String

generateTypesFromSchema :: Namer -> Name -> Q [Dec]
generateTypesFromSchema namer schemaTyName
  = do let schemaTy = ConT schemaTyName
       schDef <- typeToSchemaDef schemaTy
       case schDef of
         Nothing -> fail "schema cannot be parsed"
         Just sd -> concat <$> traverse (typeDefToDecl schemaTy namer) sd

-- Generation of types
-- ===================

typeDefToDecl :: Type -> Namer -> TypeDefB Type String String -> Q [Dec]
-- Records with one field
typeDefToDecl schemaTy namer (DRecord name _ [f])
  = do let complete = completeName namer name
       d <- newtypeD (pure [])
                     (mkName complete)
                     []
                     Nothing
                     (pure (RecC (mkName complete) [fieldDefToDecl namer complete f]))
                     deriveClauses
       let hsi = generateHasSchemaInstance schemaTy name complete (fieldMapping complete [f])
       return [d, hsi]
-- Records with more than one field
typeDefToDecl schemaTy namer (DRecord name _ fields)
  = do let complete = completeName namer name
       d <- dataD (pure [])
                  (mkName complete)
                  []
                  Nothing
                  [pure (RecC (mkName complete) (map (fieldDefToDecl namer complete) fields))]
                  deriveClauses
       let hsi = generateHasSchemaInstance schemaTy name complete (fieldMapping complete fields)
       return [d, hsi]
-- Enumerations
typeDefToDecl schemaTy namer (DEnum name _ choices)
  = do let complete = completeName namer name
       d <- dataD (pure [])
                  (mkName complete)
                  []
                  Nothing
                  [ pure (RecC (mkName (choiceName complete choicename)) [])
                    | ChoiceDef choicename _ <- choices]
                  deriveClauses
       let hsi = generateHasSchemaInstance schemaTy name complete (choiceMapping complete choices)
       return [d, hsi]
-- Simple things
typeDefToDecl _ _ (DSimple _)
  = fail "DSimple is not supported"

deriveClauses :: [Q DerivClause]
deriveClauses
  = [ pure (DerivClause Nothing [ConT ''Eq, ConT ''Ord, ConT ''Show, ConT ''Generic]) ]
{- we need to add a field mapping
    , pure (DerivClause (Just AnyclassStrategy)
                        [AppT (AppT (ConT ''HasSchema) schemaTy) (LitT (StrTyLit name))]) ]
-}

fieldDefToDecl :: Namer -> String -> FieldDefB Type String String -> (Name, Bang, Type)
fieldDefToDecl namer complete (FieldDef name _ ty)
  = ( mkName (fieldName complete name)
    , Bang NoSourceUnpackedness NoSourceStrictness
    , fieldTypeToDecl namer ty )

generateHasSchemaInstance :: Type -> String -> String -> Type -> Dec
generateHasSchemaInstance schemaTy schemaName complete mapping
  = InstanceD Nothing []
              (AppT (AppT (AppT (ConT ''HasSchema) schemaTy)
                          (LitT (StrTyLit schemaName)))
                          (ConT (mkName complete)))
              [TySynInstD ''FieldMapping
                          (TySynEqn [schemaTy, LitT (StrTyLit schemaName), ConT (mkName complete)]
                                     mapping) ]

fieldMapping :: String -> [FieldDefB Type String String] -> Type
fieldMapping _complete [] = PromotedNilT
fieldMapping complete (FieldDef name _ _ : rest)
  = AppT (AppT PromotedConsT thisMapping) (fieldMapping complete rest)
  where thisMapping
          = AppT (AppT (PromotedT '(:->))
                       (LitT (StrTyLit (fieldName complete name))))
                       (LitT (StrTyLit name))

choiceMapping :: String -> [ChoiceDef String] -> Type
choiceMapping _complete [] = PromotedNilT
choiceMapping complete (ChoiceDef name _ : rest)
  = AppT (AppT PromotedConsT thisMapping) (choiceMapping complete rest)
  where thisMapping
          = AppT (AppT (PromotedT '(:->))
                       (LitT (StrTyLit (choiceName complete name))))
                       (LitT (StrTyLit name))

-- Name manipulation
-- =================

completeName :: Namer -> String -> String
completeName namer name = firstUpper (namer (firstUpper name))

choiceName :: String -> String -> String
choiceName complete cname = firstUpper (complete ++ firstUpper cname)

fieldName :: String -> String -> String
fieldName complete fname = firstLower (complete ++ firstUpper fname)

firstUpper :: String -> String
firstUpper [] = error "Empty names are not allowed"
firstUpper (x:rest) = toUpper x : rest

firstLower :: String -> String
firstLower [] = error "Empty names are not allowed"
firstLower (x:rest) = toLower x : rest

fieldTypeToDecl :: Namer -> FieldTypeB Type String -> Type
fieldTypeToDecl _namer TNull
  = ConT ''()
fieldTypeToDecl _namer (TPrimitive t)
  = t
fieldTypeToDecl namer (TSchematic nm)
  = ConT (mkName $ completeName namer nm)
fieldTypeToDecl namer (TOption t)
  = AppT (ConT ''Maybe) (fieldTypeToDecl namer t)
fieldTypeToDecl namer (TList t)
  = AppT ListT (fieldTypeToDecl namer t)
fieldTypeToDecl namer (TMap k v)
  = AppT (AppT (ConT ''M.Map) (fieldTypeToDecl namer k)) (fieldTypeToDecl namer v)
fieldTypeToDecl namer (TUnion ts)
  = AppT (AppT (ConT ''NS) (ConT ''I)) (fieldTypeUnion namer ts)

fieldTypeUnion :: Namer -> [FieldTypeB Type String] -> Type
fieldTypeUnion _ [] = PromotedNilT
fieldTypeUnion namer (t:ts)
  = AppT (AppT PromotedConsT (fieldTypeToDecl namer t)) (fieldTypeUnion namer ts)

-- Parsing
-- =======

typeToSchemaDef :: Type -> Q (Maybe (SchemaB Type String String))
typeToSchemaDef toplevelty
  = typeToSchemaDef' <$> resolveTypeSynonyms toplevelty
  where
    typeToSchemaDef' :: Type -> Maybe (SchemaB Type String String)
    typeToSchemaDef' expanded
      = do types <- tyList expanded
           mapM typeToTypeDef types
    
    typeToTypeDef, typeToRecordDef, typeToEnumDef, typeToSimpleType 
      :: Type -> Maybe (TypeDefB Type String String)
    typeToTypeDef t
      = typeToRecordDef t <|> typeToEnumDef t <|> typeToSimpleType t
    typeToRecordDef t
      = do (nm, _anns, fields) <- tyD3 'DRecord t
           DRecord <$> tyString nm
                   <*> pure []
                   <*> (mapM typeToFieldDef =<< tyList fields)
    typeToEnumDef t
      = do (nm, _anns, choices) <- tyD3 'DEnum t
           DEnum <$> tyString nm
                 <*> pure []
                 <*> (mapM typeToChoiceDef =<< tyList choices)
    typeToSimpleType t
      = do innerT <- tyD1 'DSimple t
           DSimple <$> typeToFieldType innerT

    typeToFieldDef :: Type -> Maybe (FieldDefB Type String String)
    typeToFieldDef t
      = do (nm, _anns, innerTy) <- tyD3 'FieldDef t
           FieldDef <$> tyString nm
                    <*> pure []
                    <*> typeToFieldType innerTy

    typeToChoiceDef :: Type -> Maybe (ChoiceDef String)
    typeToChoiceDef t
      = do (nm, _anns) <- tyD2 'ChoiceDef t
           ChoiceDef <$> tyString nm <*> pure []

    typeToFieldType :: Type -> Maybe (FieldTypeB Type String)
    typeToFieldType t
      =     TNull <$ tyD0 'TNull t
        <|> TPrimitive <$>tyD1 'TPrimitive t
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

tyString :: Type -> Maybe String
tyString (SigT t _)
  = tyString t
tyString (LitT (StrTyLit s))
  = Just s
tyString _
  = Nothing

tyList :: Type -> Maybe [Type]
tyList (SigT t _)
  = tyList t
tyList PromotedNilT
  = Just []
tyList (AppT (AppT PromotedConsT ty) rest)
  = (ty :) <$> tyList rest
tyList _ = Nothing

tyD0 :: Name -> Type -> Maybe ()
tyD0 name (SigT t _) = tyD0 name t
tyD0 name (PromotedT c)
  | c == name = Just ()
  | otherwise = Nothing
tyD0 _ _ = Nothing

tyD1 :: Name -> Type -> Maybe Type
tyD1 name (SigT t _) = tyD1 name t
tyD1 name (AppT (PromotedT c) x)
  | c == name = Just x
  | otherwise = Nothing
tyD1 _ _ = Nothing

tyD2 :: Name -> Type -> Maybe (Type, Type)
tyD2 name (SigT t _) = tyD2 name t
tyD2 name (AppT (AppT (PromotedT c) x) y)
  | c == name = Just (x, y)
  | otherwise = Nothing
tyD2 _ _ = Nothing

tyD3 :: Name -> Type -> Maybe (Type, Type, Type)
tyD3 name (SigT t _) = tyD3 name t
tyD3 name (AppT (AppT (AppT (PromotedT c) x) y) z)
  | c == name = Just (x, y, z)
  | otherwise = Nothing
tyD3 _ _ = Nothing

tyD4 :: Name -> Type -> Maybe (Type, Type, Type, Type)
tyD4 name (SigT t _) = tyD4 name t
tyD4 name (AppT (AppT (AppT (AppT (PromotedT c) x) y) z) u)
  | c == name = Just (x, y, z, u)
  | otherwise = Nothing
tyD4 _ _ = Nothing