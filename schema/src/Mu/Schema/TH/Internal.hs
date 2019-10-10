{-# language TemplateHaskell #-}
module Mu.Schema.TH.Internal where

import Control.Applicative
import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

import Mu.Schema.Definition
import Mu.Schema.Class

-- Generation of types
-- ===================

typeDefToDecl :: Type -> String -> String -> TypeDefB Type String String -> Q Dec
-- Records with one field
typeDefToDecl schemaTy prefix suffix (DRecord name _ [f])
  = newtypeD (pure [])
             (mkName completeName)
             []
             Nothing
             (pure (RecC (mkName completeName) [fieldDefToDecl f]))
             (deriveClauses schemaTy name)
  where
    completeName = prefix ++ firstUpper name ++ suffix
-- Records with more than one field
typeDefToDecl schemaTy prefix suffix (DRecord name _ fields)
  = dataD (pure [])
          (mkName completeName)
          []
          Nothing
          [pure (RecC (mkName completeName) (map fieldDefToDecl fields))]
          (deriveClauses schemaTy name)
  where
    completeName = prefix ++ firstUpper name ++ suffix


deriveClauses :: Type -> String -> [Q DerivClause]
deriveClauses schemaTy name
  = [ pure (DerivClause (Just StockStrategy) [ConT ''Eq, ConT ''Ord, ConT ''Show])
    , pure (DerivClause (Just AnyclassStrategy)
                        [AppT (AppT (ConT ''HasSchema) schemaTy) (LitT (StrTyLit name))]) ]

fieldDefToDecl :: FieldDefB Type String String -> (Name, Bang, Type)
fieldDefToDecl (FieldDef name _ ty)
  = ( mkName (firstLower name)
    , Bang NoSourceUnpackedness NoSourceStrictness
    , fieldTypeToDecl ty )

firstUpper :: String -> String
firstUpper (x:rest) = toUpper x : rest

firstLower :: String -> String
firstLower (x:rest) = toLower x : rest

fieldTypeToDecl :: FieldTypeB Type String -> Type
fieldTypeToDecl = undefined

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
tyString (LitT (StrTyLit s))
  = Just s
tyString _
  = Nothing

tyList :: Type -> Maybe [Type]
tyList PromotedNilT
  = Just []
tyList (AppT (AppT PromotedConsT ty) rest)
  = (ty :) <$> tyList rest
tyList _ = Nothing

tyD0 :: Name -> Type -> Maybe ()
tyD0 name (PromotedT c)
  | c == name = Just ()
  | otherwise = Nothing
tyD0 _ _ = Nothing

tyD1 :: Name -> Type -> Maybe Type
tyD1 name (AppT (PromotedT c) x)
  | c == name = Just x
  | otherwise = Nothing
tyD1 _ _ = Nothing

tyD2 :: Name -> Type -> Maybe (Type, Type)
tyD2 name (AppT (AppT (PromotedT c) x) y)
  | c == name = Just (x, y)
  | otherwise = Nothing
tyD2 _ _ = Nothing

tyD3 :: Name -> Type -> Maybe (Type, Type, Type)
tyD3 name (AppT (AppT (AppT (PromotedT c) x) y) z)
  | c == name = Just (x, y, z)
  | otherwise = Nothing
tyD3 _ _ = Nothing

tyD4 :: Name -> Type -> Maybe (Type, Type, Type, Type)
tyD4 name (AppT (AppT (AppT (AppT (PromotedT c) x) y) z) u)
  | c == name = Just (x, y, z, u)
  | otherwise = Nothing
tyD4 _ _ = Nothing