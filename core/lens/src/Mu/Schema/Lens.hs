{-# language AllowAmbiguousTypes    #-}
{-# language DataKinds              #-}
{-# language FlexibleContexts       #-}
{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language GADTs                  #-}
{-# language InstanceSigs           #-}
{-# language LambdaCase             #-}
{-# language PartialTypeSignatures  #-}
{-# language PolyKinds              #-}
{-# language QuantifiedConstraints  #-}
{-# language RankNTypes             #-}
{-# language ScopedTypeVariables    #-}
{-# language TypeApplications       #-}
{-# language TypeFamilies           #-}
{-# language TypeOperators          #-}
{-# language UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mu.Schema.Lens (
  record,
  is
) where

import           Control.Lens
import           Data.Kind
import           Data.Map
import           Data.SOP
import qualified Data.Text            as T


import           GHC.Int
import           GHC.OverloadedLabels
import           GHC.TypeLits         hiding (Nat)
import           Mu.Schema

is :: s -> APrism' s () -> Bool
is s l = not $ isn't l s

-- we need structurally inductive Nats
data Nat = Zero | Succ Nat

record :: BuildRecord sch args r => r -> Term sch ('DRecord name args)
record = TRecord . buildR

class BuildRecord (sch :: Schema Symbol Symbol) (args :: [FieldDef Symbol Symbol]) (r :: Type) | sch args -> r where
  buildR :: r -> NP (Field sch) args

instance
  {-# OVERLAPPABLE #-}
  ( Interpret sch fieldType ~ r,
    Uninterpret r ~ fieldType,
    UninterpretField sch r
  ) =>
  BuildRecord
    sch
    '[ 'FieldDef fieldName fieldType
     ]
    r
  where
  buildR val = Field (toFieldValue val) :* Nil

instance
  ( Interpret sch fieldType1 ~ v1,
    Interpret sch fieldType2 ~ v2,
    Uninterpret v1 ~ fieldType1,
    Uninterpret v2 ~ fieldType2,
    All (UninterpretField sch) '[v1, v2]
  ) =>
  BuildRecord
    sch
    '[ 'FieldDef fieldName1 fieldType1,
       'FieldDef fieldName2 fieldType2
     ]
    (v1, v2)
  where
  buildR (v1, v2) = Field (toFieldValue v1) :* Field (toFieldValue v2) :* Nil

instance
  ( Interpret sch fieldType1 ~ v1,
    Interpret sch fieldType2 ~ v2,
    Interpret sch fieldType3 ~ v3,
    Uninterpret v1 ~ fieldType1,
    Uninterpret v2 ~ fieldType2,
    Uninterpret v3 ~ fieldType3,
    All (UninterpretField sch) '[v1, v2, v3]
  ) =>
  BuildRecord
    sch
    '[ 'FieldDef fieldName1 fieldType1,
       'FieldDef fieldName2 fieldType2,
       'FieldDef fieldName3 fieldType3
     ]
    (v1, v2, v3)
  where
  buildR (v1, v2, v3) =
    Field (toFieldValue v1)
      :* Field (toFieldValue v2)
      :* Field (toFieldValue v3)
      :* Nil

instance
  ( Functor f,
    HasFieldIx (IndexOf fieldName fields) fields fields' fieldType fieldType',
    Interpret sch fieldType ~ fieldValue,
    Interpret sch fieldType' ~ fieldValue',
    Uninterpret fieldValue ~ fieldType,
    Uninterpret fieldValue' ~ fieldType',
    UninterpretField sch fieldValue'
  ) =>
  IsLabel
    fieldName
    ( (fieldValue -> f fieldValue') ->
      (Term sch ('DRecord name fields) -> f (Term sch ('DRecord name fields')))
    )
  where
  fromLabel = field @fieldName

instance
  forall choiceName p f sch name choiceDefs choiceDefs' choiceType choiceType'.
  ( Choice p,
    Applicative f,
    HasChoiceIx (ChoiceIndexOf choiceName choiceDefs) choiceDefs choiceDefs' choiceType choiceType'
  ) =>
  IsLabel
    choiceName
    ( p choiceType (f choiceType') ->
      p (Term sch ('DEnum name choiceDefs)) (f (Term sch ('DEnum name choiceDefs')))
    )
  where
  fromLabel = choose @choiceName

choose ::
  forall (choiceName :: Symbol) sch name choiceDefs choiceDefs' choiceType choiceType'.
  (HasChoiceIx (ChoiceIndexOf choiceName choiceDefs) choiceDefs choiceDefs' choiceType choiceType') =>
  Prism
    (Term sch ('DEnum name choiceDefs))
    (Term sch ('DEnum name choiceDefs'))
    choiceType
    choiceType'
choose = chooseIx @(ChoiceIndexOf choiceName choiceDefs)

class
  HasChoiceIx
    (choiceIndex :: Nat)
    (choiceDefs :: [ChoiceDef Symbol])
    (choiceDefs' :: [ChoiceDef Symbol])
    choiceType
    choiceType'
    | choiceIndex choiceDefs -> choiceType,
      choiceIndex choiceDefs' -> choiceType',
      choiceIndex choiceDefs choiceType' -> choiceDefs',
      choiceIndex choiceDefs' choiceType -> choiceDefs where
  chooseIx ::
    Prism
      (Term sch ('DEnum name choiceDefs))
      (Term sch ('DEnum name' choiceDefs'))
      choiceType
      choiceType'

instance
  HasChoiceIx
    'Zero
    ('ChoiceDef choiceName ': choiceDefs)
    ('ChoiceDef choiceName ': choiceDefs)
    ()
    ()
  where
  chooseIx f = dimap project (either pure (fmap inject)) (right' f)
    where
      inject :: () -> Term sch ('DEnum name ('ChoiceDef choiceName ': choiceDefs))
      inject () = TEnum (Z (Proxy @('ChoiceDef choiceName)))
      project ::
        Term sch ('DEnum name ('ChoiceDef choiceName ': choiceDefs)) ->
        Either (Term sch ('DEnum name' ('ChoiceDef choiceName' ': choiceDefs))) ()
      project term = case term of
        TEnum (Z Proxy) -> Right ()
        _               -> Left (TEnum (Z Proxy))

instance
  (HasChoiceIx choiceIndex choiceDefs choiceDefs' choiceType choiceType') =>
  HasChoiceIx
    ('Succ choiceIndex)
    (choiceDef ': choiceDefs)
    (choiceDef ': choiceDefs')
    choiceType
    choiceType'
  where
  chooseIx f =
    dimap
      project
      inject
      (right' (chooseIx @choiceIndex @choiceDefs @choiceDefs' f))
    where
      project ::
        Term sch ('DEnum name (choiceDef ': choiceDefs)) ->
        Either () (Term sch ('DEnum name choiceDefs))
      project (TEnum (Z Proxy)) = Left ()
      project (TEnum (S inner)) = Right (TEnum inner)
      inject ::
        Applicative f =>
        Either () (f (Term sch ('DEnum name choiceDefs'))) ->
        f (Term sch ('DEnum name (choiceDef ': choiceDefs')))
      inject (Left ())     = pure (TEnum (Z Proxy))
      inject (Right inner) = fmap wrap inner
      wrap :: Term sch ('DEnum name choiceDefs') -> Term sch ('DEnum name (choiceDef ': choiceDefs'))
      wrap (TEnum choices) = TEnum (S choices)

field ::
  forall fieldName sch name fieldDefs fieldDefs' fieldType fieldType' fieldValue fieldValue'.
  (HasFieldIx (IndexOf fieldName fieldDefs) fieldDefs fieldDefs' fieldType fieldType') =>
  ( HasFieldIx (IndexOf fieldName fieldDefs) fieldDefs fieldDefs' fieldType fieldType',
    Interpret sch fieldType ~ fieldValue,
    Interpret sch fieldType' ~ fieldValue',
    Uninterpret fieldValue ~ fieldType,
    Uninterpret fieldValue' ~ fieldType',
    UninterpretField sch fieldValue'
  ) =>
  Lens
    (Term sch ('DRecord name fieldDefs))
    (Term sch ('DRecord name fieldDefs'))
    fieldValue
    fieldValue'
field = fieldValueName @fieldName . interpretIso

fieldValueName ::
  forall fieldName sch name fieldDefs fieldDefs' fieldType fieldType'.
  (HasFieldIx (IndexOf fieldName fieldDefs) fieldDefs fieldDefs' fieldType fieldType') =>
  Lens
    (Term sch ('DRecord name fieldDefs))
    (Term sch ('DRecord name fieldDefs'))
    (FieldValue sch fieldType)
    (FieldValue sch fieldType')
fieldValueName = fieldValueIx @(IndexOf fieldName fieldDefs)

type family IndexOf (fieldName :: fieldNameKind) (fieldDefs :: [FieldDefB builtin fieldNameKind typeNameKind]) :: Nat where
  IndexOf fieldName ('FieldDef fieldName _ ': _) = 'Zero
  IndexOf fieldName (_ ': fieldDefs) = 'Succ (IndexOf fieldName fieldDefs)
  IndexOf fieldName '[] = TypeError ('Text "does not contain field name " ':<>: 'ShowType fieldName)

type family
  ChoiceIndexOf
    (choiceName :: choiceNameKind)
    (choiceDefs :: [ChoiceDef choiceNameKind]) ::
    Nat where
  ChoiceIndexOf choiceName ('ChoiceDef choiceName : _) = 'Zero
  ChoiceIndexOf choiceName (_ ': choiceDefs) = 'Succ (ChoiceIndexOf choiceName choiceDefs)

class
  HasFieldIx
    (fieldIndex :: Nat)
    (fieldDefs :: [FieldDef Symbol Symbol])
    (fieldDefs' :: [FieldDef Symbol Symbol])
    (fieldType :: FieldType Symbol)
    (fieldType' :: FieldType Symbol)
    | fieldIndex fieldDefs -> fieldType,
      fieldIndex fieldDefs' -> fieldType',
      fieldIndex fieldDefs fieldType' -> fieldDefs',
      fieldIndex fieldDefs' fieldType -> fieldDefs where
  fieldValueIx ::
    Lens
      (Term sch ('DRecord name fieldDefs))
      (Term sch ('DRecord name' fieldDefs'))
      (FieldValue sch fieldType)
      (FieldValue sch fieldType')

instance
  HasFieldIx
    'Zero
    ('FieldDef fieldName fieldType ': fieldDefs)
    ('FieldDef fieldName fieldType' ': fieldDefs)
    fieldType
    fieldType'
  where
  fieldValueIx f (TRecord (Field fieldValue :* fields)) = TRecord . (:* fields) . Field <$> f fieldValue

instance
  ( HasFieldIx
      fieldIndex
      fieldDefs
      fieldDefs'
      fieldType
      fieldType'
  ) =>
  HasFieldIx ('Succ fieldIndex) (fieldDef ': fieldDefs) (fieldDef ': fieldDefs') fieldType fieldType'
  where
  fieldValueIx f (TRecord (firstField :* restOfFields)) =
    wrap <$> fieldValueIx @fieldIndex f (TRecord restOfFields)
    where
      wrap (TRecord fields) = TRecord (firstField :* fields)

interpretIso ::
  ( Uninterpret (Interpret sch fieldType') ~ fieldType',
    UninterpretField sch (Interpret sch fieldType')
  ) =>
  Iso (FieldValue sch fieldType) (FieldValue sch fieldType') (Interpret sch fieldType) (Interpret sch fieldType')
interpretIso = dimap fromFieldValue (fmap toFieldValue)

type family Interpret (sch :: Schema typeName fieldName) (fieldType :: FieldType typeName) :: Type where
  Interpret _ 'TNull = ()
  Interpret _ ('TPrimitive builtin) = builtin
  Interpret sch ('TSchematic typeName) = Term sch (sch :/: typeName)
  Interpret sch ('TOption innerType) = Maybe (Interpret sch innerType)
  Interpret sch ('TList innerType) = [Interpret sch innerType]
  Interpret sch ('TMap keyType valueType) = Map (Interpret sch keyType) (Interpret sch valueType)
  Interpret sch ('TUnion choiceTypes) = NS Identity (InterpretList sch choiceTypes)

type family InterpretList sch (fieldTypes :: [FieldType typeName]) :: [Type] where
  InterpretList _ '[] = '[]
  InterpretList sch (t ': ts) = (Interpret sch t ': InterpretList sch ts)

fromFieldValue :: FieldValue sch fieldType -> Interpret sch fieldType
fromFieldValue = \case
  FNull                     -> ()
  (FPrimitive val)          -> val
  (FSchematic term)         -> term
  (FOption maybeFieldValue) -> fromFieldValue <$> maybeFieldValue
  (FList listFieldValues)   -> fromFieldValue <$> listFieldValues
  (FMap mapFieldValues)     -> mapKeysMonotonic fromFieldValue (fromFieldValue <$> mapFieldValues)
  (FUnion (Z val))          -> Z (Identity (fromFieldValue val))
  (FUnion (S val))          -> S (fromFieldValue (FUnion val))

class UninterpretField sch a where
  type Uninterpret a :: FieldType typeName
  toFieldValue :: a -> FieldValue sch (Uninterpret a)

instance UninterpretField sch () where
  type Uninterpret () = 'TNull
  toFieldValue () = FNull

instance UninterpretField sch Integer where
  type Uninterpret Integer = 'TPrimitive Integer
  toFieldValue = FPrimitive

instance UninterpretField sch Int32 where
  type Uninterpret Int32 = 'TPrimitive Int32
  toFieldValue = FPrimitive

instance UninterpretField sch Int where
  type Uninterpret Int = 'TPrimitive Int
  toFieldValue = FPrimitive

instance UninterpretField sch T.Text where
  type Uninterpret T.Text = 'TPrimitive T.Text
  toFieldValue = FPrimitive

instance
  ((sch :/: recordName) ~ 'DRecord recordName fieldDefs, sch ~ sch') =>
  UninterpretField sch (Term sch' ('DRecord recordName fieldDefs))
  where
  type Uninterpret (Term sch' ('DRecord recordName fieldDefs)) = 'TSchematic recordName
  toFieldValue = FSchematic

instance
  ((sch :/: enumName) ~ 'DEnum enumName choiceDefs) =>
  UninterpretField sch (Term sch ('DEnum enumName choiceDefs))
  where
  type Uninterpret (Term sch ('DEnum enumName choiceDefs)) = 'TSchematic enumName
  toFieldValue = FSchematic

instance (UninterpretField sch a) => UninterpretField sch (Maybe a) where
  type Uninterpret (Maybe a) = 'TOption (Uninterpret a)
  toFieldValue = FOption . fmap toFieldValue

instance (UninterpretField sch a) => UninterpretField sch [a] where
  type Uninterpret [a] = 'TList (Uninterpret a)
  toFieldValue = FList . fmap toFieldValue

instance
  (Ord (FieldValue sch (Uninterpret k)), UninterpretField sch k, UninterpretField sch v) =>
  UninterpretField sch (Map k v)
  where
  type Uninterpret (Map k v) = 'TMap (Uninterpret k) (Uninterpret v)
  toFieldValue = FMap . mapKeysMonotonic toFieldValue . fmap toFieldValue

instance
  (All (UninterpretField sch) choiceTypes) =>
  UninterpretField sch (NS Identity (choiceTypes :: [Type]))
  where
  type Uninterpret (NS Identity choiceTypes) = 'TUnion (UninterpretList choiceTypes)
  toFieldValue = FUnion . nsToFieldValues

nsToFieldValues ::
  forall sch choiceTypes.
  (All (UninterpretField sch) choiceTypes) =>
  NS Identity choiceTypes ->
  NS (FieldValue sch) (UninterpretList choiceTypes)
nsToFieldValues = \case
  (Z val) -> Z . toFieldValue . runIdentity $ val
  (S val) -> S (nsToFieldValues val)

type family UninterpretList (as :: [Type]) :: [FieldType typeName] where
  UninterpretList '[] = '[]
  UninterpretList (t ': ts) = Uninterpret t ': UninterpretList ts
