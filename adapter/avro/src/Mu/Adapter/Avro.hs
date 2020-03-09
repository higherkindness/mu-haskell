{-# language ConstraintKinds       #-}
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language RankNTypes            #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Description : Adapter for Avro serialization

Just import the module and you can turn any
value with a 'ToSchema' and 'FromSchema' from
and to Avro values.
-}
module Mu.Adapter.Avro () where

import           Control.Arrow                       ((***))
import qualified Data.Avro                           as A
import qualified Data.Avro.Schema                    as ASch
import qualified Data.Avro.Types.Value               as AVal
-- 'Tagged . unTagged' can be replaced by 'coerce'
-- eliminating some run-time overhead
import           Data.Coerce                         (coerce)
import qualified Data.HashMap.Strict                 as HM
import           Data.List.NonEmpty                  (NonEmpty (..))
import qualified Data.List.NonEmpty                  as NonEmptyList
import qualified Data.Map                            as M
import           Data.Tagged
import qualified Data.Text                           as T
import qualified Data.Vector                         as V
import           GHC.TypeLits

import           Mu.Schema
import qualified Mu.Schema.Interpretation.Schemaless as SLess

instance SLess.ToSchemalessTerm (AVal.Value t) where
  toSchemalessTerm (AVal.Record _ r)
    = SLess.TRecord $ map (\(k,v) -> SLess.Field k (SLess.toSchemalessValue v))
                    $ HM.toList r
  toSchemalessTerm (AVal.Enum _ i _)
    = SLess.TEnum i
  toSchemalessTerm (AVal.Union _ _ v)
    = SLess.toSchemalessTerm v
  toSchemalessTerm v = SLess.TSimple (SLess.toSchemalessValue v)

instance SLess.ToSchemalessValue (AVal.Value t) where
  toSchemalessValue AVal.Null        = SLess.FNull
  toSchemalessValue (AVal.Boolean b) = SLess.FPrimitive b
  toSchemalessValue (AVal.Int b)     = SLess.FPrimitive b
  toSchemalessValue (AVal.Long b)    = SLess.FPrimitive b
  toSchemalessValue (AVal.Float b)   = SLess.FPrimitive b
  toSchemalessValue (AVal.Double b)  = SLess.FPrimitive b
  toSchemalessValue (AVal.String b)  = SLess.FPrimitive b
  toSchemalessValue (AVal.Fixed _ b) = SLess.FPrimitive b
  toSchemalessValue (AVal.Bytes b)   = SLess.FPrimitive b
  toSchemalessValue (AVal.Array v)
    = SLess.FList $ map SLess.toSchemalessValue $ V.toList v
  toSchemalessValue (AVal.Map hm)
    = SLess.FMap $ M.fromList
                 $ map (SLess.FPrimitive *** SLess.toSchemalessValue)
                 $ HM.toList hm
  toSchemalessValue (AVal.Union _ _ v)
    = SLess.toSchemalessValue v
  toSchemalessValue r@(AVal.Record _ _)
    = SLess.FSchematic (SLess.toSchemalessTerm r)
  toSchemalessValue e@AVal.Enum {}
    = SLess.FSchematic (SLess.toSchemalessTerm e)

instance A.HasAvroSchema (Term sch (sch :/: sty))
         => A.HasAvroSchema (WithSchema sch sty t) where
  schema = coerce $ A.schema @(Term sch (sch :/: sty))
instance ( FromSchema sch sty t
         , A.FromAvro (Term sch (sch :/: sty)) )
         => A.FromAvro (WithSchema sch sty t) where
  fromAvro v = WithSchema . fromSchema' @_ @_ @sch <$> A.fromAvro v
instance ( ToSchema sch sty t
         , A.ToAvro (Term sch (sch :/: sty)) )
         => A.ToAvro (WithSchema sch sty t) where
  toAvro (WithSchema v) = A.toAvro (toSchema' @_ @_ @sch v)

-- HasAvroSchema instances

class HasAvroSchema' x where
  schema' :: [ASch.TypeName] -> Tagged x ASch.Schema

instance HasAvroSchema' (Term sch t)
         => A.HasAvroSchema (Term sch t) where
  schema = schema' []
instance HasAvroSchema' (FieldValue sch t)
         => A.HasAvroSchema (FieldValue sch t) where
  schema = schema' []

instance (KnownName name, HasAvroSchemaFields sch args)
         => HasAvroSchema' (Term sch ('DRecord name args)) where
  schema' visited
    = if recordName `elem` visited
         then Tagged $ ASch.NamedType recordName
         else Tagged $ ASch.Record recordName [] Nothing Nothing fields
    where recordName = nameTypeName (Proxy @name)
          fields = schemaF (Proxy @sch) (Proxy @args) visited
instance (KnownName name, HasAvroSchemaEnum choices)
          => HasAvroSchema' (Term sch ('DEnum name choices)) where
  schema' visited
    = if enumName `elem` visited
         then Tagged $ ASch.NamedType enumName
         else Tagged $ ASch.mkEnum enumName [] Nothing choicesNames
    where enumName = nameTypeName (Proxy @name)
          choicesNames = schemaE (Proxy @choices)
instance HasAvroSchema' (FieldValue sch t)
         => HasAvroSchema' (Term sch ('DSimple t)) where
  schema' visited = coerce $ schema' @(FieldValue sch t) visited

instance HasAvroSchema' (FieldValue sch 'TNull) where
  schema' _ = Tagged ASch.Null
instance A.HasAvroSchema t
         => HasAvroSchema' (FieldValue sch ('TPrimitive t)) where
  schema' _ = coerce $ A.schema @t
instance (HasAvroSchema' (Term sch (sch :/: t)))
         => HasAvroSchema' (FieldValue sch ('TSchematic t)) where
  schema' visited = coerce $ schema' @(Term sch (sch :/: t)) visited
instance forall sch choices.
         HasAvroSchemaUnion (FieldValue sch) choices
         => HasAvroSchema' (FieldValue sch ('TUnion choices)) where
  schema' visited
    = Tagged $ ASch.mkUnion $ schemaU (Proxy @(FieldValue sch)) (Proxy @choices) visited
instance HasAvroSchema' (FieldValue sch t)
         => HasAvroSchema' (FieldValue sch ('TOption t)) where
  schema' visited
    = Tagged $ ASch.mkUnion $ ASch.Null :| [iSchema]
    where iSchema = unTagged $ schema' @(FieldValue sch t) visited
instance HasAvroSchema' (FieldValue sch t)
         => HasAvroSchema' (FieldValue sch ('TList t)) where
  schema' visited
    = Tagged $ ASch.Array iSchema
    where iSchema = unTagged $ schema' @(FieldValue sch t) visited
-- These are the only two versions of Map supported by the library
instance HasAvroSchema' (FieldValue sch v)
         => HasAvroSchema' (FieldValue sch ('TMap ('TPrimitive T.Text) v)) where
  schema' visited
    = Tagged $ ASch.Map iSchema
    where iSchema = unTagged $ schema' @(FieldValue sch v) visited
instance HasAvroSchema' (FieldValue sch v)
         => HasAvroSchema' (FieldValue sch ('TMap ('TPrimitive String) v)) where
  schema' visited
    = Tagged $ ASch.Map iSchema
    where iSchema = unTagged $ schema' @(FieldValue sch v) visited

class HasAvroSchemaUnion (f :: k -> *) (xs :: [k]) where
  schemaU :: Proxy f -> Proxy xs -> [ASch.TypeName] -> NonEmpty ASch.Schema
instance HasAvroSchema' (f v) => HasAvroSchemaUnion f '[v] where
  schemaU _ _ visited = vSchema :| []
    where vSchema = unTagged (schema' @(f v) visited)
instance (HasAvroSchema' (f x), HasAvroSchemaUnion f (y ': zs))
         => HasAvroSchemaUnion f (x ': y ': zs) where
  schemaU p _ visited = xSchema :| NonEmptyList.toList yzsSchema
    where xSchema = unTagged (schema' @(f x) visited)
          yzsSchema = schemaU p (Proxy @(y ': zs)) visited

class HasAvroSchemaFields sch (fs :: [FieldDef tn fn]) where
  schemaF :: Proxy sch -> Proxy fs -> [ASch.TypeName] -> [ASch.Field]
instance HasAvroSchemaFields sch '[] where
  schemaF _ _ _ = []
instance (KnownName name, HasAvroSchema' (FieldValue sch t), HasAvroSchemaFields sch fs)
         => HasAvroSchemaFields sch ('FieldDef name t ': fs) where
  schemaF psch _ visited = schemaThis : schemaF psch (Proxy @fs) visited
    where fieldName = nameText (Proxy @name)
          schemaT = unTagged $ schema' @(FieldValue sch t) visited
          schemaThis = ASch.Field fieldName [] Nothing Nothing schemaT Nothing

class HasAvroSchemaEnum (fs :: [ChoiceDef fn]) where
  schemaE :: Proxy fs -> [T.Text]
instance HasAvroSchemaEnum '[] where
  schemaE _ = []
instance (KnownName name, HasAvroSchemaEnum fs)
         => HasAvroSchemaEnum ('ChoiceDef name ': fs) where
  schemaE _ = nameText (Proxy @name) : schemaE (Proxy @fs)

-- FromAvro instances

instance (KnownName name, HasAvroSchemaFields sch args, FromAvroFields sch args)
         => A.FromAvro (Term sch ('DRecord name args)) where
  fromAvro (AVal.Record _ fields) = TRecord <$> fromAvroF fields
  fromAvro v                      = A.badValue v "record"
instance (KnownName name, HasAvroSchemaEnum choices, FromAvroEnum choices)
          => A.FromAvro (Term sch ('DEnum name choices)) where
  fromAvro v@(AVal.Enum _ n _) = TEnum <$> fromAvroEnum v n
  fromAvro v                   = A.badValue v "enum"
instance (HasAvroSchema' (FieldValue sch t), A.FromAvro (FieldValue sch t))
         => A.FromAvro (Term sch ('DSimple t)) where
  fromAvro v = TSimple <$> A.fromAvro v

instance A.FromAvro (FieldValue sch 'TNull) where
  fromAvro AVal.Null = pure FNull
  fromAvro v         = A.badValue v "null"
instance A.FromAvro t => A.FromAvro (FieldValue sch ('TPrimitive t)) where
  fromAvro v = FPrimitive <$> A.fromAvro v
instance ( KnownName t, HasAvroSchema' (Term sch (sch :/: t))
         , A.FromAvro (Term sch (sch :/: t)) )
         => A.FromAvro (FieldValue sch ('TSchematic t)) where
  fromAvro v = FSchematic <$> A.fromAvro v
instance (HasAvroSchemaUnion (FieldValue sch) choices, FromAvroUnion sch choices)
         => A.FromAvro (FieldValue sch ('TUnion choices)) where
  fromAvro (AVal.Union _ branch v) = FUnion <$> fromAvroU branch v
  fromAvro v                       = A.badValue v "union"
instance (HasAvroSchema' (FieldValue sch t), A.FromAvro (FieldValue sch t))
         => A.FromAvro (FieldValue sch ('TOption t)) where
  fromAvro v = FOption <$> A.fromAvro v
instance (HasAvroSchema' (FieldValue sch t), A.FromAvro (FieldValue sch t))
         => A.FromAvro (FieldValue sch ('TList t)) where
  fromAvro v = FList <$> A.fromAvro v
-- These are the only two versions of Map supported by the library
instance (HasAvroSchema' (FieldValue sch v), A.FromAvro (FieldValue sch v))
         => A.FromAvro (FieldValue sch ('TMap ('TPrimitive T.Text) v)) where
  fromAvro v = FMap . M.mapKeys FPrimitive <$> A.fromAvro v
instance (HasAvroSchema' (FieldValue sch v), A.FromAvro (FieldValue sch v))
         => A.FromAvro (FieldValue sch ('TMap ('TPrimitive String) v)) where
  fromAvro v = FMap . M.mapKeys (FPrimitive . T.unpack) <$> A.fromAvro v

class FromAvroEnum (vs :: [ChoiceDef fn]) where
  fromAvroEnum :: AVal.Value ASch.Schema -> Int -> A.Result (NS Proxy vs)
instance FromAvroEnum '[] where
  fromAvroEnum v _ = A.badValue v "element not found"
instance FromAvroEnum vs => FromAvroEnum (v ': vs) where
  fromAvroEnum _ 0 = pure (Z Proxy)
  fromAvroEnum v n = S <$> fromAvroEnum v (n-1)

class FromAvroUnion sch choices where
  fromAvroU :: ASch.Schema -> AVal.Value ASch.Schema
            -> ASch.Result (NS (FieldValue sch) choices)
instance FromAvroUnion sch '[] where
  fromAvroU _ v = A.badValue v "union choice not found"
instance (A.FromAvro (FieldValue sch u), FromAvroUnion sch us)
         => FromAvroUnion sch (u ': us) where
  fromAvroU branch v
    | ASch.matches branch (unTagged (A.schema @(FieldValue sch u)))
    = Z <$> A.fromAvro v
    | otherwise
    = S <$> fromAvroU branch v

class FromAvroFields sch (fs :: [FieldDef Symbol Symbol]) where
  fromAvroF :: HM.HashMap T.Text (AVal.Value ASch.Schema)
            -> A.Result (NP (Field sch) fs)
instance FromAvroFields sch '[] where
  fromAvroF _ = pure Nil
instance (KnownName name, A.FromAvro (FieldValue sch t), FromAvroFields sch fs)
         => FromAvroFields sch ('FieldDef name t ': fs) where
  fromAvroF v = case HM.lookup fieldName v of
                  Nothing -> A.badValue v "field not found"
                  Just f  -> (:*) <$> (Field <$> A.fromAvro f) <*> fromAvroF v
    where fieldName = nameText (Proxy @name)

-- ToAvro instances

instance (KnownName name, HasAvroSchemaFields sch args, ToAvroFields sch args)
         => A.ToAvro (Term sch ('DRecord name args)) where
  toAvro (TRecord fields) = AVal.Record wholeSchema (toAvroF fields)
    where wholeSchema = unTagged (A.schema @(Term sch ('DRecord name args)))
instance (KnownName name, HasAvroSchemaEnum choices, ToAvroEnum choices)
          => A.ToAvro (Term sch ('DEnum name choices)) where
  toAvro (TEnum n) = AVal.Enum wholeSchema choice text
    where wholeSchema = unTagged (A.schema @(Term sch ('DEnum name choices)))
          (choice, text) = toAvroE n
instance (HasAvroSchema' (FieldValue sch t), A.ToAvro (FieldValue sch t))
         => A.ToAvro (Term sch ('DSimple t)) where
  toAvro (TSimple v) = A.toAvro v

instance A.ToAvro (FieldValue sch 'TNull) where
  toAvro FNull = AVal.Null
instance A.ToAvro t => A.ToAvro (FieldValue sch ('TPrimitive t)) where
  toAvro (FPrimitive v) = A.toAvro v
instance ( KnownName t, HasAvroSchema' (Term sch (sch :/: t))
         , A.ToAvro (Term sch (sch :/: t)) )
         => A.ToAvro (FieldValue sch ('TSchematic t)) where
  toAvro (FSchematic v) = A.toAvro v
instance forall sch choices.
         (HasAvroSchemaUnion (FieldValue sch) choices, ToAvroUnion sch choices)
         => A.ToAvro (FieldValue sch ('TUnion choices)) where
  toAvro (FUnion v) = AVal.Union wholeSchema' chosenTy chosenVal
    where wholeSchema = schemaU (Proxy @(FieldValue sch)) (Proxy @choices) []
          wholeSchema' = V.fromList (NonEmptyList.toList wholeSchema)
          (chosenTy, chosenVal) = toAvroU v
instance (HasAvroSchema' (FieldValue sch t), A.ToAvro (FieldValue sch t))
         => A.ToAvro (FieldValue sch ('TOption t)) where
  toAvro (FOption v) = A.toAvro v
instance (HasAvroSchema' (FieldValue sch t), A.ToAvro (FieldValue sch t))
         => A.ToAvro (FieldValue sch ('TList t)) where
  toAvro (FList v) = AVal.Array $ V.fromList $ A.toAvro <$> v
-- These are the only two versions of Map supported by the library
instance (HasAvroSchema' (FieldValue sch v), A.ToAvro (FieldValue sch v))
         => A.ToAvro (FieldValue sch ('TMap ('TPrimitive T.Text) v)) where
  toAvro (FMap v) = A.toAvro $ M.mapKeys (\(FPrimitive k) -> k) v
instance (HasAvroSchema' (FieldValue sch v), A.ToAvro (FieldValue sch v))
         => A.ToAvro (FieldValue sch ('TMap ('TPrimitive String) v)) where
  toAvro (FMap v) = A.toAvro $ M.mapKeys (\(FPrimitive k) -> k) v

class ToAvroUnion sch choices where
  toAvroU :: NS (FieldValue sch) choices -> (ASch.Schema, AVal.Value ASch.Schema)
instance ToAvroUnion sch '[] where
  toAvroU _ = error "ToAvro in an empty union"
instance forall sch u us.
         (A.ToAvro (FieldValue sch u), ToAvroUnion sch us)
         => ToAvroUnion sch (u ': us) where
  toAvroU (Z v) = (unTagged (A.schema @(FieldValue sch u)), A.toAvro v)
  toAvroU (S n) = toAvroU n

class ToAvroEnum choices where
  toAvroE :: NS Proxy choices -> (Int, T.Text)
instance ToAvroEnum '[] where
  toAvroE = error "ToAvro in an empty enum"
instance (KnownName u, ToAvroEnum us)
         => ToAvroEnum ('ChoiceDef u ': us) where
  toAvroE (Z _) = (0, nameText (Proxy @u))
  toAvroE (S v) = let (n, t) = toAvroE v in (n + 1, t)

class ToAvroFields sch (fs :: [FieldDef Symbol Symbol]) where
  toAvroF :: NP (Field sch) fs -> HM.HashMap T.Text (AVal.Value ASch.Schema)
instance ToAvroFields sch '[] where
  toAvroF _ = HM.empty
instance (KnownName name, A.ToAvro (FieldValue sch t), ToAvroFields sch fs)
         => ToAvroFields sch ('FieldDef name t ': fs) where
  toAvroF (Field v :* rest) = HM.insert fieldName fieldValue (toAvroF rest)
    where fieldName  = nameText (Proxy @name)
          fieldValue = A.toAvro v

-- Conversion of symbols to other things
nameText :: KnownName s => proxy s -> T.Text
nameText = T.pack . nameVal
nameTypeName :: KnownName s => proxy s -> ASch.TypeName
nameTypeName = ASch.parseFullname . nameText
