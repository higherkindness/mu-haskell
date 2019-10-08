{-# language PolyKinds, DataKinds, GADTs, 
             FlexibleInstances, FlexibleContexts,
             TypeApplications, TypeOperators,
             ScopedTypeVariables, RankNTypes,
             MultiParamTypeClasses,
             UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mu.Schema.Adapter.Avro where

import Control.Arrow ((***))
import qualified Data.Avro as A
import qualified Data.Avro.Schema as ASch
import qualified Data.Avro.Types.Value as AVal
-- 'Tagged . unTagged' can be replaced by 'coerce'
-- eliminating some run-time overhead
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmptyList
import qualified Data.Map as M
import Data.SOP (NP(..), NS(..))
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.TypeLits

import Mu.Schema
import qualified Mu.Schema.Schemaless as SLess

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
  toSchemalessValue (AVal.Null)      = SLess.FNull
  toSchemalessValue (AVal.Boolean b) = SLess.FPrimitive b
  toSchemalessValue (AVal.Int b)     = SLess.FPrimitive b
  toSchemalessValue (AVal.Long b)    = SLess.FPrimitive b
  toSchemalessValue (AVal.Float b)   = SLess.FPrimitive b
  toSchemalessValue (AVal.Double b)  = SLess.FPrimitive b
  toSchemalessValue (AVal.String b)  = SLess.FPrimitive b
  toSchemalessValue (AVal.Fixed _ b) = SLess.FPrimitive b
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
  toSchemalessValue e@(AVal.Enum _ _ _)
    = SLess.FSchematic (SLess.toSchemalessTerm e)

instance forall sch sty t.
         HasAvroSchemas sch sch
         => A.HasAvroSchema (WithSchema sch sty t) where
  -- the previous iteration added only the schema of the type
  -- schema = coerce $ A.schema @(Term sch (sch :/: sty))
  -- but now we prefer to have all of them
  schema = Tagged $ ASch.Union (schemas (Proxy @sch) (Proxy @sch))
instance (HasSchema sch sty t, HasAvroSchemas sch sch, A.FromAvro (Term sch (sch :/: sty)))
         => A.FromAvro (WithSchema sch sty t) where
  fromAvro (AVal.Union _ _ v) = WithSchema . fromSchema' @sch <$> A.fromAvro v
  fromAvro v = ASch.badValue v "top-level"
instance forall sch sty t.
         (HasSchema sch sty t, HasAvroSchemas sch sch, A.ToAvro (Term sch (sch :/: sty)))
         => A.ToAvro (WithSchema sch sty t) where
  toAvro (WithSchema v) = AVal.Union (schemas (Proxy @sch) (Proxy @sch))
                                     (unTagged $ A.schema @(Term sch (sch :/: sty)))
                                     (A.toAvro (toSchema' @sch v))

class HasAvroSchemas (r :: Schema tn fn) (sch :: Schema tn fn) where
  schemas :: Proxy r -> Proxy sch -> V.Vector ASch.Type
instance HasAvroSchemas r '[] where
  schemas _ _ = V.empty
instance forall r d ds.
         (A.HasAvroSchema (Term r d), HasAvroSchemas r ds)
         => HasAvroSchemas r (d ': ds) where
  schemas pr _ = V.cons thisSchema (schemas pr (Proxy @ds))
    where thisSchema = unTagged $ A.schema @(Term r d)

-- HasAvroSchema instances

instance (KnownName name, HasAvroSchemaFields sch args)
         => A.HasAvroSchema (Term sch ('DRecord name args)) where
  schema = Tagged $ ASch.Record recordName [] Nothing Nothing fields
    where recordName = nameTypeName (Proxy @name)
          fields = schemaF (Proxy @sch) (Proxy @args)
instance forall sch name choices.
         (KnownName name, KnownNames choices)
          => A.HasAvroSchema (Term sch ('DEnum name choices)) where
  schema = Tagged $ ASch.mkEnum enumName [] Nothing choicesNames
    where enumName = nameTypeName (Proxy @name)
          choicesNames = symbols nameText (Proxy @choices)
instance forall sch t.
         A.HasAvroSchema (FieldValue sch t)
         => A.HasAvroSchema (Term sch ('DSimple t)) where
  schema = coerce $ A.schema @(FieldValue sch t)

instance A.HasAvroSchema (FieldValue sch 'TNull) where
  schema = Tagged ASch.Null
instance forall sch t. A.HasAvroSchema t
         => A.HasAvroSchema (FieldValue sch ('TPrimitive t)) where
  schema = coerce $ A.schema @t
instance forall sch t.
         -- A.HasAvroSchema (Term sch (sch :/: t))
         KnownName t
         => A.HasAvroSchema (FieldValue sch ('TSchematic t)) where
  -- schema = coerce $ A.schema @(Term sch (sch :/: t))
  schema = Tagged $ ASch.NamedType (nameTypeName (Proxy @t))
instance forall sch choices.
         HasAvroSchemaUnion (FieldValue sch) choices
         => A.HasAvroSchema (FieldValue sch ('TUnion choices)) where
  schema = Tagged $ ASch.mkUnion $ schemaU (Proxy @(FieldValue sch)) (Proxy @choices)
instance A.HasAvroSchema (FieldValue sch t)
         => A.HasAvroSchema (FieldValue sch ('TOption t)) where
  schema = coerce $ A.schema @(Maybe (FieldValue sch t))
instance A.HasAvroSchema (FieldValue sch t)
         => A.HasAvroSchema (FieldValue sch ('TList t)) where
  schema = coerce $ A.schema @[FieldValue sch t]
-- These are the only two versions of Map supported by the library
instance A.HasAvroSchema (FieldValue sch v)
         => A.HasAvroSchema (FieldValue sch ('TMap ('TPrimitive T.Text) v)) where
  schema = coerce $ A.schema @(M.Map T.Text (FieldValue sch v))
instance A.HasAvroSchema (FieldValue sch v)
         => A.HasAvroSchema (FieldValue sch ('TMap ('TPrimitive String) v)) where
  schema = coerce $ A.schema @(M.Map String (FieldValue sch v))

class HasAvroSchemaUnion (f :: k -> *) (xs :: [k]) where
  schemaU :: Proxy f -> Proxy xs -> NonEmpty ASch.Type
instance forall f v.
         A.HasAvroSchema (f v) => HasAvroSchemaUnion f '[v] where
  schemaU _ _ = vSchema :| []
    where vSchema = unTagged (A.schema @(f v))
instance forall f x y zs.
         (A.HasAvroSchema (f x), HasAvroSchemaUnion f (y ': zs))
         => HasAvroSchemaUnion f (x ': y ': zs) where
  schemaU p _ = xSchema :| NonEmptyList.toList yzsSchema
    where xSchema = unTagged (A.schema @(f x))
          yzsSchema = schemaU p (Proxy @(y ': zs))

class HasAvroSchemaFields sch (fs :: [k]) where
  schemaF :: Proxy sch -> Proxy fs -> [ASch.Field]
instance HasAvroSchemaFields sch '[] where
  schemaF _ _ = []
instance forall sch name t fs. 
         (KnownName name, A.HasAvroSchema (FieldValue sch t), HasAvroSchemaFields sch fs)
         => HasAvroSchemaFields sch ('FieldDef name t ': fs) where
  schemaF psch _ = schemaThis : schemaF psch (Proxy @fs)
    where fieldName = nameText (Proxy @name)
          schemaT = unTagged $ A.schema @(FieldValue sch t)
          schemaThis = ASch.Field fieldName [] Nothing Nothing schemaT Nothing

-- FromAvro instances

instance (KnownName name, HasAvroSchemaFields sch args, FromAvroFields sch args)
         => A.FromAvro (Term sch ('DRecord name args)) where
  fromAvro (AVal.Record _ fields) = TRecord <$> fromAvroF fields
  fromAvro v = A.badValue v "record"
instance forall sch name choices.
         (KnownName name, KnownNames choices, GetNthIfPossible choices)
          => A.FromAvro (Term sch ('DEnum name choices)) where
  fromAvro v@(AVal.Enum _ n _) = TEnum <$> getNthIfPossible v n
  fromAvro v = A.badValue v "enum"
instance A.FromAvro (FieldValue sch t)
         => A.FromAvro (Term sch ('DSimple t)) where
  fromAvro v = TSimple <$> A.fromAvro v

instance A.FromAvro (FieldValue sch 'TNull) where
  fromAvro AVal.Null = return FNull
  fromAvro v         = A.badValue v "null"
instance A.FromAvro t => A.FromAvro (FieldValue sch ('TPrimitive t)) where
  fromAvro v = FPrimitive <$> A.fromAvro v
instance (KnownName t, A.FromAvro (Term sch (sch :/: t)))
         => A.FromAvro (FieldValue sch ('TSchematic t)) where
  fromAvro v = FSchematic <$> A.fromAvro v
instance (HasAvroSchemaUnion (FieldValue sch) choices, FromAvroUnion sch choices)
         => A.FromAvro (FieldValue sch ('TUnion choices)) where
  fromAvro (AVal.Union _ branch v) = FUnion <$> fromAvroU branch v
  fromAvro v = A.badValue v "union"
instance A.FromAvro (FieldValue sch t)
         => A.FromAvro (FieldValue sch ('TOption t)) where
  fromAvro v = FOption <$> A.fromAvro v
instance A.FromAvro (FieldValue sch t)
         => A.FromAvro (FieldValue sch ('TList t)) where
  fromAvro v = FList <$> A.fromAvro v
-- These are the only two versions of Map supported by the library
instance A.FromAvro (FieldValue sch v)
         => A.FromAvro (FieldValue sch ('TMap ('TPrimitive T.Text) v)) where
  fromAvro v = FMap . M.mapKeys FPrimitive <$> A.fromAvro v
instance A.FromAvro (FieldValue sch v)
         => A.FromAvro (FieldValue sch ('TMap ('TPrimitive String) v)) where
  fromAvro v = FMap . M.mapKeys (FPrimitive . T.unpack) <$> A.fromAvro v

class GetNthIfPossible (vs :: [k]) where
  getNthIfPossible :: AVal.Value ASch.Type -> Int -> A.Result (NS Proxy vs)
instance GetNthIfPossible '[] where
  getNthIfPossible v _ = A.badValue v "element not found"
instance GetNthIfPossible vs => GetNthIfPossible (v ': vs) where
  getNthIfPossible _ 0 = return (Z Proxy)
  getNthIfPossible v n = S <$> getNthIfPossible v (n-1)

class FromAvroUnion sch choices where
  fromAvroU :: ASch.Type -> AVal.Value ASch.Type -> ASch.Result (NS (FieldValue sch) choices)
instance FromAvroUnion sch '[] where
  fromAvroU _ v = A.badValue v "union choice not found"
instance forall sch u us.
         (A.FromAvro (FieldValue sch u), FromAvroUnion sch us)
         => FromAvroUnion sch (u ': us) where
  fromAvroU branch v
    | ASch.matches branch (unTagged (A.schema @(FieldValue sch u)))
    = Z <$> A.fromAvro v
    | otherwise
    = S <$> fromAvroU branch v

class FromAvroFields sch (fs :: [FieldDef Symbol Symbol]) where
  fromAvroF :: HM.HashMap T.Text (AVal.Value ASch.Type) -> A.Result (NP (Field sch) fs)
instance FromAvroFields sch '[] where
  fromAvroF _ = return Nil
instance forall sch name t fs.
         (KnownName name, A.FromAvro (FieldValue sch t), FromAvroFields sch fs)
         => FromAvroFields sch ('FieldDef name t ': fs) where
  fromAvroF v = case HM.lookup fieldName v of
                  Nothing -> A.badValue v "field not found"
                  Just f  -> (:*) <$> (Field <$> A.fromAvro f) <*> fromAvroF v
    where fieldName = nameText (Proxy @name)

-- ToAvro instances

instance forall sch name args.
         (KnownName name, HasAvroSchemaFields sch args, ToAvroFields sch args)
         => A.ToAvro (Term sch ('DRecord name args)) where
  toAvro (TRecord fields) = AVal.Record wholeSchema (toAvroF fields)
    where wholeSchema = unTagged (A.schema @(Term sch ('DRecord name args)))
instance forall sch name choices.
         (KnownName name, KnownNames choices, ToAvroEnum choices)
          => A.ToAvro (Term sch ('DEnum name choices)) where
  toAvro (TEnum n) = AVal.Enum wholeSchema choice text
    where wholeSchema = unTagged (A.schema @(Term sch ('DEnum name choices)))
          (choice, text) = toAvroE n
instance A.ToAvro (FieldValue sch t)
         => A.ToAvro (Term sch ('DSimple t)) where
  toAvro (TSimple v) = A.toAvro v

instance A.ToAvro (FieldValue sch 'TNull) where
  toAvro FNull = AVal.Null
instance A.ToAvro t => A.ToAvro (FieldValue sch ('TPrimitive t)) where
  toAvro (FPrimitive v) = A.toAvro v
instance (KnownName t, A.ToAvro (Term sch (sch :/: t)))
         => A.ToAvro (FieldValue sch ('TSchematic t)) where
  toAvro (FSchematic v) = A.toAvro v
instance forall sch choices.
         (HasAvroSchemaUnion (FieldValue sch) choices, ToAvroUnion sch choices)
         => A.ToAvro (FieldValue sch ('TUnion choices)) where
  toAvro (FUnion v) = AVal.Union wholeSchema' chosenTy chosenVal
    where wholeSchema = schemaU (Proxy @(FieldValue sch)) (Proxy @choices)
          wholeSchema' = V.fromList (NonEmptyList.toList wholeSchema)
          (chosenTy, chosenVal) = toAvroU v 
instance A.ToAvro (FieldValue sch t)
         => A.ToAvro (FieldValue sch ('TOption t)) where
  toAvro (FOption v) = A.toAvro v
instance A.ToAvro (FieldValue sch t)
         => A.ToAvro (FieldValue sch ('TList t)) where
  toAvro (FList v) = AVal.Array $ V.fromList $ A.toAvro <$> v
-- These are the only two versions of Map supported by the library
instance A.ToAvro (FieldValue sch v)
         => A.ToAvro (FieldValue sch ('TMap ('TPrimitive T.Text) v)) where
  toAvro (FMap v) = A.toAvro $ M.mapKeys (\(FPrimitive k) -> k) v
instance A.ToAvro (FieldValue sch v)
         => A.ToAvro (FieldValue sch ('TMap ('TPrimitive String) v)) where
  toAvro (FMap v) = A.toAvro $ M.mapKeys (\(FPrimitive k) -> k) v

class ToAvroUnion sch choices where
  toAvroU :: NS (FieldValue sch) choices -> (ASch.Type, AVal.Value ASch.Type)
instance ToAvroUnion sch '[] where
  toAvroU _ = error "ToAvro in an empty union"
instance forall sch u us.
         (A.ToAvro (FieldValue sch u), ToAvroUnion sch us)
         => ToAvroUnion sch (u ': us) where
  toAvroU (Z v) = (unTagged (A.schema @(FieldValue sch u)), A.toAvro v)
  toAvroU (S n) = toAvroU n

class KnownNames choices => ToAvroEnum choices where
  toAvroE :: NS Proxy choices -> (Int, T.Text)
instance ToAvroEnum '[] where
  toAvroE = error "ToAvro in an empty enum"
instance forall u us. (KnownName u, ToAvroEnum us)
         => ToAvroEnum (u ': us) where
  toAvroE (Z _) = (0, nameText (Proxy @u))
  toAvroE (S v) = let (n, t) = toAvroE v in (n + 1, t)

class ToAvroFields sch (fs :: [FieldDef Symbol Symbol]) where
  toAvroF :: NP (Field sch) fs -> HM.HashMap T.Text (AVal.Value ASch.Type)
instance ToAvroFields sch '[] where
  toAvroF _ = HM.empty
instance forall sch name t fs.
         (KnownName name, A.ToAvro (FieldValue sch t), ToAvroFields sch fs)
         => ToAvroFields sch ('FieldDef name t ': fs) where
  toAvroF (Field v :* rest) = HM.insert fieldName fieldValue (toAvroF rest)
    where fieldName  = nameText (Proxy @name)
          fieldValue = A.toAvro v

-- Conversion of symbols to other things

class KnownNames (ss :: [k]) where
  symbols :: (forall (s :: k). KnownName s => Proxy s -> a) -> Proxy ss -> [a]
instance KnownNames '[] where
  symbols _ _ = []
instance forall s ss. (KnownName s, KnownNames ss) => KnownNames (s ': ss) where
  symbols f _ = f (Proxy @s) : symbols f (Proxy @ss)

nameText :: KnownName s => proxy s -> T.Text
nameText = T.pack . nameVal
nameTypeName :: KnownName s => proxy s -> ASch.TypeName
nameTypeName = ASch.parseFullname . nameText