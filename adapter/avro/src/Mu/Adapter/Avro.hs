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
{-# language TypeFamilies          #-}
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
import qualified Data.Avro.Encoding.FromAvro         as AVal
import qualified Data.Avro.Encoding.ToAvro           as A
import qualified Data.Avro.Schema.ReadSchema         as RSch
import qualified Data.Avro.Schema.Schema             as ASch
-- 'Tagged . unTagged' can be replaced by 'coerce'
-- eliminating some run-time overhead
import           Control.Monad.Trans.State
import           Data.Avro.EitherN                   (putIndexedValue)
import           Data.ByteString.Builder             (Builder, word8)
import           Data.Coerce                         (coerce)
import qualified Data.HashMap.Strict                 as HM
import           Data.List.NonEmpty                  (NonEmpty (..))
import qualified Data.List.NonEmpty                  as NonEmptyList
import qualified Data.Map                            as M
import           Data.Maybe                          (fromJust)
import           Data.Tagged
import qualified Data.Text                           as T
import qualified Data.Vector                         as V
import           GHC.TypeLits

import           Mu.Schema
import qualified Mu.Schema.Interpretation.Schemaless as SLess

instance SLess.ToSchemalessTerm AVal.Value where
  toSchemalessTerm (AVal.Record s r)
    = case s of
        RSch.Record { RSch.fields = fs }
          -> SLess.TRecord $
               zipWith (\k v -> SLess.Field k (SLess.toSchemalessValue v))
                       (map RSch.fldName fs) (V.toList r)
        _ -> error ("this should never happen:\n" ++ show s)
  toSchemalessTerm (AVal.Enum _ i _)
    = SLess.TEnum i
  toSchemalessTerm (AVal.Union _ _ v)
    = SLess.toSchemalessTerm v
  toSchemalessTerm v = SLess.TSimple (SLess.toSchemalessValue v)

instance SLess.ToSchemalessValue AVal.Value where
  toSchemalessValue AVal.Null         = SLess.FNull
  toSchemalessValue (AVal.Boolean  b) = SLess.FPrimitive b
  toSchemalessValue (AVal.Int    _ b) = SLess.FPrimitive b
  toSchemalessValue (AVal.Long   _ b) = SLess.FPrimitive b
  toSchemalessValue (AVal.Float  _ b) = SLess.FPrimitive b
  toSchemalessValue (AVal.Double _ b) = SLess.FPrimitive b
  toSchemalessValue (AVal.String _ b) = SLess.FPrimitive b
  toSchemalessValue (AVal.Fixed  _ b) = SLess.FPrimitive b
  toSchemalessValue (AVal.Bytes  _ b) = SLess.FPrimitive b
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

instance (HasAvroSchema' (Term sch (sch :/: sty)))
         => A.HasAvroSchema (WithSchema sch sty t) where
  schema = coerce $ evalState (schema' @(Term sch (sch :/: sty))) []
instance ( FromSchema sch sty t
         , A.FromAvro (Term sch (sch :/: sty)) )
         => A.FromAvro (WithSchema sch sty t) where
  fromAvro entire
    = WithSchema . fromSchema' @_ @_ @sch <$> AVal.fromAvro entire
instance ( ToSchema sch sty t
         , A.ToAvro (Term sch (sch :/: sty)) )
         => A.ToAvro (WithSchema sch sty t) where
  toAvro sch (WithSchema v)
    = A.toAvro sch (toSchema' @_ @_ @sch v)

-- HasAvroSchema instances

class HasAvroSchema' x where
  schema' :: State [ASch.TypeName] (Tagged x ASch.Schema)

instance TypeError ('Text "you should never use HasAvroSchema directly on Term, use WithSchema")
         => A.HasAvroSchema (Term sch t) where
  schema = error "this should never happen"
instance HasAvroSchema' (FieldValue sch t)
         => A.HasAvroSchema (FieldValue sch t) where
  schema = evalState schema' []

instance (KnownName name, HasAvroSchemaFields sch args)
         => HasAvroSchema' (Term sch ('DRecord name args)) where
  schema'
    = do let recordName = nameTypeName (Proxy @name)
         visited <- gets (recordName `elem`)
         if visited
            then pure $ Tagged $ ASch.NamedType recordName
            else do modify (recordName :)
                    fields <- schemaF (Proxy @sch) (Proxy @args)
                    pure $ Tagged $ ASch.Record recordName [] Nothing fields
instance (KnownName name, HasAvroSchemaEnum choices)
          => HasAvroSchema' (Term sch ('DEnum name choices)) where
  schema'
    = do let enumName = nameTypeName (Proxy @name)
             choicesNames = schemaE (Proxy @choices)
         visited <- gets (enumName `elem`)
         if visited
            then pure $ Tagged $ ASch.NamedType enumName
            else do modify (enumName :)
                    pure $ Tagged $ ASch.mkEnum enumName [] Nothing choicesNames

instance HasAvroSchema' (FieldValue sch t)
         => HasAvroSchema' (Term sch ('DSimple t)) where
  schema' = coerce <$> schema' @(FieldValue sch t)

instance HasAvroSchema' (FieldValue sch 'TNull) where
  schema' = pure $ Tagged ASch.Null
instance A.HasAvroSchema t
         => HasAvroSchema' (FieldValue sch ('TPrimitive t)) where
  schema' = pure $ coerce $ A.schema @t
instance (HasAvroSchema' (Term sch (sch :/: t)))
         => HasAvroSchema' (FieldValue sch ('TSchematic t)) where
  schema' = coerce <$> schema' @(Term sch (sch :/: t))
instance forall sch choices.
         HasAvroSchemaUnion (FieldValue sch) choices
         => HasAvroSchema' (FieldValue sch ('TUnion choices)) where
  schema' = do
    schs <- schemaU (Proxy @(FieldValue sch)) (Proxy @choices)
    pure $ Tagged $ ASch.mkUnion schs
instance HasAvroSchema' (FieldValue sch t)
         => HasAvroSchema' (FieldValue sch ('TOption t)) where
  schema' = do
    iSchema <- unTagged <$> schema' @(FieldValue sch t)
    pure $ Tagged $ ASch.mkUnion $ ASch.Null :| [iSchema]
instance HasAvroSchema' (FieldValue sch t)
         => HasAvroSchema' (FieldValue sch ('TList t)) where
  schema' = do
    iSchema <- unTagged <$> schema' @(FieldValue sch t)
    pure $ Tagged $ ASch.Array iSchema
-- These are the only two versions of Map supported by the library
instance HasAvroSchema' (FieldValue sch v)
         => HasAvroSchema' (FieldValue sch ('TMap ('TPrimitive T.Text) v)) where
  schema' = do
    iSchema <- unTagged <$> schema' @(FieldValue sch v)
    pure $ Tagged $ ASch.Map iSchema
instance HasAvroSchema' (FieldValue sch v)
         => HasAvroSchema' (FieldValue sch ('TMap ('TPrimitive String) v)) where
  schema' = do
    iSchema <- unTagged <$> schema' @(FieldValue sch v)
    pure $ Tagged $ ASch.Map iSchema

class HasAvroSchemaUnion (f :: k -> *) (xs :: [k]) where
  schemaU :: Proxy f -> Proxy xs -> State [ASch.TypeName] (NonEmpty ASch.Schema)
instance HasAvroSchema' (f v) => HasAvroSchemaUnion f '[v] where
  schemaU _ _ = do
    vSchema <- unTagged <$> schema' @(f v)
    pure $ vSchema :| []
instance (HasAvroSchema' (f x), HasAvroSchemaUnion f (y ': zs))
         => HasAvroSchemaUnion f (x ': y ': zs) where
  schemaU p _ = do
    xSchema   <- unTagged <$> schema' @(f x)
    yzsSchema <- schemaU p (Proxy @(y ': zs))
    pure $ xSchema :| NonEmptyList.toList yzsSchema

class HasAvroSchemaFields sch (fs :: [FieldDef tn fn]) where
  schemaF :: Proxy sch -> Proxy fs -> State [ASch.TypeName] [ASch.Field]
instance HasAvroSchemaFields sch '[] where
  schemaF _ _ = pure []
instance (KnownName name, HasAvroSchema' (FieldValue sch t), HasAvroSchemaFields sch fs)
         => HasAvroSchemaFields sch ('FieldDef name t ': fs) where
  schemaF psch _ = do
    let fieldName = nameText (Proxy @name)
    schemaT <- unTagged <$> schema' @(FieldValue sch t)
    let schemaThis = ASch.Field fieldName [] Nothing Nothing schemaT Nothing
    rest <- schemaF psch (Proxy @fs)
    pure $ schemaThis : rest

class HasAvroSchemaEnum (fs :: [ChoiceDef fn]) where
  schemaE :: Proxy fs -> [T.Text]
instance HasAvroSchemaEnum '[] where
  schemaE _ = []
instance (KnownName name, HasAvroSchemaEnum fs)
         => HasAvroSchemaEnum ('ChoiceDef name ': fs) where
  schemaE _ = nameText (Proxy @name) : schemaE (Proxy @fs)

-- FromAvro instances

instance (KnownName name, FromAvroFields sch args)
         => A.FromAvro (Term sch ('DRecord name args)) where
  fromAvro (AVal.Record RSch.Record { RSch.fields = fs } fields)
    = TRecord <$> fromAvroF r
    where
      r = HM.fromList $ zip (map RSch.fldName fs) (V.toList fields)
  fromAvro _ = Left "expecting record"
instance (KnownName name, FromAvroEnum choices)
          => A.FromAvro (Term sch ('DEnum name choices)) where
  fromAvro (AVal.Enum _ _ v) = TEnum <$> fromAvroEnum v
  fromAvro _                 = Left "expecting enum"
instance (A.FromAvro (FieldValue sch t))
         => A.FromAvro (Term sch ('DSimple t)) where
  fromAvro v = TSimple <$> AVal.fromAvro v

instance A.FromAvro (FieldValue sch 'TNull) where
  fromAvro AVal.Null = pure FNull
  fromAvro _         = Left "expecting null"
instance A.FromAvro t => A.FromAvro (FieldValue sch ('TPrimitive t)) where
  fromAvro v = FPrimitive <$> AVal.fromAvro v
instance ( KnownName t, A.FromAvro (Term sch (sch :/: t)) )
         => A.FromAvro (FieldValue sch ('TSchematic t)) where
  fromAvro v = FSchematic <$> AVal.fromAvro v
instance (FromAvroUnion sch choices)
         => A.FromAvro (FieldValue sch ('TUnion choices)) where
  fromAvro (AVal.Union _ i v) = FUnion <$> fromAvroU i v
  fromAvro _                  = Left "expecting union"
instance (A.FromAvro (FieldValue sch t))
         => A.FromAvro (FieldValue sch ('TOption t)) where
  fromAvro v = FOption <$> AVal.fromAvro v
instance (A.FromAvro (FieldValue sch t))
         => A.FromAvro (FieldValue sch ('TList t)) where
  fromAvro v = FList <$> AVal.fromAvro v
-- These are the only two versions of Map supported by the library
instance (A.FromAvro (FieldValue sch v))
         => A.FromAvro (FieldValue sch ('TMap ('TPrimitive T.Text) v)) where
  fromAvro v = FMap . M.mapKeys FPrimitive <$> AVal.fromAvro v
instance (A.FromAvro (FieldValue sch v))
         => A.FromAvro (FieldValue sch ('TMap ('TPrimitive String) v)) where
  fromAvro v = FMap . M.mapKeys (FPrimitive . T.unpack) <$> AVal.fromAvro v

class FromAvroEnum (vs :: [ChoiceDef fn]) where
  fromAvroEnum :: T.Text -> Either String (NS Proxy vs)
instance FromAvroEnum '[] where
  fromAvroEnum _ = Left "enum choice not found"
instance (KnownName name, FromAvroEnum vs)
         => FromAvroEnum ('ChoiceDef name ': vs) where
  fromAvroEnum s
    | s == fieldName = pure $ Z Proxy
    | otherwise      = S <$> fromAvroEnum s
    where fieldName = nameText (Proxy @name)

class FromAvroUnion sch choices where
  fromAvroU :: Int -> AVal.Value -> Either String (NS (FieldValue sch) choices)
instance FromAvroUnion sch '[] where
  fromAvroU _ _ = Left "union choice not found"
instance (A.FromAvro (FieldValue sch u), FromAvroUnion sch us)
         => FromAvroUnion sch (u ': us) where
  fromAvroU 0 v = Z <$> AVal.fromAvro v
  fromAvroU n v = S <$> fromAvroU (n-1) v

class FromAvroFields sch (fs :: [FieldDef Symbol Symbol]) where
  fromAvroF :: HM.HashMap T.Text AVal.Value
            -> Either String (NP (Field sch) fs)
instance FromAvroFields sch '[] where
  fromAvroF _ = pure Nil
instance (KnownName name, A.FromAvro (FieldValue sch t), FromAvroFields sch fs)
         => FromAvroFields sch ('FieldDef name t ': fs) where
  fromAvroF v = case HM.lookup fieldName v of
                  Nothing -> Left "field not found"
                  Just f  -> (:*) <$> (Field <$> AVal.fromAvro f) <*> fromAvroF v
    where fieldName = nameText (Proxy @name)

-- ToAvro instances

instance (KnownName name, ToAvroFields sch args, HasAvroSchemaFields sch args)
         => A.ToAvro (Term sch ('DRecord name args)) where
  toAvro s@ASch.Record {} (TRecord fields)
    = A.record s $ toAvroF fields
  -- if we don't have a record, fall back to the one from schema
  toAvro _ (TRecord fields)
    = A.record sch (toAvroF fields)
    where sch = unTagged $ evalState (schema' @(Term sch ('DRecord name args))) []
instance (KnownName name, ToAvroEnum choices, HasAvroSchemaEnum choices)
          => A.ToAvro (Term sch ('DEnum name choices)) where
  toAvro ASch.Enum { ASch.symbols = ss } (TEnum n)
    = word8 $ fromIntegral $ toAvroE ss n
  -- otherwise fall back to the one from schema
  toAvro _ (TEnum n)
    = word8 $ fromIntegral $ toAvroE (V.fromList $ schemaE (Proxy @choices)) n
instance (A.ToAvro (FieldValue sch t))
         => A.ToAvro (Term sch ('DSimple t)) where
  toAvro s (TSimple v) = A.toAvro s v

instance A.ToAvro (FieldValue sch 'TNull) where
  toAvro _ FNull = mempty
instance A.ToAvro t => A.ToAvro (FieldValue sch ('TPrimitive t)) where
  toAvro s (FPrimitive v) = A.toAvro s v
instance ( KnownName t, A.ToAvro (Term sch (sch :/: t)) )
         => A.ToAvro (FieldValue sch ('TSchematic t)) where
  toAvro s (FSchematic v) = A.toAvro s v
instance (ToAvroUnion sch choices)
         => A.ToAvro (FieldValue sch ('TUnion choices)) where
  toAvro (ASch.Union vs) (FUnion v) = toAvroU vs 0 v
  toAvro s _                        = error ("this should never happen:\n" ++ show s)
instance (A.ToAvro (FieldValue sch t))
         => A.ToAvro (FieldValue sch ('TOption t)) where
  toAvro s (FOption v) = A.toAvro s v
instance (A.ToAvro (FieldValue sch t))
         => A.ToAvro (FieldValue sch ('TList t)) where
  toAvro s (FList v) = A.toAvro s v
-- These are the only two versions of Map supported by the library
instance (A.ToAvro (FieldValue sch v))
         => A.ToAvro (FieldValue sch ('TMap ('TPrimitive T.Text) v)) where
  toAvro s (FMap v) = A.toAvro s $ M.mapKeys (\(FPrimitive k) -> k) v
instance (A.ToAvro (FieldValue sch v))
         => A.ToAvro (FieldValue sch ('TMap ('TPrimitive String) v)) where
  toAvro s (FMap v) = A.toAvro s $ M.mapKeys (\(FPrimitive k) -> T.pack k) v

class ToAvroEnum choices where
  toAvroE :: V.Vector T.Text -> NS Proxy choices -> Int
instance ToAvroEnum '[] where
  toAvroE = error "ToAvro in an empty enum"
instance (KnownName u, ToAvroEnum us)
         => ToAvroEnum ('ChoiceDef u ': us) where
  toAvroE s (Z _) = fromJust $ nameText (Proxy @u) `V.elemIndex` s
  toAvroE s (S v) = toAvroE s v

class ToAvroUnion sch choices where
  toAvroU :: V.Vector ASch.Schema
          -> Int -> NS (FieldValue sch) choices -> Builder
instance ToAvroUnion sch '[] where
  toAvroU = error "this should never happen"
instance (A.ToAvro (FieldValue sch u), ToAvroUnion sch us)
         => ToAvroUnion sch (u ': us) where
  toAvroU allSch n (Z v)
    = putIndexedValue n allSch v
  toAvroU allSch n (S v)
    = toAvroU allSch (n+1) v

class ToAvroFields sch (fs :: [FieldDef Symbol Symbol]) where
  toAvroF :: NP (Field sch) fs -> [(T.Text, A.Encoder)]
instance ToAvroFields sch '[] where
  toAvroF _ = []
instance (KnownName name, A.ToAvro (FieldValue sch t), ToAvroFields sch fs)
         => ToAvroFields sch ('FieldDef name t ': fs) where
  toAvroF (Field v :* rest) = (fieldName A..= v) : toAvroF rest
    where fieldName  = nameText (Proxy @name)

-- Conversion of symbols to other things
nameText :: KnownName s => proxy s -> T.Text
nameText = T.pack . nameVal
nameTypeName :: KnownName s => proxy s -> ASch.TypeName
nameTypeName = ASch.parseFullname . nameText
