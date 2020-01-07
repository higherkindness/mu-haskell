{-# language DataKinds        #-}
{-# language DeriveAnyClass   #-}
{-# language DeriveGeneric    #-}
{-# language TypeApplications #-}
{-# language TypeOperators    #-}
{-# language ViewPatterns     #-}
{-|
Description : Client for the Compendium schema registry

Client for the Compendium schema registry
-}
module Compendium.Client (
-- * Generic query of schemas
  IdlName
, transformation
-- * Query Protocol Buffer schemas
, obtainProtoBuf
, ObtainProtoBufError(..)
) where

import           Data.Aeson
import           Data.Char
import           Data.Proxy
import           Data.Text
import           Language.ProtocolBuffers.Parser
import           Language.ProtocolBuffers.Types
import           Network.HTTP.Client             (Manager)
import           Servant.API
import           Servant.Client
import           Text.Megaparsec

import           GHC.Generics

newtype Protocol
  = Protocol { raw :: Text }
  deriving (Eq, Show, Generic, FromJSON)

-- | Interface Description Languages supported by Compendium.
data IdlName
  = Avro | Protobuf | Mu | OpenApi | Scala
  deriving (Eq, Show, Generic)
instance ToHttpApiData IdlName where
  toQueryParam (show -> x:xs)
    = pack $ Data.Char.toLower x : xs
  toQueryParam _ = error "this should never happen"

type TransformationAPI
  = "protocol" :> Capture "id" Text
               :> "transformation"
               :> QueryParam' '[ Required ] "target" IdlName
               :> Get '[JSON] Protocol

-- | Obtain a schema from the registry.
transformation :: Manager  -- ^ Connection details (from 'http-client').
               -> BaseUrl  -- ^ URL in which Compendium is running.
               -> Text     -- ^ Name that identifies the schema.
               -> IdlName  -- ^ Format of the returned schema.
               -> IO (Either ClientError Text)
transformation m url ident idl
  = runClientM (transformation' ident idl) (mkClientEnv m url)

transformation' :: Text
                -> IdlName
                -> ClientM Text
transformation' ident idl
  = raw <$> client (Proxy @TransformationAPI) ident idl

-- | Errors which may arise during 'obtainProtoBuf'.
data ObtainProtoBufError
  = OPEClient ClientError  -- ^ Error obtaining schema from Compendium
  | OPEParse  (ParseErrorBundle Text Char)  -- ^ Obtaining the schema was OK, error parsing it
  deriving (Show)

-- | Obtain a schema from the registry,
--   and parse it as Protocol Buffers.
obtainProtoBuf :: Manager -> BaseUrl
               -> Text -> IO (Either ObtainProtoBufError ProtoBuf)
obtainProtoBuf m url ident = do
  r <- transformation m url ident Protobuf
  case r of
    Left e
      -> return $ Left (OPEClient e)
    Right p
      -> case parseProtoBuf p of
          Left e   -> return $ Left (OPEParse e)
          Right pb -> return $ Right pb
