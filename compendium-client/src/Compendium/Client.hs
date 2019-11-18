{-# language DataKinds, TypeOperators,
             DeriveGeneric, DeriveAnyClass,
             ViewPatterns, TypeApplications #-}
module Compendium.Client where

import Data.Aeson
import Data.Char
import Data.Proxy
import Data.Text
import Language.ProtocolBuffers.Types
import Language.ProtocolBuffers.Parser
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client
import Text.Megaparsec

import GHC.Generics

newtype Protocol
  = Protocol { raw :: Text }
  deriving (Eq, Show, Generic, FromJSON)
  
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

transformation :: Manager -> BaseUrl
               -> Text -> IdlName -> IO (Either ClientError Protocol)
transformation m url ident idl
  = runClientM (transformation' ident idl) (mkClientEnv m url)

transformation' :: Text -> IdlName -> ClientM Protocol
transformation'
  = client (Proxy @TransformationAPI)

data ObtainProtoBufError
  = OPEClient ClientError
  | OPEParse  (ParseErrorBundle Text Char)
  deriving (Show)

obtainProtoBuf :: Manager -> BaseUrl
               -> Text -> IO (Either ObtainProtoBufError ProtoBuf)
obtainProtoBuf m url ident
  = do r <- transformation m url ident Protobuf
       case r of
         Left e
           -> return $ Left (OPEClient e)
         Right (Protocol p)
           -> case parseProtoBuf p of
                Left e   -> return $ Left (OPEParse e)
                Right pb -> return $ Right pb