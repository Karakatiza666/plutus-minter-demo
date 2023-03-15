{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Plutus.Validator where

import Cardano.Api.Shelley  as Shelley (PlutusScript (..), PlutusScriptV1, PlutusScriptV2, textEnvelopeRawCBOR, serialiseToTextEnvelope)

import Data.ByteString.Short qualified as Short
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString qualified as Strict
import Data.ByteString.Base16.Lazy as Lazy16
import Data.ByteString.Base16 as Strict16

import Codec.Serialise
import PlutusTx
import PlutusTx.Prelude
import Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Value

import Plutus.Script.Utils.V2.Scripts as ScriptsV2
import Plutus.Script.Utils.V2.Typed.Scripts as ScriptsV2 hiding (validatorHash)

mintingPolicyScript :: MintingPolicy -> Validator
mintingPolicyScript = Validator . unMintingPolicyScript

unValidatorHash (ValidatorHash hash) = hash

typedValidatorHash :: TypedValidator a -> ValidatorHash
typedValidatorHash = validatorHash . validatorScript

class PlutusScriptLike a where
    toPlutusScript :: Validator -> PlutusScript a
    plutusScriptToCBOR :: PlutusScript a -> Strict.ByteString
    validatorToCBOR :: Validator -> Strict.ByteString
    validatorToCBOR = plutusScriptToCBOR @a . toPlutusScript
    policyHashAndBytes :: MintingPolicy -> Strict.ByteString
    policyHashAndBytes policy =
        let
            policyHash = fromBuiltin . unCurrencySymbol . scriptCurrencySymbol $ policy
            policyBytes = validatorToCBOR @a . mintingPolicyScript $ policy
        in Strict16.encode $ policyHash `Strict.append` policyBytes
    scriptHashAndBytes :: TypedValidator b -> Strict.ByteString
    scriptHashAndBytes script =
        let
            scriptHash = fromBuiltin . unValidatorHash . typedValidatorHash $ script
            scriptBytes = validatorToCBOR @a . validatorScript $ script
        in Strict16.encode $ scriptHash `Strict.append` scriptBytes

instance PlutusScriptLike PlutusScriptV1 where
    toPlutusScript = Shelley.PlutusScriptSerialised . Short.toShort . Lazy.toStrict . serialise
    plutusScriptToCBOR = Shelley.textEnvelopeRawCBOR . Shelley.serialiseToTextEnvelope Nothing

instance PlutusScriptLike PlutusScriptV2 where
    toPlutusScript = Shelley.PlutusScriptSerialised . Short.toShort . Lazy.toStrict . serialise
    plutusScriptToCBOR = Shelley.textEnvelopeRawCBOR . Shelley.serialiseToTextEnvelope Nothing


{- Using Proxy 
class PlutusScriptLike a where
    toPlutusScript :: Validator -> PlutusScript a
    plutusScriptToCBOR :: PlutusScript a -> Strict.ByteString
    validatorToCBOR :: Proxy a -> Strict.ByteString
    validatorToCBOR _ = plutusScriptToCBOR . toPlutusScript

instance PlutusScriptLike PlutusScriptV1 where
    toPlutusScript = Shelley.PlutusScriptSerialised . Short.toShort . Lazy.toStrict . serialise
    plutusScriptToCBOR = Shelley.textEnvelopeRawCBOR . Shelley.serialiseToTextEnvelope Nothing

mintingPolicyScript :: MintingPolicy -> Validator
mintingPolicyScript = Validator . unMintingPolicyScript

policyHashAndBytes :: MintingPolicy -> Strict.ByteString
policyHashAndBytes policy =
    let
        policyHash = fromBuiltin . unCurrencySymbol . scriptCurrencySymbol $ policy
        policyBytes = validatorToCBOR . mintingPolicyScript $ policy
    in Strict16.encode $ policyHash <> policyBytes
-}

{-
mintingPolicyScript :: MintingPolicy -> Validator
mintingPolicyScript = Validator . unMintingPolicyScript

policyHashAndBytes :: MintingPolicy -> Strict.ByteString
policyHashAndBytes policy =
    let
        policyHash = fromBuiltin . unCurrencySymbol . scriptCurrencySymbol $ policy
        policyBytes = validatorToCBOR . mintingPolicyScript $ policy
    in Strict16.encode $ policyHash <> policyBytes

unValidatorHash (ValidatorHash hash) = hash
-- unValidatorHash = . scriptHashAddress


toPlutusScript :: Validator -> PlutusScript PlutusScriptV1
toPlutusScript = PlutusScriptSerialised . Short.toShort . Lazy.toStrict . serialise

plutusScriptToCBOR :: PlutusScript PlutusScriptV1 -> Strict.ByteString
plutusScriptToCBOR = textEnvelopeRawCBOR . serialiseToTextEnvelope Nothing

validatorToCBOR :: Validator -> Strict.ByteString
validatorToCBOR = plutusScriptToCBOR . toPlutusScript

typedValidatorHash :: TypedValidator a -> ValidatorHash
typedValidatorHash = validatorHash . validatorScript

scriptHashAndBytes :: TypedValidator a -> Strict.ByteString
scriptHashAndBytes script =
    let
        scriptHash = fromBuiltin . unValidatorHash . typedValidatorHash $ script
        scriptBytes = validatorToCBOR . validatorScript $ script
    in Strict16.encode $ scriptHash <> scriptBytes
-}