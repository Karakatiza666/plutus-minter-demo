-- Authoritatively provide the price of one token relative to the other
-- Oracle can be updated by holder of its UniqueNFT - the owner

module Cardano.Plutus.OnChain.AuthorOracle where

import Cardano.Plutus.OnChain.FeeMinter.UniqueNFT
import Cardano.Plutus.Common

newtype AuthorOracleDatum = AuthorOracleDatum {
    aPolicy :: CurrencySymbol
    aToken :: TokenName
    bPolicy :: CurrencySymbol
    bToken :: TokenName
    numerator :: Integer
    -- denominator is 10^8
}
    deriving stock Generic
    deriving anyclass Eq
PlutusTx.unstableMakeIsData ''AuthorOracleDatum

data AuthorOracleRedeemer = AuthorOracleRedeemer {
    newRatio :: Integer
} deriving stock Generic
PlutusTx.unstableMakeIsData ''NonceRedeemer
PlutusTx.makeLift ''NonceRedeemer

data AuthorOracleValidator
instance Scripts.ValidatorTypes AuthorOracleValidator where
   type instance DatumType AuthorOracleValidator = AuthorOracleDatum
   type instance RedeemerType AuthorOracleValidator = Maybe (Either NonceTransferRedeemer NonceRedeemer)


{-# INLINABLE mkAuthorOracleValidator #-}
mkAuthorOracleValidator :: CurrencySymbol -- ^ Witness of owner's UniqueNFT policyId
                        -> AuthorOracleDatum
                        -> AuthorOracleRedeemer
                        -> ScriptContext -> Bool
mkAuthorOracleValidator witness datum@AuthorOracleDatum{..} redeemer ctx@ScriptContext{scriptContextTxInfo=txIn} =
    if selfWitnessed then outOraclesWitnessed else False
    where
        selfWitnessed = or (hasCurrency witness <$> findOwnInput ctx)
        outOraclesWitnessed = and $ hasCurrency witness . txOutValue <$> getContinuingOutputs

authorOracleScript :: CurrencySymbol -- ^ Owner's UniqueNFT policyId
                   -> Scripts.TypedValidator AuthorOracleValidator
authorOracleScript owner = Scripts.mkTypedValidator @AuthorOracleValidator
    ($$(PlutusTx.compile [|| mkAuthorOracleValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode owner)
    $$(PlutusTx.compile [|| Scripts.mkUntypedValidator ||])