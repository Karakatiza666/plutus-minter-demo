{-# LANGUAGE DeriveGeneric #-}

module Cardano.Plutus.OnChain.FeeMinter.FeeExchangeCollectorDatum where
import GHC.Generics (Generic)
import PlutusTx
import PlutusTx.Prelude
import Plutus.V1.Ledger.Value

data FeeExchangeDatum = NoExchange | ExchangeAda {
    policyId :: CurrencySymbol,
    assetName :: TokenName,
    amount :: Integer
}
    deriving stock Generic

instance Eq FeeExchangeDatum where
    {-# INLINABLE (==) #-}
    NoExchange == NoExchange = True
    (ExchangeAda a b c) == (ExchangeAda a' b' c') = a == a' && b == b' && c == c'
    _ == _ = False

makeIsDataIndexed ''FeeExchangeDatum [('NoExchange, 0), ('ExchangeAda, 1)]
makeLift ''FeeExchangeDatum