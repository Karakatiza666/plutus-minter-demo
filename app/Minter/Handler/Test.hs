
module Minter.Handler.Test where

import Minter.Import
import Cardano.Plutus.OnChain.FeeMinter.Test

getTestR :: Text -> Handler Value
getTestR _ = do
    let 
    return $ object [
        ("collectorDatum", String testCollectorDatum),
        ("testOracle", String testOracle),
        ("testEmpty", String testEmpty)
        ]