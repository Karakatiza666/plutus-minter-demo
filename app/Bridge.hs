module Bridge where

import Prelude
import Cardano.Plutus.OnChain.Bridge.Fungible
import Cardano.Plutus.OnChain.Bridge.Hold
import Cardano.Plutus.OnChain.Bridge.NFT
import Cardano.Plutus.OnChain.Bridge.NonceInteger
import Cardano.Plutus.OnChain.Bridge.UniqueSeed
import Cardano.Plutus.OnChain.Bridge.Withdraw
import Cardano.Plutus.OnChain.Bridge.WithdrawFactory
import Data.ByteString.Lazy.Char8 qualified as Lazy8
import Data.ByteString.Char8 qualified as Strict8
import System.Environment                  (getArgs)
import System.Exit

app :: IO ()
app = do
    args <- map (fromStrict . Strict8.pack) <$> getArgs
    exitWith ExitSuccess

dispatch ["-fungible-policy", nftName] = _fungiblePolicy (extractTokenName nftName)
dispatch ["-hold-script", owner] = _holdScript (extractPubKeyHash owner)
dispatch ["-nft-policy"] = _nftPolicy
dispatch ["-nonce-script", owner] = _nonceScript (extractPubKeyHash owner)
dispatch ["-unique-seed-script"] = _uniqueSeedScript
dispatch ["-unique-seed-policy"] = _uniqueSeedPolicy
dispatch ["-withdraw-script", owner, client] = _withdrawScript (extractPubKeyHash owner) (extractPubKeyHash client)
dispatch ["-factory-script", owner] = _factoryScript (extractPubKeyHash owner)
dispatch ["-nonce-constr"] = testNonceConstr
dispatch _ = error "Args do not match any script"