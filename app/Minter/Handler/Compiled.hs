{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Minter.Handler.Compiled where

import Minter.Import

import Cardano.Plutus.OnChain.FeeMinter.Fungible
import Cardano.Plutus.OnChain.FeeMinter.NFT
import Cardano.Plutus.OnChain.FeeMinter.NFTCollection
import Cardano.Plutus.OnChain.FeeMinter.FeeCollector
import Cardano.Plutus.OnChain.FeeMinter.FeeExchangeCollector
import Cardano.Plutus.OnChain.FeeMinter.ScriptHolder
import Cardano.Plutus.OnChain.FeeMinter.TokenOracle
import Cardano.Plutus.OnChain.FeeMinter.UniqueNFT

import Data.Aeson
import Data.Aeson.Key

import Data.ByteString.Char8 qualified as Strict8
import Data.ByteString qualified as Strict
import Data.Maybe

import Text.Hex (encodeHex, decodeHex)

fromHex :: Text -> Strict.ByteString
fromHex = fromJust . decodeHex
toHex :: Strict.ByteString -> Text
toHex = encodeHex

getCompiledR :: [Text] -> Handler Value
getCompiledR (script : args) = do
    let refs = map fromHex args
    let scriptBytes = dispatch $ encodeUtf8 script : map fromHex args
    return $ object [(fromText script, String $ decodeUtf8 scriptBytes)]

dispatch ["fungible", adminNft, seedUTxO]       = compilePlutusFungible adminNft seedUTxO
dispatch ["nft", adminNft, seedUTxO]            = compilePlutusNFT adminNft seedUTxO
dispatch ["nft_collection", adminNft, seedUTxO] = compilePlutusNFTCollection adminNft seedUTxO
dispatch ["nft_factory", adminNft, seedUTxO]    = compilePlutusNFTFactory adminNft seedUTxO
dispatch ["fee_collector", adminNft]                  = compilePlutusFeeCollector adminNft
dispatch ["fee_exchange_collector", adminNft]         = compilePlutusFeeExchangeCollector adminNft
dispatch ["script_holder", adminNft, scriptHash]      = compilePlutusScriptHolder adminNft scriptHash
dispatch ["token_oracle", adminNft]                   = compilePlutusTokenOracle adminNft
dispatch ["unique_nft", seedUTxO]                     = compilePlutusUniqueNFT seedUTxO
dispatch ["unique_nft_witness", seedUTxO]             = compilePlutusUniqueWitness seedUTxO
dispatch ["witness", currency]                        = compilePlutusWitness currency