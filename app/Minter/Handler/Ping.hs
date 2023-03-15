{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Minter.Handler.Ping where

import Minter.Import

getPingR :: Handler Value
getPingR = return $ object [("status", String $ "Works")]
