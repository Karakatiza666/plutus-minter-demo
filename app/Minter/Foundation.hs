{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Minter.Foundation where

import Yesod.Core
import Minter.Import.NoFoundation
import Yesod.Core.Types            (Logger)

data App = App {
    appSettings    :: AppSettings
  , appLogger      :: Logger
}

-- https://github.com/naliwajek/docker-haskell-yesod/blob/master/src/Foundation.hs

-- Absolute path instead of "app/Minter/routes.yesodroutes" because otherwise `cabal install` fails
mkYesodData "App" $(parseRoutesFile "/workspaces/minter-plutus/app/Minter/routes.yesodroutes")

instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"
    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger