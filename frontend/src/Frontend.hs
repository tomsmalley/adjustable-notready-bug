{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import Control.Monad.Fix
import Data.Map (Map)
import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex.Dom.Core
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Common.Api
import Common.Route

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = prerender_ blank $ do
    let innerMap = Map.fromList [(k, ()) | k <- ['A'..'H']]
        outerMap = Map.fromList [(k, innerMap) | k <- [0..30]]
    el "ul" $ listWithKey (pure outerMap) nestedRow
    pure ()
  }

nestedRow :: (MonadWidget t m) => Int -> Dynamic t (Map Char ()) -> m ()
nestedRow i inner = do
  el "li" $ do
    el "span" $ text $ T.pack $ show i
    el "ol" $ listWithKey inner innerRow
  pure ()

innerRow :: (MonadWidget t m) => Char -> Dynamic t () -> m ()
innerRow c str = do
  notReadyUntil =<< delay 0.5 =<< getPostBuild
  el "p" $ text $ T.singleton c
