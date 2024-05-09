module Htmx.Request where

import Data.Maybe (isJust)
import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ActionT, headers)

findHeader :: Text -> [(Text, Text)] -> Maybe Text
findHeader header hs =
  case filter (\(h, _) -> h == header) hs of
    [] -> Nothing
    ((_, v) : _) -> Just v

isBoosted :: (Monad m) => ActionT m Bool
isBoosted = isJust . findHeader "HX-Boosted" <$> headers

hxCurrentUrl :: (Monad m) => ActionT m (Maybe Text)
hxCurrentUrl = findHeader "HX-Current-Url" <$> headers

isHtmx :: (Monad m) => ActionT m Bool
isHtmx = isJust . findHeader "HX-Request" <$> headers

hxHistoryRestoreRequest :: (Monad m) => ActionT m Bool
hxHistoryRestoreRequest =
  (Just "true" ==) . findHeader "HX-History-Restore-Request" <$> headers

hxPrompt :: (Monad m) => ActionT m (Maybe Text)
hxPrompt = findHeader "HX-Prompt" <$> headers

hxTarget :: (Monad m) => ActionT m (Maybe Text)
hxTarget = findHeader "HX-Target" <$> headers

hxTrigger :: (Monad m) => ActionT m (Maybe Text)
hxTrigger = findHeader "HX-Trigger" <$> headers

hxTriggerName :: (Monad m) => ActionT m (Maybe Text)
hxTriggerName = findHeader "HX-Trigger-Name" <$> headers
