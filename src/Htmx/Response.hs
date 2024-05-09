module Htmx.Response where

import Control.Monad.IO.Class (MonadIO)
import Data.Text.Lazy (Text)
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (setHeader)

hxLocation :: (MonadIO m) => Text -> ActionT m ()
hxLocation = setHeader "HX-Location"

hxPushUrl :: (MonadIO m) => Text -> ActionT m ()
hxPushUrl = setHeader "HX-Push-Url"

hxRedirect :: (MonadIO m) => Text -> ActionT m ()
hxRedirect = setHeader "HX-Redirect"

hxRefresh :: (MonadIO m) => Text -> ActionT m ()
hxRefresh = setHeader "HX-Refresh"

hxReplaceUrl :: (MonadIO m) => Text -> ActionT m ()
hxReplaceUrl = setHeader "HX-Replace-Url"

hxReswap :: (MonadIO m) => Text -> ActionT m ()
hxReswap = setHeader "HX-Reswap"

hxRetarget :: (MonadIO m) => Text -> ActionT m ()
hxRetarget = setHeader "HX-Retarget"

hxReselect :: (MonadIO m) => Text -> ActionT m ()
hxReselect = setHeader "HX-Reselect"

hxTrigger :: (MonadIO m) => Text -> ActionT m ()
hxTrigger = setHeader "HX-Trigger"

hxTriggerAfterSettle :: (MonadIO m) => Text -> ActionT m ()
hxTriggerAfterSettle = setHeader "HX-Trigger-After-Settle"

hxTriggerAfterSwap :: (MonadIO m) => Text -> ActionT m ()
hxTriggerAfterSwap = setHeader "HX-Trigger-After-Swap"
