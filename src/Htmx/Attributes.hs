module Htmx.Attributes where

import Data.Text (Text)
import Lucid (Attribute)
import Lucid.Base (makeAttribute)

hxGet :: Text -> Attribute
hxGet = makeAttribute "hx-get"

hxPost :: Text -> Attribute
hxPost = makeAttribute "hx-post"

hxPut :: Text -> Attribute
hxPut = makeAttribute "hx-put"

hxDelete :: Text -> Attribute
hxDelete = makeAttribute "hx-delete"

hxPatch :: Text -> Attribute
hxPatch = makeAttribute "hx-patch"

hxTarget :: Text -> Attribute
hxTarget = makeAttribute "hx-target"

hxSwap :: Text -> Attribute
hxSwap = makeAttribute "hx-swap"

hxOn :: Text -> Text -> Attribute
hxOn evt = makeAttribute ("hx-on:" <> evt)

hxPushUrl :: Text -> Attribute
hxPushUrl = makeAttribute "hx-push-url"

hxSelect :: Text -> Attribute
hxSelect = makeAttribute "hx-select"

hxSelectOob :: Text -> Attribute
hxSelectOob = makeAttribute "hx-select-oob"

hxSwapOob :: Text -> Attribute
hxSwapOob = makeAttribute "hx-swap-oob"

hxTrigger :: Text -> Attribute
hxTrigger = makeAttribute "hx-trigger"

hxVals :: Text -> Attribute
hxVals = makeAttribute "hx-vals"

hxBoost :: Attribute
hxBoost = makeAttribute "hx-boost" "true"

hxConfirm :: Text -> Attribute
hxConfirm = makeAttribute "hx-confirm"

hxDisable :: Text -> Attribute
hxDisable = makeAttribute "hx-disable"

hxDisabledElt :: Text -> Attribute
hxDisabledElt = makeAttribute "hx-disabled-elt"

hxDisinherit :: Text -> Attribute
hxDisinherit = makeAttribute "hx-disinherit"

hxEncoding :: Text -> Attribute
hxEncoding = makeAttribute "hx-encoding"

hxExt :: Text -> Attribute
hxExt = makeAttribute "hx-ext"

hxHeaders :: Text -> Attribute
hxHeaders = makeAttribute "hx-headers"

hxHistory :: Text -> Attribute
hxHistory = makeAttribute "hx-history"

hxHistoryElt :: Text -> Attribute
hxHistoryElt = makeAttribute "hx-history-elt"

hxInclude :: Text -> Attribute
hxInclude = makeAttribute "hx-include"

hxIndicator :: Text -> Attribute
hxIndicator = makeAttribute "hx-indicator"

hxParams :: Text -> Attribute
hxParams = makeAttribute "hx-params"

hxPreserve :: Text -> Attribute
hxPreserve = makeAttribute "hx-preserve"

hxPrompt :: Text -> Attribute
hxPrompt = makeAttribute "hx-prompt"

hxReplaceUrl :: Text -> Attribute
hxReplaceUrl = makeAttribute "hx-replace-url"

hxRequest :: Text -> Attribute
hxRequest = makeAttribute "hx-request"

hxSync :: Text -> Attribute
hxSync = makeAttribute "hx-sync"

hxValidate :: Text -> Attribute
hxValidate = makeAttribute "hx-validate"

hxVars :: Text -> Attribute
hxVars = makeAttribute "hx-vars"
