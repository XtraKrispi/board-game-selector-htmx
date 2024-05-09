module App where

import Bgg (BggUsername (BggUsername), BoardGame)
import Bgg qualified
import Control.Concurrent.STM (TVar)
import Data.IntMap (IntMap)
import Data.Text.Lazy qualified as LT
import Htmx.Response (hxPushUrl)
import Lucid (renderText)
import Network.Wai.Middleware.Static
import View (CollectionView (WithBoardgames), boardGameError, boardgameDrawer, collection, collectionError, homePage)
import Web.Scotty

app :: TVar (IntMap BoardGame) -> ScottyM ()
app bgCache = do
  middleware (staticPolicy (addBase "public"))
  get "/" $ html $ renderText $ homePage Nothing
  get "/:username" do
    username <- BggUsername <$> captureParam "username"
    html $ renderText $ homePage (Just username)
  get "/collection/:username" do
    username <- BggUsername <$> captureParam "username"
    eColl <- liftIO $ Bgg.getCollection bgCache username
    hxPushUrl $ "/" <> LT.fromStrict (Bgg.getUsername username)
    html $
      renderText $
        either
          (const collectionError)
          (collection . WithBoardgames)
          eColl
  get "/game/:bggId" do
    bggId <- captureParam "bggId"
    eGame <- liftIO $ Bgg.getBoardGame bgCache bggId
    html $ renderText $ either (const boardGameError) (boardgameDrawer . Just) eGame
  get "/game/clear" do
    html $ renderText $ boardgameDrawer Nothing
