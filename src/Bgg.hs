{-# LANGUAGE RecordWildCards #-}

module Bgg (getCollection, getBoardGames, getBoardGame, MyException (..), BggUsername (..), BoardGame (..)) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar, readTVarIO)
import Control.Monad (when)
import Control.Retry (RetryStatus (rsIterNumber), exponentialBackoff, retrying)
import Data.IntMap (IntMap)
import Data.IntMap qualified as Map
import Data.List (sortOn, (\\))
import Data.Maybe (catMaybes, listToMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client.Conduit (defaultManagerSettings)
import Network.HTTP.Conduit (ManagerSettings (managerResponseTimeout), httpLbs, newManager, responseTimeoutNone)
import Network.HTTP.Simple (getResponseBody, getResponseStatus, getResponseStatusCode)
import Network.HTTP.Types (Status)
import Text.Read (readMaybe)
import Text.XML
import Text.XML.Cursor

newtype BggUsername = BggUsername {getUsername :: Text}

data MyException
  = HttpException Status
  | XmlParseException String
  | NotFoundException Int
  | UnexpectedException
  deriving (Show)

data BoardGame = BoardGame
  { bggId :: Int
  , title :: Text
  , publishedYear :: Int
  , imageUrl :: Text
  , thumbnailUrl :: Text
  , series :: Maybe Text
  }
  deriving (Show)

parseBoardGame :: Cursor -> Maybe BoardGame
parseBoardGame item =
  let
    getElementContent elemName = T.concat $ descendant item >>= element elemName >>= descendant >>= content
    mBggId = readMaybe $ T.unpack $ T.concat $ attribute "id" item
    title = T.concat $ descendant item >>= element "name" >>= attributeIs "type" "primary" >>= attribute "value"
    mPublishedYear = readMaybe $ T.unpack $ T.concat $ descendant item >>= element "yearpublished" >>= attribute "value"
    thumbnailUrl = getElementContent "thumbnail"
    imageUrl = getElementContent "image"
    series = listToMaybe $ map (T.replace "Series: " "") $ filter ("Series: " `T.isInfixOf`) $ descendant item >>= element "link" >>= attributeIs "type" "boardgamefamily" >>= attribute "value"
   in
    (\bggId publishedYear -> BoardGame{..}) <$> mBggId <*> mPublishedYear

parseBoardGames :: Document -> [BoardGame]
parseBoardGames doc =
  let items = element "items" (fromDocument doc) >>= descendant >>= element "item"
   in catMaybes $ parseBoardGame <$> items

getCollection :: TVar (IntMap BoardGame) -> BggUsername -> IO (Either MyException [BoardGame])
getCollection cache username = do
  coll <- makeCollectionRequest username
  case coll of
    Right c -> do
      let ids = catMaybes $ readMaybe . T.unpack <$> (descendant (fromDocument c) >>= element "item" >>= attribute "objectid")
      getBoardGames cache ids
    Left e -> pure $ Left e

getBoardGames :: TVar (IntMap BoardGame) -> [Int] -> IO (Either MyException [BoardGame])
getBoardGames cache ids = do
  existingBgs <- Map.elems <$> readTVarIO cache
  let applicableIds = ids \\ ((.bggId) <$> existingBgs)
  newBoardGames <-
    if null applicableIds
      then pure (Right [])
      else do
        fmap parseBoardGames <$> makeThingRequest applicableIds
  case newBoardGames of
    Left e -> pure $ Left e
    Right bgs -> do
      atomically $ modifyTVar cache (\c_ -> foldr (\bg -> Map.insert (bg.bggId) bg) c_ bgs)
      let merged = sortOn ((.title)) $ bgs ++ filter (\bg -> bg.bggId `elem` ids) existingBgs
      pure $ Right merged

getBoardGame :: TVar (IntMap BoardGame) -> Int -> IO (Either MyException BoardGame)
getBoardGame cache bggId = do
  bgs <- getBoardGames cache [bggId]
  case bgs of
    Right [bg] -> pure $ Right bg
    Right [] -> pure $ Left $ NotFoundException bggId
    Right _ -> pure $ Left UnexpectedException
    Left ex -> pure $ Left ex

makeCollectionRequest :: BggUsername -> IO (Either MyException Document)
makeCollectionRequest (BggUsername username) =
  makeBggRequest $ "https://boardgamegeek.com/xmlapi2/collection?username=" <> username <> "&excludesubtype=boardgameexpansion&subtype=boardgame&brief=1&own=1"

makeThingRequest :: [Int] -> IO (Either MyException Document)
makeThingRequest thingId =
  makeBggRequest $ "https://boardgamegeek.com/xmlapi2/thing?id=" <> T.intercalate "," (T.pack . show <$> thingId) <> "&comments=0&marketplace=0&versions=0&videos=0"

makeBggRequest :: Text -> IO (Either MyException Document)
makeBggRequest url = do
  manager <- newManager (defaultManagerSettings{managerResponseTimeout = responseTimeoutNone})
  resp <-
    retrying
      (exponentialBackoff (2 * 1000000))
      (\_ resp -> pure $ 202 == getResponseStatusCode resp)
      ( \rs -> do
          putStrLn $ "Attempting http request: " ++ T.unpack url
          when (rs.rsIterNumber > 0) do
            putStrLn $ "Rety number: " ++ show rs.rsIterNumber
          httpLbs (fromString $ T.unpack url) manager
      )
  case getResponseStatusCode resp of
    200 -> case parseLBS def $ getResponseBody resp of
      Left ex -> pure . Left . XmlParseException $ show ex
      Right d -> pure $ Right d
    _ -> pure . Left . HttpException $ getResponseStatus resp