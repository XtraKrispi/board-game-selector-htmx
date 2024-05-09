module View where

import Bgg (BggUsername (BggUsername), BoardGame (..))

import Data.List.Split (chunksOf)
import Data.Text qualified as T
import Htmx.Attributes (hxExt, hxGet, hxIndicator, hxPushUrl, hxSwap, hxTarget, hxTrigger, hxVals)
import Lucid
import Lucid.Base

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

homePage :: Maybe BggUsername -> Html ()
homePage mUser = doctypehtml_ do
  head_ do
    title_ "Board Game Selector"
    link_ [rel_ "stylesheet", href_ "/output.css"]
    script_ [src_ "https://unpkg.com/htmx.org@1.9.12"] ("" :: T.Text)
    script_ [src_ "https://unpkg.com/htmx.org@1.9.12/dist/ext/path-params.js"] ("" :: T.Text)
  body_
    [ hxExt "path-params"
    , class_ "min-w-screen min-h-screen bg-base-200"
    ]
    do
      div_ [class_ "flex flex-col items-center space-y-4"] do
        div_ [class_ "hero"] do
          div_ [class_ "hero-content text-center"] do
            div_ [class_ "max-w-md"] do
              h1_ [class_ "text-5xl font-bold"] "Board Game Selector"
              p_ [class_ "py-6"] "Type in a BGG username to see what board games they own, and even randomly select one!"
              form_
                [ hxIndicator "#indicator"
                , hxGet "/collection/{username}"
                , hxTarget "#collection"
                , hxSwap "outerHTML"
                , hxPushUrl "true"
                , class_ "flex flex-col space-y-4"
                ]
                do
                  div_ [class_ "flex space-x-2"] do
                    input_
                      [ type_ "text"
                      , placeholder_ "Board Game Geek Username"
                      , class_ "input input-bordered w-full max-w-xs"
                      , name_ "username"
                      ]
                    button_ [class_ "btn btn-primary"] "Go!"
                  div_ [id_ "indicator", class_ "htmx-indicator"] do
                    p_ [class_ "animate-pulse"] do
                      "Loading collection... please be aware, due to BGG APIs, this may take a long time"
        collection (maybe NoData OnLoad mUser)
      boardgameDrawer Nothing

boardgameDrawer :: Maybe BoardGame -> Html ()
boardgameDrawer Nothing = div_ [id_ "game", class_ "right-0 left-full h-screen bg-base-100/50 fixed top-0 w-1/3 transition-all"] ""
boardgameDrawer (Just bg) =
  div_
    [ id_ "game"
    , class_ "p-4 h-screen bg-base-100/50 fixed top-0 left-2/3 right-0 transition-all flex flex-col space-y-8 shadow-xl"
    ]
    do
      div_ [class_ "flex justify-between items-center"] do
        h2_ [class_ "text-3xl"] $ toHtml $ bg.title <> " (" <> tshow bg.publishedYear <> ")"
        button_
          [ hxGet "/game/clear"
          , hxTarget "#game"
          , hxSwap "outerHTML"
          , class_ "btn btn-square"
          ]
          do
            svg_
              [ class_ "h-6 w-6"
              , makeAttribute "fill" "none"
              , makeAttribute "viewBox" "0 0 24 24"
              , makeAttribute "stroke" "currentColor"
              ]
              do
                path_
                  [makeAttribute "stroke-linecap" "round", makeAttribute "stroke-linejoin" "round", makeAttribute "stroke-width" "2", makeAttribute "d" "M6 18L18 6M6 6l12 12"]

      div_ [class_ "flex flex-col items-center"] do
        img_ [src_ bg.imageUrl, class_ "w-1/2"]

boardgameTile :: BoardGame -> Html ()
boardgameTile bg = do
  div_
    [ hxGet "/game/{bggId}"
    , hxVals ("{\"bggId\": \"" <> tshow bg.bggId <> "\"}")
    , hxTarget "#game"
    , hxSwap "outerHTML"
    , class_ "relative hover:scale-125 hover:rotate-2 hover:z-10 transition-all w-32 bg-base-100 shadow-xl cursor-pointer"
    ]
    do
      figure_ do
        img_ [class_ "h-auto w-32 rounded-lg", src_ bg.thumbnailUrl]

data CollectionView
  = NoData
  | WithBoardgames [BoardGame]
  | OnLoad BggUsername

boardGameChunk :: [BoardGame] -> Html ()
boardGameChunk bgs =
  div_
    [class_ "grid gap-4"]
    (mconcat $ boardgameTile <$> bgs)

collection :: CollectionView -> Html ()
collection v = do
  let (attrs, content) = case v of
        NoData -> ([], mempty)
        OnLoad (BggUsername username) ->
          (
            [ hxTrigger "load"
            , hxGet ("/collection/" <> username)
            , hxIndicator "#indicator"
            , hxSwap "outerHTML"
            ]
          , mempty
          )
        WithBoardgames bgs ->
          let chunked = chunksOf (ceiling $ ((fromIntegral $ length bgs) / 5 :: Double)) bgs
           in ( []
              , div_ [class_ "grid grid-cols-2 md:grid-cols-5 gap-4"] do
                  mconcat $ boardGameChunk <$> chunked
              )
  div_
    ( [ id_ "collection"
      , class_ "w-1/2 flex justify-center"
      ]
        ++ attrs
    )
    content

path_ :: [Attribute] -> Html ()
path_ = with $ makeXmlElementNoEnd "path"

collectionError :: Html ()
collectionError = err "There was a problem retrieving the user's collection, please try again."

boardGameError :: Html ()
boardGameError = err "There was a problem retrieving details for the selected game, please try again."

err :: T.Text -> Html ()
err msg =
  div_ [class_ "alert alert-error"] do
    svg_
      [ class_ "stroke-current shrink-0 h-6 w-6"
      , makeAttribute "fill" "none"
      , makeAttribute "viewBox" "0 0 24 24"
      ]
      do
        path_
          [ makeAttribute "stroke-linecap" "round"
          , makeAttribute "stroke-linejoin" "round"
          , makeAttribute "stroke-width" "2"
          , makeAttribute "d" "M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"
          ]
    span_ (toHtml msg)