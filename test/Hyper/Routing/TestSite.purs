module Hyper.Routing.TestSite where

import Prelude
import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Either (Either(..))
import Data.String (trim)
import Hyper.Routing (type (:/), type (:<|>), type (:>), Capture, CaptureAll, Raw, Resource)
import Hyper.Routing.ContentType.HTML (HTML, class EncodeHTML)
import Hyper.Routing.ContentType.JSON (JSON)
import Hyper.Routing.Method (Get, Post)
import Hyper.Routing.PathPiece (class FromPathPiece, class ToPathPiece)
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))

data Home = Home

instance encodeJsonHome :: EncodeJson Home where
  encodeJson Home = jsonEmptyObject

instance encodeHTMLHome :: EncodeHTML Home where
  encodeHTML Home = h1 (text "Home")

newtype UserID = UserID String

instance fromPathPieceUserID :: FromPathPiece UserID where
  fromPathPiece s =
    case trim s of
      "" -> Left "UserID must not be blank."
      s' -> Right (UserID s')

instance toPathPieceUserID :: ToPathPiece UserID where
  toPathPiece (UserID s) = s

data User = User UserID

instance encodeUser :: EncodeJson User where
  encodeJson (User (UserID userId)) =
    "userId" := userId
    ~> jsonEmptyObject

data WikiPage = WikiPage String

instance encodeHTMLWikiPage :: EncodeHTML WikiPage where
  encodeHTML (WikiPage title) = text ("Viewing page: " <> title)


type UserResources =
  "profile" :/ Resource (Get User) JSON
  :<|> "friends" :/ Resource (Get (Array User) :<|> Post User) JSON

type TestSite =
  Resource (Get Home) (HTML :<|> JSON)
  -- nested routes with capture
  :<|> "users" :/ Capture "user-id" UserID :> UserResources
  -- capture all
  :<|> "wiki" :/ CaptureAll "segments" String :> Resource (Get WikiPage) HTML
  -- raw middleware
  :<|> "about" :/ Raw "GET"

testSite :: Proxy TestSite
testSite = Proxy
