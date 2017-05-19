module Hyper.Trout.TestSite where

import Prelude
import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.String (trim)
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:/), type (:<|>), type (:>), Capture, CaptureAll, Raw, Resource, QueryParam)
import Type.Trout.ContentType.HTML (HTML, class EncodeHTML)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get, Post)
import Type.Trout.PathPiece (class FromPathPiece, class ToPathPiece)

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
  "profile" :/ Resource (Get User JSON)
  :<|> "friends" :/ Resource (Get (Array User) JSON :<|> Post User JSON)

type TestSite =
  Resource (Get Home (HTML :<|> JSON))
  -- nested routes with capture
  :<|> "users" :/ Capture "user-id" UserID :> UserResources
  -- capture all
  :<|> "wiki" :/ CaptureAll "segments" String :> Resource (Get WikiPage HTML)
  -- query string parameters
  :<|> "search" :/ QueryParam "q" String :> Resource (Get (Maybe User) JSON)
  -- raw middleware
  :<|> "about" :/ Raw "GET"

testSite :: Proxy TestSite
testSite = Proxy
