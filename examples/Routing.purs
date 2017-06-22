module Examples.Routing where

import Prelude
import Control.IxMonad ((:*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Array (find, (..))
import Data.Foldable (traverse_)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textHTML)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Status (statusNotFound)
import Hyper.Trout.Server (RoutingError(..), router)
import Node.HTTP (HTTP)
import Text.Smolder.HTML (h1, li, nav, p, section, ul)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:/), type (:<|>), type (:=), type (:>), Capture, Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML, linkTo)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Links (linksTo)
import Type.Trout.Method (Get)

type PostID = Int

newtype Post = Post { id :: PostID
                    , title :: String
                    }

derive instance genericPost :: Generic Post

instance encodeJsonPost :: EncodeJson Post where
  encodeJson (Post { id, title }) =
    "id" := id
    ~> "title" := title
    ~> jsonEmptyObject

instance encodeHTMLPost :: EncodeHTML Post where
  encodeHTML (Post { id: postId, title}) =
    let {posts} = linksTo site
    in section do
      h1 (text title)
      p (text "Contents...")
      nav (linkTo posts (text "All Posts"))

newtype PostsView = PostsView (Array Post)

derive instance genericPostsView :: Generic PostsView

instance encodeJsonPostsView :: EncodeJson PostsView where
  encodeJson = gEncodeJson

instance encodeHTMLPostsView :: EncodeHTML PostsView where
  encodeHTML (PostsView posts) =
    let {post} = linksTo site
        postLink (Post { id: postId, title }) =
          li (linkTo (post postId) (text title))
    in section do
        h1 (text "Posts")
        ul (traverse_ postLink posts)

type Site =
  "posts" := Resource (Get PostsView (HTML :<|> JSON))
  :<|> "post" := "posts" :/ Capture "id" PostID :> Resource (Get Post (HTML :<|> JSON))

site :: Proxy Site
site = Proxy

type AppM e a = ExceptT RoutingError (Aff e) a

-- This would likely be a database query in
-- a real app:
allPosts :: forall e. AppM e (Array Post)
allPosts = pure (map (\i -> Post { id: i, title: "Post #" <> show i }) (1..10))

postsResource :: forall e. { "GET" :: AppM e PostsView }
postsResource = { "GET": PostsView <$> allPosts }

postResource :: forall e. PostID -> { "GET" :: AppM e Post }
postResource postId =
  { "GET":
    find (\(Post p) -> p.id == postId) <$> allPosts >>=
    case _ of
        Just post -> pure post
        -- You can throw 404 Not Found in here as well.
        Nothing -> throwError (HTTPError { status: statusNotFound
                                        , message: Just "Post not found."
                                        })
  }

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR | e) Unit
main =
  runServer defaultOptions {} siteRouter
  where
    siteRouter = router
                 site
                 { posts: postsResource
                 , post: postResource
                 }
                 onRoutingError
    onRoutingError status msg = do
      writeStatus status
      :*> contentType textHTML
      :*> closeHeaders
      :*> respond (maybe "" id msg)
