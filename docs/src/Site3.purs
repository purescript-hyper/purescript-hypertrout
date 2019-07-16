module Site3 where

import Control.Monad.Indexed ((:*>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Data.Argonaut (class EncodeJson, encodeJson, fromArray, jsonEmptyObject, (:=), (~>))
import Data.Array (find)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textHTML)
import Effect (Effect)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Status (statusNotFound)
import Hyper.Trout.Router (RoutingError(..), router)
import Text.Smolder.HTML (div, h1, li, p, ul)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:/), type (:<|>), type (:=), type (:>), Capture, Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML, linkTo)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Links (linksTo)
import Type.Trout.Method (Get)
import Prelude hiding (div)

data Home = Home

newtype User = User { id :: Int, name :: String }

instance encodeJsonUser :: EncodeJson User where
  encodeJson (User { id, name }) =
    "id" := show id
    ~> "name" := name
    ~> jsonEmptyObject


data AllUsers = AllUsers (Array User)

instance encodeJsonAllUsers :: EncodeJson AllUsers where
  encodeJson (AllUsers users) = fromArray (map encodeJson users)

-- start snippet routing-type
type Site3 =
       "home"  := Resource (Get Home HTML)
  :<|> "users" := "users" :/ Resource (Get AllUsers (HTML :<|> JSON))
  :<|> "user"  := "users" :/ Capture "user-id" Int
                          :> Resource (Get User (HTML :<|> JSON))
-- end snippet routing-type

site3 :: Proxy Site3
site3 = Proxy

homeResource :: forall m. Monad m => {"GET" :: ExceptT RoutingError m Home}
homeResource = {"GET": pure Home}

usersResource :: forall m. Monad m => {"GET" :: ExceptT RoutingError m AllUsers}
usersResource = {"GET": AllUsers <$> getUsers}

userResource :: forall m. Monad m => Int -> {"GET" :: ExceptT RoutingError m User}
userResource id' =
  {"GET":
   find (\(User u) -> u.id == id') <$> getUsers >>=
   case _ of
       Just user -> pure user
       Nothing ->
         throwError (HTTPError { status: statusNotFound
                                 , message: Just "User not found."
                                 })
  }

instance encodeHTMLHome :: EncodeHTML Home where
  encodeHTML Home =
    let {users} = linksTo site3
    in p do
      text "Welcome to my site! Go check out my "
      linkTo users (text "Users")
      text "."

instance encodeHTMLAllUsers :: EncodeHTML AllUsers where
  encodeHTML (AllUsers users) =
    div do
      h1 (text "Users")
      ul (traverse_ linkToUser users)
    where
      linkToUser (User u) =
        let {user} = linksTo site3
        in li (linkTo (user u.id) (text u.name))

instance encodeHTMLUser :: EncodeHTML User where
  encodeHTML (User { name }) =
    h1 (text name)

getUsers :: forall m. Applicative m => m (Array User)
getUsers =
  pure
  [ User { id: 1, name: "John Paul Jones" }
  , User { id: 2, name: "Tal Wilkenfeld" }
  , User { id: 3, name: "John Patitucci" }
  , User { id: 4, name: "Jaco Pastorious" }
  ]

main :: Effect Unit
main =
  let resources = { home: homeResource
                  , users: usersResource
                  , user: userResource
                  }

      site3Router =
        router site3 resources onRoutingError

      onRoutingError status msg =
        writeStatus status
        :*> contentType textHTML
        :*> closeHeaders
        :*> respond (maybe "" identity msg)

  in runServer defaultOptions {} site3Router
