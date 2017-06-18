module Site2 where

import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Data.Array (find)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textHTML)
import Data.Traversable (traverse_)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Status (statusNotFound)
import Hyper.Trout.Router (RoutingError(..), router)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)
import Text.Smolder.HTML (div, h1, li, p, ul)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:/), type (:<|>), type (:=), type (:>), Capture, Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML, linkTo)
import Type.Trout.Links (linksTo)
import Type.Trout.Method (Get)
import Prelude hiding (div)

-- start snippet resources-and-type
data Home = Home

data AllUsers = AllUsers (Array User)

newtype User = User { id :: Int, name :: String }

type Site2 =
       "home"  := Resource (Get Home HTML)
  :<|> "users" := "users" :/ Resource (Get AllUsers HTML)
  :<|> "user"  := "users" :/ Capture "user-id" Int
                          :> Resource (Get User HTML)

site2 :: Proxy Site2
site2 = Proxy
-- end snippet resources-and-type

-- start snippet handlers
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
-- end snippet handlers

-- start snippet encoding
instance encodeHTMLHome :: EncodeHTML Home where
  encodeHTML Home =
    let {users} = linksTo site2
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
        let {user} = linksTo site2
        in li (linkTo (user u.id) (text u.name))

instance encodeHTMLUser :: EncodeHTML User where
  encodeHTML (User { name }) =
    h1 (text name)
-- end snippet encoding

-- start snippet get-users
getUsers :: forall m. Applicative m => m (Array User)
getUsers =
  pure
  [ User { id: 1, name: "John Paul Jones" }
  , User { id: 2, name: "Tal Wilkenfeld" }
  , User { id: 3, name: "John Patitucci" }
  , User { id: 4, name: "Jaco Pastorious" }
  ]
-- end snippet get-users

-- start snippet main
main :: forall e. Eff (http :: HTTP, console :: CONSOLE, buffer :: BUFFER | e) Unit
main =
  let resources = { home: homeResource
                  , users: usersResource
                  , user: userResource
                  }

      otherSiteRouter =
        router site2 resources onRoutingError

      onRoutingError status msg =
        writeStatus status
        :*> contentType textHTML
        :*> closeHeaders
        :*> respond (maybe "" id msg)

  in runServer defaultOptions {} otherSiteRouter
-- end snippet main
