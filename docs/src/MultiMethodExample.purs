module MultiMethodExample where

import Control.Monad.Indexed ((:*>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textHTML)
import Effect (Effect)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Status (statusBadRequest)
import Hyper.Trout.Router (RoutingError(..), router)
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:<|>), type (:=), Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML)
import Type.Trout.Method (Get, Delete)
import Prelude hiding (div)

newtype User = User { name :: String }

-- start snippet routing-type
type MultiMethodExample =
  "user" := Resource (Get User HTML :<|> Delete User HTML)
-- end snippet routing-type

site :: Proxy MultiMethodExample
site = Proxy

getUser :: forall m. Monad m => ExceptT RoutingError m User
getUser =
  pure (User { name: "An existing user." })

deleteUser :: forall m. Monad m => ExceptT RoutingError m User
deleteUser =
  throwError (HTTPError { status: statusBadRequest
                        , message: Just "Not doing that, no..."
                        })

instance encodeHTMLUser :: EncodeHTML User where
  encodeHTML (User { name }) =
    h1 (text $ "User: " <> name)

main :: Effect Unit
main =
  let
      -- start snippet resources
      resources =
        { user: { "GET": getUser
                , "DELETE": deleteUser
                }
        }
      -- end snippet resources

      site3Router = router site resources onRoutingError

      onRoutingError status msg =
        writeStatus status
        :*> contentType textHTML
        :*> closeHeaders
        :*> respond (maybe "" id msg)

  in runServer defaultOptions {} site3Router
