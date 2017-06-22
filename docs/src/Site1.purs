module Site1 where

import Prelude
import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (maybe)
import Data.MediaType.Common (textHTML)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (contentType, respond, closeHeaders, writeStatus)
import Hyper.Trout.Server (router)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)
import Text.Smolder.HTML (p)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML)
import Type.Trout.Method (Get)

-- start snippet routing-type
data Home = Home

type Site1 = Resource (Get Home HTML)
-- end snippet routing-type

-- start snippet handler
home :: forall m. Applicative m => {"GET" :: m Home}
home = {"GET": pure Home}
-- end snippet handler

-- start snippet encoding
instance encodeHTMLHome :: EncodeHTML Home where
  encodeHTML Home =
    p (text "Welcome to my site!")
-- end snippet encoding

-- start snippet proxy
site1 :: Proxy Site1
site1 = Proxy
-- end snippet proxy

-- start snippet main
main :: forall e. Eff (http :: HTTP, console :: CONSOLE, buffer :: BUFFER | e) Unit
main =
  runServer defaultOptions {} siteRouter
-- end snippet main
  where

    -- start snippet router
    onRoutingError status msg =
      writeStatus status
      :*> contentType textHTML
      :*> closeHeaders
      :*> respond (maybe "" id msg)

    siteRouter = router site1 home onRoutingError
    -- end snippet router
