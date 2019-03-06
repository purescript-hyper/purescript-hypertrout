module Site1 where

import Prelude
import Control.Monad.Indexed ((:*>))
import Data.Maybe (maybe)
import Data.MediaType.Common (textHTML)
import Effect (Effect)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (contentType, respond, closeHeaders, writeStatus)
import Hyper.Trout.Router (router)
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
main :: Effect Unit
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
