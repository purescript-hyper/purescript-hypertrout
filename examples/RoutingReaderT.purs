module Examples.RoutingReaderT where

import Prelude
import Control.Monad.Indexed ((:*>))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer')
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Trout.Router (RoutingError, router)
import Text.Smolder.HTML (p)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML)
import Type.Trout.Method (Get)

data Greeting = Greeting String

type Site = "greeting" := Resource (Get Greeting HTML)

instance encodeHTMLGreeting :: EncodeHTML Greeting where
  encodeHTML (Greeting g) = p (text g)

runAppM ∷ ∀ a. String -> ReaderT String Aff a → Aff a
runAppM = flip runReaderT

site :: Proxy Site
site = Proxy

greetingResource
  :: forall m
   . Monad m
  => {"GET" :: ExceptT RoutingError (ReaderT String m) Greeting}
greetingResource =
  {"GET": Greeting <$> ask}

main :: Effect Unit
main =
  let app = router site {"greeting": greetingResource} onRoutingError

      onRoutingError status msg =
        writeStatus status
        :*> closeHeaders
        :*> respond (fromMaybe "" msg)

  in runServer' defaultOptionsWithLogging {} (runAppM "Hello") app
