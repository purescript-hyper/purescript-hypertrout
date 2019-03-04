module Hyper.Trout.RouterSpec (spec) where

import Prelude
import Control.Monad.Indexed ((:*>))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(POST, GET))
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textPlain)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as F
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, evalMiddleware)
import Hyper.Request (class Request)
import Hyper.Response (class Response, contentType, headers, respond, class ResponseWritable, ResponseEnded, StatusLineOpen, closeHeaders, writeStatus)
import Hyper.Status (statusBadRequest, statusMethodNotAllowed, statusOK)
import Hyper.Test.TestServer (TestResponse(..), TestRequest(..), defaultRequest, testHeaders, testServer, testStatus, testStringBody)
import Hyper.Trout.TestSite (Home(..), User(..), UserID(..), WikiPage(..), testSite)
import Hyper.Trout.Router (router)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

homeResource :: forall m. Monad m => {"GET" :: m Home}
homeResource = {"GET": pure Home}

profileResource :: forall m. Monad m => UserID -> {"GET" :: m User}
profileResource userId = {"GET": pure (User userId)}

friendsResource
  :: forall m
   . Monad m
  => UserID
  -> { "GET" :: m (Array User)
     , "POST" :: m User
     }
friendsResource (UserID uid) =
  { "GET": pure [ User (UserID "foo")
               , User (UserID "bar")
               ]
  -- TODO: add ReqBody when supported
  , "POST": pure (User (UserID "new-user"))
  }

wikiResource :: forall m. Monad m => Array String -> {"GET" :: m WikiPage}
wikiResource segments = {"GET": pure (WikiPage (joinWith "/" segments))}

aboutMiddleware
  :: forall m req res c r
   . Monad m
  => Request req m
  => Response res m r
  => ResponseWritable r m String
  => Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
aboutMiddleware = do
  writeStatus statusOK
  :*> contentType textPlain
  :*> closeHeaders
  :*> respond "This is a test."

searchResource :: forall f m. Functor f => Monad m => f String -> {"GET" :: m (f User)}
searchResource q =
  {"GET": pure $ User <<< UserID <$> q}

spec :: Spec Unit
spec =
  describe "Hyper.Routing.Router" do
    let userResources userId = { profile: profileResource userId
                               , friends: friendsResource userId
                               }
        resources = { home: homeResource
                    , user: userResources
                    , wiki: wikiResource
                    , about: aboutMiddleware
                    , search: searchResource
                    , searchMany: searchResource
                    }

        onRoutingError status msg = do
          writeStatus status
          :*> headers []
          :*> respond (maybe "" identity msg)

        makeRequestWithHeaders method path headers =
          { request: TestRequest defaultRequest { method = Left method
                                                , url = path
                                                , headers = headers
                                                }
          , response: TestResponse Nothing [] []
          , components: {}
          }
          # evalMiddleware (router testSite resources onRoutingError)
          # testServer

        makeRequest method path =
          makeRequestWithHeaders method path (F.empty :: Object String)

    describe "router" do
      it "matches root" do
        conn <- makeRequest GET "/"
        testStringBody conn `shouldEqual` "<h1>Home</h1>"

      it "considers Accept header for multi-content-type resources" do
        conn <- makeRequestWithHeaders GET "/" (F.singleton "accept" "application/json")
        testStatus conn `shouldEqual` Just statusOK
        testStringBody conn `shouldEqual` "{}"

      it "validates based on custom Capture instance" do
        conn <- makeRequest GET "/users/ /profile"
        testStatus conn `shouldEqual` Just statusBadRequest
        testStringBody conn `shouldEqual` "UserID must not be blank."

      it "matches nested routes" do
        conn <- makeRequest GET "/users/owi/profile"
        testStringBody conn `shouldEqual` "{\"userId\":\"owi\"}"

      it "ignores extraneous query string parameters" do
        conn <- makeRequest GET "/users/owi/profile?bugs=bunny"
        testStringBody conn `shouldEqual` "{\"userId\":\"owi\"}"

      it "supports arrays of JSON values" do
        conn <- makeRequest GET "/users/owi/friends"
        testStringBody conn `shouldEqual` "[{\"userId\":\"foo\"},{\"userId\":\"bar\"}]"

      it "supports second method of resource with different representation" do
        conn <- makeRequest POST "/users/owi/friends"
        testStringBody conn `shouldEqual` "{\"userId\":\"new-user\"}"

      it "matches CaptureAll route" do
        conn <- makeRequest GET "/wiki/foo/bar/baz.txt"
        testStringBody conn `shouldEqual` "Viewing page: foo&#x2F;bar&#x2F;baz.txt"

      it "matches QueryParam route" do
        conn <- makeRequest GET "/search?q=bunny"
        testStringBody conn `shouldEqual` "{\"userId\":\"bunny\"}"

      it "matches QueryParam route with empty value" do
        conn <- makeRequest GET "/search?q"
        testStringBody conn `shouldEqual` "{\"userId\":\"\"}"

      it "matches QueryParam route with missing key" do
        conn <- makeRequest GET "/search?r=bunny"
        testStringBody conn `shouldEqual` "null"

      it "matches QueryParams route" do
        conn <- makeRequest GET "/search-many?q=bugs&q=bunny"
        testStringBody conn `shouldEqual` "[{\"userId\":\"bugs\"},{\"userId\":\"bunny\"}]"

      it "matches QueryParams route with empty value" do
        conn <- makeRequest GET "/search-many?q&q=bunny"
        testStringBody conn `shouldEqual` "[{\"userId\":\"\"},{\"userId\":\"bunny\"}]"

      it "matches QueryParams route with missing key" do
        conn <- makeRequest GET "/search-many?p&q=bunny"
        testStringBody conn `shouldEqual` "[{\"userId\":\"bunny\"}]"

      it "matches Raw route" do
        conn <- makeRequest GET "/about"
        testHeaders conn `shouldEqual` [ Tuple "Content-Type" "text/plain" ]
        testStringBody conn `shouldEqual` "This is a test."

      it "checks HTTP method" do
        conn <- makeRequest POST "/"
        testStatus conn `shouldEqual` Just statusMethodNotAllowed
