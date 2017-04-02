module Hyper.Routing.Router
       ( RoutingError(..)
       , class Router
       , route
       , class MethodRouter
       , routeMethod
       , router
       ) where

import Prelude
import Data.HTTP.Method as Method
import Data.StrMap as StrMap
import Hyper.Routing as Routing
import Control.IxMonad (ibind)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Array (elem, filter, null, uncons)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (textPlain)
import Data.StrMap (StrMap)
import Data.String (Pattern(..), split)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Hyper.Conn (Conn)
import Hyper.ContentNegotiation (AcceptHeader, NegotiationResult(..), negotiateContent, parseAcceptHeader)
import Hyper.Middleware (Middleware, lift')
import Hyper.Request (class Request, getRequestData)
import Hyper.Response (class ResponseWritable, class Response, ResponseEnded, StatusLineOpen, closeHeaders, contentType, end, respond, writeStatus)
import Hyper.Routing (type (:<|>), type (:>), Capture, CaptureAll, Lit, Raw, (:<|>))
import Hyper.Routing.ContentType (class AllMimeRender, allMimeRender)
import Hyper.Routing.PathPiece (class FromPathPiece, fromPathPiece)
import Hyper.Status (Status, statusBadRequest, statusMethodNotAllowed, statusNotAcceptable, statusNotFound, statusOK)
import Type.Proxy (Proxy(..))

type Method' = Either Method CustomMethod

type RoutingContext = { path :: Array String
                      , method :: Method'
                      }

data RoutingError
  = HTTPError { status :: Status
              , message :: Maybe String
              }

derive instance genericRoutingError :: Generic RoutingError _

instance eqRoutingError :: Eq RoutingError where
  eq = genericEq

instance showRoutingError :: Show RoutingError where
  show = genericShow

class Router e h r | e -> h, e -> r where
  route :: Proxy e -> RoutingContext -> h -> Either RoutingError r

class MethodRouter m cts h r | m -> cts
                             , m -> h
                             , m -> r where
  routeMethod :: Proxy m -> RoutingContext -> h -> Either RoutingError r

instance routerAltE :: (Router e1 h1 out, Router e2 h2 out)
                       => Router (e1 :<|> e2) (h1 :<|> h2) out where
  route _ context (h1 :<|> h2) =
    case route (Proxy :: Proxy e1) context h1 of
      Left err1 ->
        case route (Proxy :: Proxy e2) context h2 of
          -- The Error that's thrown depends on the Errors' HTTP codes.
          Left err2 -> throwError (selectError err1 err2)
          Right handler -> pure handler
      Right handler -> pure handler
    where
      fallbackStatuses = [statusNotFound, statusMethodNotAllowed]
      selectError (HTTPError errL) (HTTPError errR) =
        case Tuple errL.status errR.status of
          Tuple  s1 s2
            | s1 `elem` fallbackStatuses && s2 == statusNotFound -> HTTPError errL
            | s1 /= statusNotFound && s2 `elem` fallbackStatuses -> HTTPError errL
            | otherwise -> HTTPError errR


instance routerLit :: ( Router e h out
                      , IsSymbol lit
                      )
                      => Router (Lit lit :> e) h out where
  route _ ctx r =
    case uncons ctx.path of
      Just { head, tail } | head == expectedSegment ->
        route (Proxy :: Proxy e) ctx { path = tail} r
      Just _ -> throwError (HTTPError { status: statusNotFound
                                      , message: Nothing
                                      })
      Nothing -> throwError (HTTPError { status: statusNotFound
                                       , message: Nothing
                                       })
    where expectedSegment = reflectSymbol (SProxy :: SProxy lit)

instance routerCapture :: ( Router e h out
                          , FromPathPiece v
                          )
                          => Router (Capture c v :> e) (v -> h) out where
  route _ ctx r =
    case uncons ctx.path of
      Nothing -> throwError (HTTPError { status: statusNotFound
                                       , message: Nothing
                                       })
      Just { head, tail } ->
        case fromPathPiece head of
          Left err -> throwError (HTTPError { status: statusBadRequest
                                            , message: Just err
                                            })
          Right x -> route (Proxy :: Proxy e) ctx { path = tail } (r x)


instance routerCaptureAll :: ( Router e h out
                             , FromPathPiece v
                             )
                             => Router (CaptureAll c v :> e) (Array v -> h) out where
  route _ ctx r =
    case traverse fromPathPiece ctx.path of
      Left err -> throwError (HTTPError { status: statusBadRequest
                                        , message: Just err
                                        })
      Right xs -> route (Proxy :: Proxy e) ctx { path = [] } (r xs)


routeEndpoint :: forall e r method.
                 (IsSymbol method)
                 => Proxy e
                 -> RoutingContext
                 -> r
                 -> SProxy method
                 -> Either RoutingError r
routeEndpoint _ context r methodProxy = do
  unless (null context.path) $
    throwError (HTTPError { status: statusNotFound
                          , message: Nothing
                          })

  let expectedMethod = Method.fromString (reflectSymbol methodProxy)
  unless (expectedMethod == context.method) $
    throwError (HTTPError { status: statusMethodNotAllowed
                          , message: Just ("Method "
                                           <> show context.method
                                           <> " did not match "
                                           <> show expectedMethod
                                           <> ".")
                          })
  pure r

getAccept :: StrMap String -> Either String (Maybe AcceptHeader)
getAccept m =
  case StrMap.lookup "accept" m of
    Just a -> Just <$> parseAcceptHeader a
    Nothing -> pure Nothing

instance methodRouterMethod :: ( Monad m
                               , Request req m
                               , Response res m wb
                               , ResponseWritable wb m r
                               , IsSymbol method
                               , AllMimeRender body ct r
                               )
                           => MethodRouter
                              (Routing.Method method body)
                              ct
                              (ExceptT RoutingError m body)
                              (Middleware
                               m
                               { request :: req
                               , response :: (res StatusLineOpen)
                               , components :: c
                               }
                               { request :: req
                               , response :: (res ResponseEnded)
                               , components :: c
                               }
                               Unit)
  where
  routeMethod proxy context action = do
    let handler = lift' (runExceptT action) `ibind`
                  case _ of
                    Left (HTTPError { status }) -> do
                      writeStatus status
                      contentType textPlain
                      closeHeaders
                      end
                    Right body -> do
                      { headers } ← getRequestData
                      case getAccept headers of
                        Left err -> do
                          writeStatus statusBadRequest
                          contentType textPlain
                          closeHeaders
                          end
                        Right parsedAccept -> do
                          case negotiateContent parsedAccept (allMimeRender (Proxy :: Proxy ct) body) of
                            Match (Tuple ct rendered) -> do
                              writeStatus statusOK
                              contentType ct
                              closeHeaders
                              respond rendered
                            Default (Tuple ct rendered) -> do
                              writeStatus statusOK
                              contentType ct
                              closeHeaders
                              respond rendered
                            NotAcceptable _ -> do
                              writeStatus statusNotAcceptable
                              contentType textPlain
                              closeHeaders
                              end
    routeEndpoint proxy context handler (SProxy :: SProxy method)
    where bind = ibind

instance routerRaw :: (IsSymbol method)
                   => Router
                      (Raw method)
                      (Middleware
                       m
                       { request :: req
                       , response :: (res StatusLineOpen)
                       , components :: c
                       }
                       { request :: req
                       , response :: (res ResponseEnded)
                       , components :: c
                       }
                       Unit)
                      (Middleware
                       m
                       { request :: req
                       , response :: (res StatusLineOpen)
                       , components :: c
                       }
                       { request :: req
                       , response :: (res ResponseEnded)
                       , components :: c
                       }
                       Unit)
                      where
  route proxy context r =
    routeEndpoint proxy context r (SProxy :: SProxy method)


instance routerResourceMethods :: ( MethodRouter m1 cts h1 out
                                  , MethodRouter m2 cts h2 out
                                  )
                               => Router (Routing.Resource (m1 :<|> m2) cts) (h1 :<|> h2) out where
  route proxy ctx (h1 :<|> h2) =
    case routeMethod (Proxy :: Proxy m1) ctx h1 of
      Left _ -> routeMethod (Proxy :: Proxy m2) ctx h2
      Right r -> pure r



instance routerResource :: ( IsSymbol m
                           , MethodRouter (Routing.Method m r) cts h out
                           )
                        => Router (Routing.Resource (Routing.Method m r) cts) h out where
  route proxy =
    routeMethod (Proxy :: Proxy (Routing.Method m r))


router
  :: forall s r m req res c
   . ( Monad m
     , Request req m
     , Router s r (Middleware
                   m
                   (Conn req (res StatusLineOpen) c)
                   (Conn req (res ResponseEnded) c)
                   Unit)
     ) =>
     Proxy s
  -> r
  -> (Status
      -> Maybe String
      -> Middleware
         m
         (Conn req (res StatusLineOpen) c)
         (Conn req (res ResponseEnded) c)
         Unit)
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit

router site handler onRoutingError = do
  handler'
  -- Run the routing to get a handler.
  -- route (Proxy :: Proxy s) ctx handler
  -- Then, if successful, run the handler, possibly also generating an HTTPError.
  -- # either catch runHandler
  where
    splitUrl = filter ((/=) "") <<< split (Pattern "/")
    context requestData =
      { path: splitUrl requestData.url
      , method: requestData.method
      }
    catch (HTTPError { status, message }) =
      onRoutingError status message

    handler' ∷ Middleware
               m
               (Conn req (res StatusLineOpen) c)
               (Conn req (res ResponseEnded) c)
               Unit
    handler' = do
      ctx ← context <$> getRequestData
      case route site ctx handler of
        Left err → catch err
        Right h → h

    bind = ibind
