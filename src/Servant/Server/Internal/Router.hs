module Servant.Server.Internal.Router where

import           Control.Applicative
import           Control.Monad
import           Data.Map                                   (Map)
import qualified Data.Map                                   as M
import           Data.Monoid                                ((<>))
import           Data.Text                                  (Text)
--import           Network.Wai                 (Request, pathInfo)
import           Servant.Server.Internal.PathInfo
import           Servant.Server.Internal.RoutingApplication
import           Snap.Core
import           Snap.Snaplet

import Debug.Trace

-- | Internal representation of a router.
data Router b =
    WithRequest   (Request -> Router b)
      -- ^ current request is passed to the router
  | StaticRouter  (Map Text (Router b))
      -- ^ first path component used for lookup and removed afterwards
  | DynamicRouter (Text -> Router b)
      -- ^ first path component used for lookup and removed afterwards
  | LeafRouter    (Handler b b (RouteResult ()))
      -- ^ to be used for routes that match an empty path
  | Choice        (Router b) (Router b)
      -- ^ left-biased choice between two routers

-- | Smart constructor for the choice between routers.
-- We currently optimize the following cases:
--
--   * Two static routers can be joined by joining their maps.
--   * Two dynamic routers can be joined by joining their codomains.
--   * Two 'WithRequest' routers can be joined by passing them
--     the same request and joining their codomains.
--   * A 'WithRequest' router can be joined with anything else by
--     passing the same request to both but ignoring it in the
--     component that does not need it.
--
choice :: Router b -> Router b -> Router b
choice (StaticRouter table1) (StaticRouter table2) =
  StaticRouter (M.unionWith choice table1 table2)
choice (DynamicRouter fun1)  (DynamicRouter fun2)  =
  DynamicRouter (\ first -> choice (fun1 first) (fun2 first))
choice (WithRequest router1) (WithRequest router2) =
  WithRequest (\ request -> choice (router1 request) (router2 request))
choice (WithRequest router1) router2 =
  WithRequest (\ request -> choice (router1 request) router2)
choice router1 (WithRequest router2) =
  WithRequest (\ request -> choice router1 (router2 request))
choice router1 router2 = Choice router1 router2

-- | Interpret a router as an application.
runRouter
  :: Router b
  -> Handler b b (RouteResult ())

runRouter (WithRequest fRouter) = do
  getRequest >>= \r -> runRouter (fRouter r)
runRouter (StaticRouter table) = do
  request <- getRequest
  case processedPathInfo request of
    first : _
      | Just router <- M.lookup first table
      -> let request' = reqSafeTail request
         in  putRequest request' >> runRouter router
    _ -> pass
runRouter (DynamicRouter fun) = do
  request <- getRequest
  case processedPathInfo request of
    first : _
      -> let request' = reqSafeTail request
         in  putRequest request' >> runRouter (fun first)
    _ -> pass
runRouter (LeafRouter h) = h
runRouter (Choice r1 r2) =
  runRouter r1 <|> runRouter r2
