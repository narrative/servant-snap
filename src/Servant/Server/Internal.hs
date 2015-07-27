{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE OverlappingInstances #-}

module Servant.Server.Internal
  ( module Servant.Server.Internal
  , module Servant.Server.Internal.PathInfo
  , module Servant.Server.Internal.Router
  , module Servant.Server.Internal.RoutingApplication
  , module Servant.Server.Internal.ServantErr
  ) where

import           Control.Applicative         ((<$>))
import           Control.Monad.Trans.Either  (EitherT)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import           Data.CaseInsensitive        (mk)
import qualified Data.Map                    as M
import           Data.Maybe                  (catMaybes, fromMaybe, mapMaybe)
import           Data.String                 (fromString)
import           Data.String.Conversions     (cs, (<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Typeable
import           GHC.TypeLits                (KnownSymbol, symbolVal)
import           Network.HTTP.Types          (QueryText, parseQueryText)
import Network.Wai (rawQueryString) --TODO temporary
import           Snap.Core                   hiding (Headers, getHeaders,
                                              getResponse, route)
import           Snap.Snaplet
import           Servant.API                 ((:<|>) (..), (:>), Capture,
                                               Delete, Get, Header,
                                              MatrixFlag, MatrixParam, MatrixParams,
                                              Patch, Post, Put, QueryFlag,
                                              QueryParam, QueryParams, Raw,
                                              ReqBody)
import           Servant.API.ContentTypes    (AcceptHeader (..),
                                              AllCTRender (..),
                                              AllCTUnrender (..))
import           Servant.API.ResponseHeaders (Headers, getResponse, GetHeaders,
                                              getHeaders)
import           Servant.Common.Text         (FromText, fromText)

import           Servant.Server.Internal.PathInfo
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServantErr
import           Servant.Server.Internal.SnapShims

class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout -> Snap (RouteResult (Server layout m)) -> Router m

type Server layout m = ServerT layout m

-- * Instances

-- | A server for @a ':<|>' b@ first tries to match the request against the route
--   represented by @a@ and if it fails tries @b@. You must provide a request
--   handler for each route.
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post '[JSON] Book -- POST /books
-- >
-- > server :: Server MyApi
-- > server = listAllBooks :<|> postBook
-- >   where listAllBooks = ...
-- >         postBook book = ...
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where

  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route Proxy server = choice (route pa (extractL <$> server))
                              (route pb (extractR <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

captured :: FromText a => proxy (Capture sym a) -> Text -> Maybe a
captured _ = fromText

-- | If you use 'Capture' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by the 'Capture'.
-- This lets servant worry about getting it from the URL and turning
-- it into a value of the type you specify.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = getBook
-- >   where getBook :: Text -> EitherT ServantErr IO Book
-- >         getBook isbn = ...
instance (KnownSymbol capture, FromText a, HasServer sublayout)
      => HasServer (Capture capture a :> sublayout) where

  type ServerT (Capture capture a :> sublayout) b =
     a -> ServerT sublayout b

  route Proxy subserver =
    DynamicRouter $ \ first ->
      route (Proxy :: Proxy sublayout)
            (case captured captureProxy first of
               Nothing  -> return $ failWith NotFound
               Just v   -> feedTo subserver v)
    where captureProxy = Proxy :: Proxy (Capture capture a)


methodRouter :: (AllCTRender ctypes a, MonadSnap m)
             => Method -> Proxy ctypes -> Status
             -> Snap (RouteResult (EitherT ServantErr Snap a))
             -> Router m
             -- -> m (RouteResult (EitherT ServantErr m a))
             -- -> Router Request (RoutingApplication m) m
methodRouter method proxy status action =
  LeafRouter (liftSnap $ do
    resp <- route'
    either (const pass) putResponse (routeResult resp)
    return (RR (Right ())))

  where
    route' = do
      request <- getRequest
      case (pathIsEmpty request, rqMethod request == method) of
        (True, True) -> do
          runAction action return $ \ output -> do
            let accH = fromMaybe ct_wildcard $ getHeader (mk "Accept") request
            case handleAcceptH proxy (AcceptHeader accH) output of
              Nothing -> failWith UnsupportedMediaType
              Just (contentT, body) -> succeedWith $
                responseLBS status [ ("Content-Type" , cs contentT)] body
        (True,False) ->
          pass -- respond $ failWith WrongMethod
        _ -> pass -- respond $ failWith NotFound

methodRouterHeaders :: (GetHeaders (Headers h v), AllCTRender ctypes v, MonadSnap m)
                    => Method -> Proxy ctypes -> Status
                    -> Snap (RouteResult (EitherT ServantErr Snap (Headers h v)))
                    -> Router m
                    -- -> m (RouteResult (EitherT ServantErr m (Headers h v)))
                    -- -> Router Request (RoutingApplication m) m
methodRouterHeaders method proxy status action =
  LeafRouter $ liftSnap $ do
    resp <- route'
    either (const pass) putResponse (routeResult resp)
    return (RR (Right ()))
  where
    route' = do
      request <- getRequest
      case (pathIsEmpty request,  rqMethod request == method) of
       (True,True) -> do
        runAction action return $ \ output -> do
          let accH = fromMaybe ct_wildcard $ getHeader (mk "Accept") request
              headers = getHeaders output
          case handleAcceptH proxy (AcceptHeader accH) (getResponse output) of
            Nothing -> failWith UnsupportedMediaType
            Just (contentT, body) -> succeedWith $
              responseLBS status ( ("Content-Type" , cs contentT) : headers) body
       (True,False) -> do
          pass -- respond $ failWith WrongMethod
       _  ->  pass -- respond $ failWith NotFound

methodRouterEmpty :: Method
                  -> Snap (RouteResult (EitherT ServantErr Snap ()))
                  -> Router (Handler b v)
                  -- -> m (RouteResult (EitherT ServantErr m ()))
                  -- -> Router Request (RoutingApplication m) m
methodRouterEmpty method action =
  LeafRouter $ liftSnap $ do
    resp <- route'
    either (const pass) putResponse (routeResult resp)
    return (RR (Right ()))
  where
    route' = do
      request <- getRequest
      case (pathIsEmpty request, rqMethod request == method) of
       (True, True) -> do
          runAction action return $ \ () ->
            succeedWith $ responseLBS noContent204 [] ""
       (True,False) ->
          pass --respond $ failWith WrongMethod
       _ -> pass --respond $ failWith NotFound

-- | If you have a 'Delete' endpoint in your API,
-- the handler for this endpoint is meant to delete
-- a resource.
--
-- The code of the handler will, just like
-- for 'Servant.API.Get.Get', 'Servant.API.Post.Post' and
-- 'Servant.API.Put.Put', run in @EitherT ServantErr IO ()@.
-- The 'Int' represents the status code and the 'String' a message
-- to be returned. You can use 'Control.Monad.Trans.Either.left' to
-- painlessly error out if the conditions for a successful deletion
-- are not met.
instance
         {-# OVERLAPPABLE #-}
         ( AllCTRender ctypes a
         ) => HasServer (Delete ctypes a) where

  type ServerT (Delete ctypes a) m = m a

  route Proxy = methodRouter DELETE (Proxy :: Proxy ctypes) ok200

instance
         {-# OVERLAPPING #-}
         HasServer (Delete ctypes ()) where

  type ServerT (Delete ctypes ()) m = m ()

  route Proxy = methodRouterEmpty DELETE

-- Add response headers
instance
         {-# OVERLAPPING #-}
         ( GetHeaders (Headers h v), AllCTRender ctypes v
         ) => HasServer (Delete ctypes (Headers h v)) where

  type ServerT (Delete ctypes (Headers h v)) m = m (Headers h v)

  route Proxy = methodRouterHeaders DELETE (Proxy :: Proxy ctypes) ok200

-- | When implementing the handler for a 'Get' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Post.Post'
-- and 'Servant.API.Put.Put', the handler code runs in the
-- @EitherT ServantErr IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we use the type-level list, combined
-- with the request's @Accept@ header, to encode the value for you
-- (returning a status code of 200). If there was no @Accept@ header or it
-- was @*\/\*@, we return encode using the first @Content-Type@ type on the
-- list.
instance
         {-# OVERLAPPABLE #-}
         ( AllCTRender ctypes a ) => HasServer (Get ctypes a) where

  type ServerT (Get ctypes a) m = m a

  route Proxy = methodRouter GET (Proxy :: Proxy ctypes) ok200

-- '()' ==> 204 No Content
instance
         {-# OVERLAPPING #-}
          HasServer (Get ctypes ()) where

  type ServerT (Get ctypes ()) m = m ()

  route Proxy = methodRouterEmpty GET

-- Add response headers
instance
         {-# OVERLAPPING #-}
          ( GetHeaders (Headers h v), AllCTRender ctypes v
          ) => HasServer (Get ctypes (Headers h v)) where

  type ServerT (Get ctypes (Headers h v)) m = m (Headers h v)

  route Proxy = methodRouterHeaders GET (Proxy :: Proxy ctypes) ok200

-- | If you use 'Header' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'Header'.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
-- All it asks is for a 'FromText' instance.
--
-- Example:
--
-- > newtype Referer = Referer Text
-- >   deriving (Eq, Show, FromText, ToText)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get '[JSON] Referer
-- >
-- > server :: Server MyApi
-- > server = viewReferer
-- >   where viewReferer :: Referer -> EitherT ServantErr IO referer
-- >         viewReferer referer = return referer
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (Header sym a :> sublayout) where

  type ServerT (Header sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let mheader = fromText . decodeUtf8 =<< getHeader str request
    in  route (Proxy :: Proxy sublayout) (feedTo subserver mheader)
    where str = fromString $ symbolVal (Proxy :: Proxy sym)

-- | When implementing the handler for a 'Post' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Put.Put', the handler code runs in the
-- @EitherT ServantErr IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we use the type-level list, combined
-- with the request's @Accept@ header, to encode the value for you
-- (returning a status code of 201). If there was no @Accept@ header or it
-- was @*\/\*@, we return encode using the first @Content-Type@ type on the
-- list.
instance
         {-# OVERLAPPABLE #-}
         ( AllCTRender ctypes a
         ) => HasServer (Post ctypes a) where

  type ServerT (Post ctypes a) m = m a

  route Proxy = methodRouter POST (Proxy :: Proxy ctypes) created201

instance
         {-# OVERLAPPING #-}
         HasServer (Post ctypes ()) where

  type ServerT (Post ctypes ()) m = m ()

  route Proxy = methodRouterEmpty POST

-- Add response headers
instance
         {-# OVERLAPPING #-}
         ( GetHeaders (Headers h v), AllCTRender ctypes v
         ) => HasServer (Post ctypes (Headers h v)) where

  type ServerT (Post ctypes (Headers h v)) m = m (Headers h v)

  route Proxy = methodRouterHeaders POST (Proxy :: Proxy ctypes) created201

-- | When implementing the handler for a 'Put' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Post.Post', the handler code runs in the
-- @EitherT ServantErr IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we use the type-level list, combined
-- with the request's @Accept@ header, to encode the value for you
-- (returning a status code of 200). If there was no @Accept@ header or it
-- was @*\/\*@, we return encode using the first @Content-Type@ type on the
-- list.
instance
         {-# OVERLAPPABLE #-}
         ( AllCTRender ctypes a) => HasServer (Put ctypes a) where

  type ServerT (Put ctypes a) m = m a

  route Proxy = methodRouter PUT (Proxy :: Proxy ctypes) ok200

instance
         {-# OVERLAPPING #-}
         HasServer (Put ctypes ()) where

  type ServerT (Put ctypes ()) m = m ()

  route Proxy = methodRouterEmpty PUT

-- Add response headers
instance
         {-# OVERLAPPING #-}
         ( GetHeaders (Headers h v), AllCTRender ctypes v
         ) => HasServer (Put ctypes (Headers h v)) where

  type ServerT (Put ctypes (Headers h v)) m = m (Headers h v)

  route Proxy = methodRouterHeaders PUT (Proxy :: Proxy ctypes) ok200

-- | When implementing the handler for a 'Patch' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Put.Put', the handler code runs in the
-- @EitherT ServantErr IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we just require that its type has
-- a 'ToJSON' instance and servant takes care of encoding it for you,
-- yielding status code 200 along the way.
instance
         {-# OVERLAPPABLE #-}
         ( AllCTRender ctypes a) => HasServer (Patch ctypes a) where

  type ServerT (Patch ctypes a) m = m a

  route Proxy = methodRouter PATCH (Proxy :: Proxy ctypes) ok200

instance
         {-# OVERLAPPING #-}
          HasServer (Patch ctypes ()) where

  type ServerT (Patch ctypes ()) m = m ()

  route Proxy = methodRouterEmpty PATCH

-- Add response headers
instance
         {-# OVERLAPPING #-}
         ( GetHeaders (Headers h v), AllCTRender ctypes v
         ) => HasServer (Patch ctypes (Headers h v)) where

  type ServerT (Patch ctypes (Headers h v)) m = m (Headers h v)

  route Proxy = methodRouterHeaders PATCH (Proxy :: Proxy ctypes) ok200

-- | If you use @'QueryParam' "author" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @'Maybe' 'Text'@.
--
-- This lets servant worry about looking it up in the query string
-- and turning it into a value of the type you specify, enclosed
-- in 'Maybe', because it may not be there and servant would then
-- hand you 'Nothing'.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: Maybe Text -> EitherT ServantErr IO [Book]
-- >         getBooksBy Nothing       = ...return all books...
-- >         getBooksBy (Just author) = ...return books by the given author...
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (QueryParam sym a :> sublayout) where

  type ServerT (QueryParam sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let p =
          case rqQueryParam paramname request of
            Nothing       -> Nothing -- param absent from the query string
            Just []       -> Nothing -- param present with no value -> Nothing
            Just (v:_)    -> fromText  (decodeUtf8 v)-- if present, we try to convert to
                                        -- the right type
    in route (Proxy :: Proxy sublayout) (feedTo subserver p)
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)

-- | If you use @'QueryParams' "authors" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @['Text']@.
--
-- This lets servant worry about looking up 0 or more values in the query string
-- associated to @authors@ and turning each of them into a value of
-- the type you specify.
--
-- You can control how the individual values are converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: [Text] -> EitherT ServantErr IO [Book]
-- >         getBooksBy authors = ...return all books by these authors...
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (QueryParams sym a :> sublayout) where

  type ServerT (QueryParams sym a :> sublayout) m =
    [a] -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy sublayout) (feedTo subserver (values request))
    where
      paramName     = cs $ symbolVal (Proxy :: Proxy sym)
      paramsBare r = concat $ rqQueryParam paramName r
      paramsBrak r = concat $ rqQueryParam (paramName <> "[]") r
      values     r = mapMaybe (fromText . decodeUtf8) $ paramsBare r <> paramsBrak r


-- | If you use @'QueryFlag' "published"@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type 'Bool'.
--
-- Example:
--
-- > type MyApi = "books" :> QueryFlag "published" :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooks
-- >   where getBooks :: Bool -> EitherT ServantErr IO [Book]
-- >         getBooks onlyPublished = ...return all books, or only the ones that are already published, depending on the argument...
instance (KnownSymbol sym, HasServer sublayout)
      => HasServer (QueryFlag sym :> sublayout) where

  type ServerT (QueryFlag sym :> sublayout) m =
    Bool -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let p = case rqQueryParam paramname request of
          Just []       -> True  -- param is there, with no value
          Just (v:_)    -> examine v -- param with a value
          Nothing       -> False -- param not in the query string
    in  route (Proxy :: Proxy sublayout) (feedTo subserver p)
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          examine v | v == "true" || v == "1" || v == "" = True
                    | otherwise = False

parseMatrixText :: B.ByteString -> QueryText --TODO: still depending on wai/network-types here. port to uri-bytestring
parseMatrixText = parseQueryText

-- | If you use @'MatrixParam' "author" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @'Maybe' 'Text'@.
--
-- This lets servant worry about looking it up in the query string
-- and turning it into a value of the type you specify, enclosed
-- in 'Maybe', because it may not be there and servant would then
-- hand you 'Nothing'.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> MatrixParam "author" Text :> Get [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: Maybe Text -> EitherT ServantErr IO [Book]
-- >         getBooksBy Nothing       = ...return all books...
-- >         getBooksBy (Just author) = ...return books by the given author...
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (MatrixParam sym a :> sublayout) where

  type ServerT (MatrixParam sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    case (\(a,b) -> a ++ b) $ pathSafeTail request of
      (first : _)
        -> do let querytext = parseMatrixText . encodeUtf8 $ T.tail (decodeUtf8 first)
                  param = case lookup paramname querytext of
                    Nothing       -> Nothing -- param absent from the query string
                    Just Nothing  -> Nothing -- param present with no value -> Nothing
                    Just (Just v) -> fromText v -- if present, we try to convert to
                                          -- the right type
              route (Proxy :: Proxy sublayout) (feedTo subserver param)
      _   -> route (Proxy :: Proxy sublayout) (feedTo subserver Nothing)

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)

-- | If you use @'MatrixParams' "authors" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @['Text']@.
--
-- This lets servant worry about looking up 0 or more values in the query string
-- associated to @authors@ and turning each of them into a value of
-- the type you specify.
--
-- You can control how the individual values are converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> MatrixParams "authors" Text :> Get [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: [Text] -> EitherT ServantErr IO [Book]
-- >         getBooksBy authors = ...return all books by these authors...
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (MatrixParams sym a :> sublayout) where

  type ServerT (MatrixParams sym a :> sublayout) m =
    [a] -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    case parsePathInfo request of
      (first : _)
        -> do let matrixtext = parseMatrixText . encodeUtf8 $ T.tail first
                  -- if sym is "foo", we look for matrix parameters
                  -- named "foo" or "foo[]" and call fromText on the
                  -- corresponding values
                  parameters = filter looksLikeParam matrixtext
                  values = catMaybes $ map (convert . snd) parameters
              route (Proxy :: Proxy sublayout) (feedTo subserver values)
      _ -> route (Proxy :: Proxy sublayout) (feedTo subserver [])

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          looksLikeParam (name, _) = name == paramname || name == (paramname <> "[]")
          convert Nothing = Nothing
          convert (Just v) = fromText v

-- | If you use @'MatrixFlag' "published"@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type 'Bool'.
--
-- Example:
--
-- > type MyApi = "books" :> MatrixFlag "published" :> Get [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooks
-- >   where getBooks :: Bool -> EitherT ServantErr IO [Book]
-- >         getBooks onlyPublished = ...return all books, or only the ones that are already published, depending on the argument...
instance (KnownSymbol sym, HasServer sublayout)
      => HasServer (MatrixFlag sym :> sublayout) where

  type ServerT (MatrixFlag sym :> sublayout) m =
    Bool -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    case parsePathInfo request of
      (first : _)
        -> do let matrixtext = parseMatrixText . encodeUtf8 $ T.tail first
                  param = case lookup paramname matrixtext of
                    Just Nothing  -> True  -- param is there, with no value
                    Just (Just v) -> examine v -- param with a value
                    Nothing       -> False -- param not in the query string

              route (Proxy :: Proxy sublayout) (feedTo subserver param)

      _ -> route (Proxy :: Proxy sublayout) (feedTo subserver False)

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          examine v | v == "true" || v == "1" || v == "" = True
                    | otherwise = False

-- | Just pass the request to the underlying application and serve its response.
--
-- Example:
--
-- > type MyApi = "images" :> Raw
-- >
-- > server :: Server MyApi
-- > server = serveDirectory "/var/www/images"
instance HasServer Raw where

  --type ServerT Raw m = Request -> (Response -> Snap Response) -> Snap Response
  type ServerT Raw m = m ()

  -- route :: Proxy layout -> IO (RouteResult (Server layout)) -> Router
  route Proxy rawApplication = LeafRouter $ \ request respond -> do
    r <- rawApplication
    case r of
      RR (Left err)  -> respond $ failWith err
      RR (Right app) -> undefined -- TODO app request (respond . succeedWith)

-- | If you use 'ReqBody' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'ReqBody'.
-- The @Content-Type@ header is inspected, and the list provided is used to
-- attempt deserialization. If the request does not have a @Content-Type@
-- header, it is treated as @application/octet-stream@.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
--
-- All it asks is for a 'FromJSON' instance.
--
-- Example:
--
-- > type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = postBook
-- >   where postBook :: Book -> EitherT ServantErr IO Book
-- >         postBook book = ...insert into your db...
instance ( AllCTUnrender list a, HasServer sublayout
         ) => HasServer (ReqBody list a :> sublayout) where

  type ServerT (ReqBody list a :> sublayout) m =
    a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy sublayout) $ do
      -- See HTTP RFC 2616, section 7.2.1
      -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
      -- See also "W3C Internet Media Type registration, consistency of use"
      -- http://www.w3.org/2001/tag/2002/0129-mime
      let contentTypeH = fromMaybe "application/octet-stream"
                       $ getHeader (mk "Content-Type") request
      rBody <- readRequestBody 1000000
      mrqbody <- handleCTypeH (Proxy :: Proxy list) (cs contentTypeH)
                 <$> return rBody -- lazyRequestBody request
      case mrqbody of
        Nothing -> return $ failWith $ UnsupportedMediaType
        Just (Left e) -> return $ failWith $ InvalidBody e
        Just (Right v) -> feedTo subserver v

-- | Make sure the incoming request starts with @"/path"@, strip it and
-- pass the rest of the request path to @sublayout@.
instance (KnownSymbol path, HasServer sublayout) => HasServer (path :> sublayout) where

  type ServerT (path :> sublayout) m = ServerT sublayout m

  route Proxy subserver = StaticRouter $
    M.singleton (cs (symbolVal proxyPath))
                (route (Proxy :: Proxy sublayout) subserver)
    where proxyPath = Proxy :: Proxy path

ct_wildcard :: B.ByteString
ct_wildcard = "*" <> "/" <> "*" -- Because CPP
