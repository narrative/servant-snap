{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           GHC.Generics
--import Network.Wai
--import Network.Wai.Handler.Warp
import           Servant.Server.Internal.SnapShims
import           Snap.Core
import           Snap.Http.Server

import           Servant

import           Debug.Trace

import Heist
import Snap.Snaplet.Heist
import Snap.Snaplet
-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

data App = App { _heist :: Snaplet (Heist App)}
makeLenses ''App

app :: SnapletInit App App
app = makeSnaplet "app" "example" Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  return $ App h

-- API specification (pretend this is in a different package)
type TestApi =

       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
  "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] ()
  -- :<|> Get '[JSON] Greet

  -- :<|> "justreq" :> Get '[JSON] ()

testApi :: Proxy (TestApi)
testApi = Proxy


--instance HasHeist App where
--  heistLens = subSnaplet heist

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT ServantErr IO' monad.
server :: Server TestApi (Handler App App)
server = helloH :<|> postGreetH :<|> deleteGreetH

  where helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        --helloH name (Just False) = writeBS ("Hello, " <> name)
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        --postGreetH :: Greet -> Server App Greet
        postGreetH greet = return greet

        deleteGreetH _ = return ()
        -- nodeal = return $ Greet "NoDeal"
        --justReq = cs $ mconcat (pathInfo req)


-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Handler App App ()
test = serve testApi server

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: Int -> IO ()
runTestServer port = do
  (_,s,_) <- runSnaplet Nothing app
  simpleHttpServe (setPort port mempty :: Config Snap ())
                     (applicationToSnap s)

-- Put this all to work!
main :: IO ()
main = runTestServer 8001
