# Several modules only needed to have their Wai `Request` replaced by snap-core's `Request`

# Where Wai `Application` shows up, replace by MonadSnap m => m ()

# `pathInfo` doesn't exist in Snap world, but it's easy to write

# I dropped a lot of CPP flags... to appease ghc-mod. May have to bring them back!

# First major hang-up is in Servant.Server.Internal.RoutingApplication

 - Memoization trick works now over Wai's kind of implicit ByteString fragment generator thing.
 - snap-core uses a bona-fide `Enumerator` pre-1.0, and io-streams `InputStream` post-1.0
 - snap 1.0 isn't yet released, but mostly works and is on github
 - snap-core doesn't expose the Request constructor, for safety. No easy way to muck wit the internal InputSource
 - Right now, I'm simply going to patch snap-core to export (rqBody :: Request -> InputStream ByteString), so we can play w/ the stream.
 - Waiting to hear back from gcollins on alternatives
 - May be even better to consider ways of elimininating the multiple-body-read. Is it possible? Even when we end in servant's `Raw` route?

 - UPDATE:
 - You can mess with the Enumerator/InputStream simply by importing a .Internal from snap-core
 - [Example](https://github.com/snapframework/snap-server/blob/master/src/Snap/Internal/Http/Server/Session.hs#L423): snap-server plays with the stream when processing forms that ar x-www-form-encoded
 - We can just read the first N bytes into a bytestring from the iostream, pass it through the routing machinery, and build a new iostream to put back into the request for use by the handler

# Wai and Snap differ in how they store request path. Wai just has a list of path parts. Snap-core has a 'rqPathContext' and a 'rqPathInfo', each is a bytestring with '/'s inside delimiting components. Patched over this in `Servant.Server.Internal.PathInfo` with the functions `pathInfo` and `pathSafeTail`

# Hmm. Snap doesn't seem to parse matrix params.. troublesome for Internal.hs. Will continue to depend on Wai for `parseQueryText` until we can replace it in snap-core or servant-server

# Look at uri-bytestring. Once this gets matrix param supporte, we can use it instead of the Network.URI parser

# Application <-> Snap ()

I believe I need a function `snapServeApplication :: (Request -> (Respsonse -> IO Response) -> IO Response) -> Snap Response`
This is proving to be a pain to write. The arguments to the `Application` function aren't easy to get at. I need to lift the callback in the second argument into the Snap monad.

# Long convo with Alp and Andres

Discovered one of the main goals of Servant's design: make all a handler's dependencies explicit in the types. Makes a lot of sense in retrospect, but hadn't realized this, or the implications for handling in Snap's Handler monad, until now. Oops!

We want to forbid looking up info from the request in the handler. All that looking up must be done in servant's internals. Enforce if possible.

Another goal - have greet.hs compile without changing the handlers or their type signatures (so handling is done in Servant's Server monad).

We might continue to use MonadSnap m in the routing, but MonadSnaplet m in the interface. Or our own MonadServantSnaplet which denies access to the underlying MonadSnap monad, so that users can't get at raw request info. Created the branch `monadsnaplet` for trying to get some of this working.

# Can we convert `Handler b b a]` to `EitherT ServantErr IO a`?

I don't think so. Even to `runSnaplet` on the handler, we need access to its initializer. This seems wrong...

# Replace `EitherT ServantErr IO a` with `Handler b b a`?

Maybe, but this would require us to pass in `b` as a type parameter. All previously written handlers would break. Asside from that issue, this seems like kind of a promising way to have servant's routing type be snaplet-aware.

# `Handlers` branch

The point of this branch is to replace both `RoutingApplication` and `Application` by `Handler b b ()`
