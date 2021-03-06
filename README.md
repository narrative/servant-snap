# servant-snap

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

Snaplet-capable [snap](http://www.snapframework.com) server for [servant](http://github.com/haskell-servant) APIs.

This library lets you *implement* an HTTP server with handlers for each endpoint of a servant API, handling most of the boilerplate for you.

## Building

We target the soon-to-be-released [snap-1.0](http://github.com/snapframework/snap) and a WIP branch of [servant-0.5](http://github.com/codedmart/servant). To get all the necessary dependencies, clone the servant-snap repository, and within that directory, run

```bash
git submodule update --init --recursive
./init-sandbox.sh
```

Then, build the library and example server:

```
cabal install --only-dep
cabal build
dist/build/greet/greet
curl localhost:8001/api/hello/DearUser
```

## Getting started

We've written a [Getting Started](http://haskell-servant.github.io/getting-started/) guide that introduces the core types and features of servant. After this article, you should be able to write your first servant webservices, learning the rest from the haddocks' examples.

## Repositories and Haddocks

- The core [servant](http://github.com/haskell-servant) package - [docs](http://hackage.haskell.org/package/servant)
- Implementing an HTTP server for a webservice API with [servant-server](http://github.com/haskell-servant/servant/tree/master/servant-server) - [docs](http://hackage.haskell.org/package/servant-server)
- (Haskell) client-side function generation with [servant-client](http://github.com/haskell-servant/servant/tree/master/servant-client) - [docs](http://hackage.haskell.org/package/servant-client)
- (Javascript) client-side function generation with [servant-jquery](http://github.com/haskell-servant/servant/tree/master/servant-jquery) - [docs](http://hackage.haskell.org/package/servant-jquery)
- API docs generation with [servant-docs](http://github.com/haskell-servant/servant/tree/master/servant-docs) - [docs](http://hackage.haskell.org/package/servant-docs)
