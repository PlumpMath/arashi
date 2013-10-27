# lookalike

Provides a simple API for fetching favicons of websites.

## HTTP API

* `/favicon?url=<url>`: Redirects to the favicon of the given url

    Usable in `<img>` tags:

        <img src="/favicon?url=http://paulgraham.com">
* `/favicon_url?url=<url>`: Returns the url of the favicon of the given url
* `/cache`: Inspect the internal cache.

## Clojure API

* `lookalike.core`: Provides `favicon-of` and `save-favicon-of`, where the
  "safe" version doesn't throw exceptions.
* `lookalike.cache`: Provides a cached version of `save-favicon-of`, using
  a LRU cache that can contain up to 1000 entries (arbitrarily chosen for
  now, might change later).
* `lookalike.server`: Can be used for development using `lein ring server`.
  To embed `lookalike` into your application use something like the following:

    (defroutes my-awesome-routes
      ; rockin' in here
      (context "/lookalike" []
               lookalike.server/app-routes))

## Problems

* `save-favicon-of` should probably not return `<root-url>/favicon.ico` but
  return nothing
* might be massively memory-inefficient for several reasons:
    - spinning a JVM for this little thing
        * rewriting in Haskell would be both fun and leaner
    - storing favicon by URL, not by host
        * not doing so would be "wrong" (e.g. for things that have favicons in
            subdirectories)
        * you can pass `scheme://hostname` to `favicon-of` if you think that's
            ok, but it will break for several things (urls that redirect,
            for example urls from feedburner)