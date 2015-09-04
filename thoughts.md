* what about haskell
    - seems quite cool
    - strings are still weird though
        * would using Text help? (feed uses strings for everything, but
          we'd only need that for new things.)
    - speaking of which (new things), maybe we don't have to parse
        all feed entries, just the new ones? e.g. if the posts have a
        uuid field and we've already seen that then we could just skip
        that. this might also make identity/eq/ord a bit easier.
    * the post set should be both time and url sorted, but it should
      still not add posts twice. (uuid, time+url, url? not sure which is
      best.)
* t'is very slow, let's try clojurescript
    - well, try not sending `2mb` each time first
    - [node-feedparser](https://github.com/danmactough/node-feedparser)
        might help if we actually do this
    - not quite happy that the choice is leaving clojure or using
        the js ecosystem. would love to do this in haskell, but
        failed last time. (string representations, http hassle,
        parsing nightmare.) theory: haskell is best for transformations
        of "known" data, less so for kind-of unstructured data.
