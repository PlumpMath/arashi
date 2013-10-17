# arashi - Helping you survive the internet

Aggregates posts from a number of sources and displays them in
reverse chronological order. Very non-finished, which is good
because it only displays links and bad because everything fancy
is not done.

## What does it do?

Fetch posts from a number of sources (all of which are kind of
experimental):

* ATOM/RSS feeds
* Twitter pages (e.g. everything that looks like a [user page][twitter]
    should work)
* [HackerNews](https://news.ycombinator.com)
* [Paul Graham's essays](http://paulgraham.com/articles.html)

Fetches less frequently if the source doesn't change.

## What does it want to do?

Give you (me) more time to spend creating things, not checking all the
things you could check. You check one thing, and that's it.

What's missing: Some incentive to spend less time on this giant list.
This could be rate-limiting or something more interesting like
"ignoring" links and filters.

You could also put all your sources in here and then check back every
few days without worrying of missing something.

Or you could take the sane route and not worry about "missing something"
but rather creating something.

## What could it do?

* track which links you clicked
* let you remove posts from the feed (either to remove non-interesting
    posts or as kind-of todo list)
* let you favourite notes (possibly via pinboard to be able to tag
    them and add notes to them?)
* filter links (by source, by title matching, by date range)
* be extensible via ClojureScript (e.g. custom filters)
* detect "trends" using fancy algorithms
* persist entries to a database (e.g. datomic)

## How to use it?

1. grab the [latest released version][latest]
2. run it!

        $ java -jar arashi-v0.0-alpha.jar

If you want to use different feeds than the default ones, put a
file name `config.edn` into the same directory you start `arashi`
from (here's [the one i use][config]).

## How to help

* use it & tell me when it breaks (preferably also telling me what
    broke it or even fixing it)
* have a look at the code (and tell me what you think)

## Similar things

* feed readers (i think [stringer][] is good to host yourself)
* twitter, hn
* [murlsh](https://github.com/mmb/murlsh)

## Acknowledgements

* Clojure (inspiration; immutability, refs/agents/stm)
* [HN](https://news.ycombinator) for the "list of links" interface
    and often inspiring/interesting posts
* [Andy Baio](http://waxy.org) and [Christian Neukirchen](http://chneukirchen.org)
    for their respective [link][waxy-links] [blogs][trivium]
* [rome][] & [feedparser-clj][] for ATOM/RSS parsing

Thank you, internet. For ideas, frustrations and standards/spirit that
make things such as this possible.

[twitter]: https://twitter.com/bonus500
[latest]: https://github.com/heyLu/arashi/releases/download/v0.0-alpha/arashi-0.0-SNAPSHOT-standalone.jar
[config]: https://github.com/heyLu/arashi/blob/master/config.edn
[stringer]: https://github.com/swanson/stringer
[waxy-links]: http://waxy.org/links
[trivium]: http://chneukirchen.org/trivium
[rome]: https://github.com/rometools/rome
[feedparser-clj]: https://github.com/scsibug/feedparser-clj
