# Fuzzy Dict #

A simple web-based application for myself to learn English vocabulary.

It can search words by matching:

1. exact match
2. prefix
2. suffix
3. wildcard
4. maximum edit distance

and use various index to boost the search speed.

Written in Scala using Scalatra web framework.

## Build & Run ##

```sh
$ cd Fuzzy_Dict
$ ./sbt
> container:start
> browse
```

If `browse` doesn't launch your browser, manually open [http://localhost:8080/](http://localhost:8080/) in your browser.
