# ednsl

A super-simple library for creating DSLs with EDN syntax

## Smarter configuration

edn is great, it allows rich configuration, supporting more datatypes than
things like json, but up until now we've not been able to get the best out
of it. I wrote a library called [oolong](https://github.com/jjl/oolong/) to
experiment with wiring applications together in edn syntax and while I was
happy with the flexibility this enabled, I wasn't happy with the code so I
started to refactor it and somewhere along the line I found I was writing
a DSL for DSLs. Such is life :-)

## What can I do with it?

Not that I'm necessarily endorsing all of these, but...

* Automatically replace symbols with their values in clojure-space
* Provide for users to call select functions within your config file
* Provide a clojure-like language with simplified evaluation rules for less
  technical users of your application

I'd love to hear what you come up with, drop me a github message!

## Walkthrough

This is a simple example of an application which reads a config file which
provides a function to run along with arguments. No, it's not meant to be
useful, it's meant to provide a simple example:

```clojure
(ns my.app
  (:use [ednsl]))
(defn main [& args]
  (let [r (-> "config-file.edn" read-file ctx-form
           (>>= emap (eithery (etuple ekey (eor (>>- esym eload-sym) eany)))))
        {:keys [main args]} r]
   (apply main args)))
(defn real-main [& args]
 ;; Do something useful!
 (prn args))
```

And here's a config file for it:

```edn
{:main my.app/real-main
 :args [:something :something-else]}
```

# Documentation

Codox docs are in the 'doc' directory or can be read on
[github](https://github.com/jjl/). They contain a good walkthrough.

# Copyright and License

Copyright (c) 2015 James Laver

Distributed under the MIT License (see LICENSE file in this repository).

