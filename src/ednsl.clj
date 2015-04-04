(ns ednsl
  (:require [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as rt]
            [cats.core :as m]
            [cats.monad.either :as e]
            [collectible :as c]
            [ednsl.util.either :as ee]))

;; # ednsl
;; 
;; A super-simple library for creating DSLs with EDN syntax

(declare eor) ; Move declaration order for didactic purposes

;; ## Smarter configuration
;; 
;; edn is great, it allows rich configuration, supporting more datatypes than
;; things like json, but up until now we've not been able to get the best out
;; of it. I wrote a library called [oolong](https://github.com/jjl/oolong/) to
;; experiment with wiring applications together in edn syntax and while I was
;; happy with the flexibility this enabled, I wasn't happy with the code so I
;; started to refactor it and somewhere along the line I found I was writing
;; a DSL for DSLs. Such is life :-)
;;
;; ## What can I do with it?
;;
;; Not that I'm necessarily endorsing all of these, but...
;;
;; * Automatically replace symbols with their values in clojure-space
;; * Provide for users to call select functions within your config file
;; * Provide a clojure-like language with simplified evaluation rules for less
;;   technical users of your application
;;
;; I'd love to hear what you come up with, drop me a github message!
;;
;; ## Walkthrough
;;
;; This is a simple example of an application which reads a config file which
;; provides a function to run along with arguments. No, it's not meant to be
;; useful, it's meant to provide a simple example:
;;
;; ```clojure
;; (ns my.app
;;   (:use [ednsl]))
;; (defn main [& args]
;;   (let [r (-> "config-file.edn" read-file ctx-form
;;            (>>= emap (eithery (etuple ekey (eor (>>- esym eload-sym) eany)))))
;;         {:keys [main args]} r]
;;    (apply main args)))
;; (defn real-main [& args]
;;  ;; Do something useful!
;;  (prn args))
;; ```
;;
;; And here's a config file for it:
;;
;; ```edn
;; {:main my.app/real-main
;;  :args [:something :something-else]}
;; ```
;;
;; Let's take the function apart a step at a time
;; 
;; ```clojure
;; (-> "config-file.edn" read-file ctx-form)
;; ```
;; This firstly calls `(read-file "config-file.edn"), which reads that file and
;; returns the data inside as regular clojure data. Then it calls `ctx-form` on
;; the result, which returns a context form, the datastructure that threads
;; through our DSL. We'll get onto those in a minute, but let's take the next
;; line apart first. I've inserted `result` to replace the threaded result
;; from the last line.
;;
;; ```clojure
;; (>>= result emap (eithery (etuple ekey (eor (>>- esym eload-sym) eany)))))
;; ```
;;
;; If you've programmed haskell before, you might recognise the monadic bind.
;; We use the `Either` datatype to represent either success or failure. Progress
;; flows from left to right, like with `->`. So let's take it apart.
;;
;; The first check is emap, which requires a map. Since our config files is a
;; map, we pass and the same map passes through to the next check:
;; 
;; ```clojure
;; (eithery (etuple ekey (eor (>>- esym eload-sym) eany)))
;; ```
;;
;; These are actually quite simple pieces that fit together. Let's track what it
;; does alongside the config file, which I'll reprint for convenience:
;;
;; ```edn
;; {:main my.app/real-main
;;  :args [:something :something-else]}
;; ```
;;
;; `eithery` takes a check which will operate over each item in a collection.
;; For our map, this means each item is a MapEntry. The check we've chosen is:
;;
;; ```clojure
;; (etuple ekey (eor (>>- esym eload-sym) eany))
;; ```
;; 
;; `etuple` takes any number of items and compares them to the provided
;; collection. In our case, the collection has two items, a key and a value.
;; The first checker will be applied to the first item and the second to the
;; second item. Our first checker is `ekey`, which matches a keyword. Here's
;; the second, and the code is getting much smaller now:

;; ```clojure
;; (eor (>>- esym eload-sym) eany)
;; ```
;;
;; `eor` takes any number of checks and returns one check that matches if any
;; of the provided checks matches. Skipping over the more complex first check
;; for a second, `eany` matches anything. We're using this as a fallback in case
;; the more complex check fails, it's basically the identity. Let's take on that
;; last check now:
;;
;; ```clojure
;; (>>- esym eload-sym)
;; ```
;;
;; `>>-` combines a checker pipeline into a check. First we use `esym` which
;; matches a symbol and then `eload-sym` which loads that symbol and returns
;; the value inside. Finally, let's make it useful:
;; 
;; ```clojure
;; (let [{:keys [main args]} r]
;;    (apply main args))
;; ```
;;
;; We simply look up the `:main` and `:args` keys in config and run the main
;; function with the arguments provided in the config.
;;
;; ## Context
;;
;; All checks take a single argument, a CtxForm. It contains a single key by
;; default: `form`. This represents the piece of data from the config file
;; currently being worked on. You can add custom keys as you like to maintain
;; custom context

(defrecord CtxForm [form])

;; We most commonly refer to these as 'ctx-form' after the function we use to
;; create them.

(defn ctx-form
  "Constructs a context form. If arg is a map, expect a :form argument
   else assume it is a form. Be careful with map data.
   Args: [Any]
   Returns: ctx-form"
  [arg]
  (map->CtxForm 
   (if (and (map? arg) (contains? arg :form))
     arg
     {:form arg})))

;; We provide an accessor for the form

(def cf-form
  "Returns the form from a given context form or else a default (default: nil)
   Args: [ctx-form default?]
   Returns: "
  :form)

;; And some utilities for working with the ctx-form. Note that two of these are
;; considered to be part of the implementation and not part of the public API,
;; hence the '*' suffix, but we don't make them private because they may be useful.

(defn update-form*
  "Given a function f and a ctx-form cf, applies the form inside cf and returns
   a new result with the form mutated to the return value of (f form) 
   Args: [f ctx-form]
   Returns: ctx-form"
  [f cf]
  (update-in cf [:form] f))

(defn replace-form*
  "Given a new value and a ctx-form, replaces the form and returns a new ctx-form
   Args: [new ctx-form]
   Returns: ctx-form"
  [new cf]
  (update-form* (constantly new) cf))

(declare right)

(defn add-context
  "Given a key and a value, returns a function which adds the context to the
   context form it is given and wraps it in a Right.
   Args: [k v]
   Returns: Function:
       Args: [ctx-form]
       Returns: right[ctx-form]"
  [k v]
  #(right (assoc-in % [k] v)))

;; ## Either
;;
;; Checks are expected to take a ctx-form and return an Either, a type with two
;; possible values: Left and Right. We provide creation functions in the form of
;; the `left` and `right` functions. They each take a value which will be contained
;; in the resulting Either. Right represents success and should contain the result
;; of said success. Left represents failure and should contain some information
;; about the failure.

(def left
  "Creates a Left wrapping the provided value.
   Args: [Any]
   Returns: Left[Any]"
  e/left)

(def right
  "Creates a Right wrapping the provided value
   Args: [Any]
   Returns: Right[Any]"
  e/right)

;; To work with them at a low level, we can use the `left?` and `right` predicates

(def left?  "Is this a left? Any -> Bool."  (every-pred e/either? e/left?))
(def right? "Is this a right? Any -> Bool." (every-pred e/either? e/right?))

;; We can also use some macros to conditionally execute code. These are here mostly
;; to provide additional flexibility required to improve performance of the rest
;; of the library. These are slightly naughty because they don't require you to
;; pass an Either, which cleans up some other functions a lot.
;; Again, note the '*' for 'not really public'.

(defmacro when-left*
  "Given a value and a list of expressions, either runs the expressions if the
   provided value is a Left or else returns the other value unmodified
   Args: [e & exprs]
   Returns: result of exprs or value
   Warning: Macro!"
  [e & exprs]
  `(let [e# ~e]
     (if (left? e#)
       (do ~@exprs)
       e#)))

(defmacro when-not-left*
  "Given an value and a list of expressions, either runs the expressions if the
   provided value is not a Left or else return the Left unmodified
   Args: [e & exprs]
   Returns: result of exprs or value
   Warning: Macro!"
  [e & exprs]
  `(let [e# ~e]
     (if (left? e#)
       e# 
       (do ~@exprs))))

(defmacro when-right*
  "Given a value and a list of expressions, either runs the expressions if the
   provided value is a Right or else returns the other value unmodified
   Args: [e & exprs]
   Returns: result of exprs or value
   Warning: Macro!"
  [e & exprs]
  `(let [e# ~e]
     (if (right? e#)
       (do ~@exprs)
       e#)))

(defmacro when-not-right*
  "Given a value and a list of expressions, either runs the expressions if the
   provided value is not a Right or else returns the other value unmodified
   Args: [e & exprs]
   Returns: result of exprs or value
   Warning: Macro!"
  [e & exprs]
  `(let [e# ~e]
     (if (right? e#)
       e# 
       (do ~@exprs))))

;; We also wrap these in some public functions which simplifies composition

(defn when-left
  "Given a function, returns a function which executes with the provided either
   only when it is Left or else returns the value unchanged
   NOTE: you cannot bind into this, because bind will never reach it
   Correct example: `((when-left (>>- my monadic chain)) (left nil))
   Args: [f]
   Returns: Function:
      Args: [Any]
      Return: Right, or return-typeof(f)"
  [f]
  #(when-left* % (f m/extract %)))

(defn when-right
  "Given a function, returns a function which executes with the provided value
   only when it is Right or else returns the value unchanged
   NOTE: you cannot bind into this, because bind will never reach it
   Correct example: `((when-right (>>- my monadic chain)) (left nil))
   Args: [f]
   Returns: Function:
      Args: [Any]
      Return: Left, or return-typeof(f)"
  [f]
  #(when-right* % (f m/extract %)))

;; Then we have some basic building blocks for working with Either values

(defn fail
  "Fails because some data was invalid. If extra-data-map is provided,
   merges the maps together to provide failure info.
   Args: [cf] [extra-data-map cf]
   Returns: Right"
  ([cf]
     (fail {} cf))
  ([extra cf]
     (e/left (into cf extra))))

(defn invert
  "Wraps the provided function in a function which inverts the either result
   provided, that is if it returns a left, return a right and vice versa.
   Eg: `(>>= (invert #(left nil)) handle-error)`"
  [f]
  #(ee/invert (f %)))

(defn branch
  "Given two functions, returns a function which executes one depending on
   whether the value passed to it is a Left or a Right and returns the result
   Provided functions are expected to take the value in the monad and are
   easily chained by returning an Either
   Args: [l-f r-f]
   Returns: Function:
       Args: [Either]
       Returns: Any"
  [l-f r-f]
  (partial ee/branch l-f r-f))

;; And some functions for converting ordinary predicates into checks

(defn ctx-pred
  "Returns a function which applies the predicate to the incoming data and
   if truthy, returns the data, else fails with exp-desc
   Args: [p exp-desc ctx-form]
   Returns: Either[ctx-form]"
  [p exp-desc cf]
  (let [form (cf-form cf)]
    (if (p form)
      (e/right cf)
        (fail {:expected exp-desc} cf))))

(defn epred
  "Given a predicate function, Returns a function in the Either monad based
   on whether the predicate returns a truthy value
   Args: [p exp]
     p: predicate. Ordinary clojure function whose return should be boolish
     exp: generally a string, but anything descriptive (e.g. map), for ex-info
   Returns: Function:
       Args: [ctx-form]
       Returns: Either[ctx-form]"
  [p exp-desc]
  (partial ctx-pred p exp-desc))

;; We can combine multiple checkers

(defn eor
  "Given some epreds, returns a function which returns the first Right
   respon se or else a Left
   Args: [& epreds]
   Returns: Function:
       Args: [ctx-form]
       Returns: Either[ctx-form]"
  [desc & epreds]
  {:pre [#(-> epreds count pos?)]}
  (fn [cf]
    (let [rs (map #(% cf) epreds)]
      (or (ee/first-right rs)
          (fail {:expected desc} cf)))))

(def >>=
  "Pronounced 'bind'. If the provided either is a Right, apply the given check
   and recur along the rest of the chain, substituting the result of that check
   for the original Either
   Args: [either check & [checks]]
   Returns: either"
  m/>>=)
(def >>- ee/>>-)

;; We can also work on collections

(defn eithery*
  "Takes a function and a collection and returns either the first Left result
   of calling the f with an element of the collection or a new collection
   made from the results in a Right. This means you can mutate in-transit.
   Args: [f coll ctx-form]
   Returns: Either[ctx-form]"
   [f cf]
   (let [form (cf-form cf)
         rs (reduce (fn [acc v]
                      (let [r (f (replace-form* v cf))]
                        (if (e/left? r)
                          (reduced r)
                          (conj acc (-> r m/extract cf-form)))))
                    [] form)]
     (if (e/either? rs)
       rs
       (e/right (replace-form* (c/into-coll form rs) cf)))))

(defn eithery
  "Returns a function that asserts that all the contents of the given form
   in the ctx-form match a given epred (which implies the form is a coll)
   That function returns a Right wrapping a ctx-form constructed by reusing
   the context and replacing the form with a new form constructed by turning
   the results into a new collection
   We try to do the right thing, so play around in the repl 
   The new ctx-form 
   Args: [epred]
   Returns: Function:
       Args: [ctx-form]
       Returns:Either[ctx-form]"
  [epred]
  (partial eithery* epred))

(defn collect
  "Given a collection, returns a function which takes a collection of Eithers in a
   ctx-form and casts it to the type of the original collection, folding the Eithers
   in the collection into a single Either wrapping the cast collection
   Args: [coll]
   Returns: Function:
       Args: [cf]
       Returns: Either[ctx-form[typeof(coll)]]"
  [coll]
  (fn [cf]
    (let [rs (cf-form cf)]
      (or (ee/first-left rs)
          (e/right (update-form* (constantly (c/into-coll coll (map m/extract rs))) cf))))))

;; ## Simple Checkers
;;
;; All of them take a ctx-form and return an Either[ctx-form]

(def eany
  "Matches anything.
   Args: [ctx-form]
   Returns: Right[ctx-form]"
  right)

(def enil
  "Expects nil
   Args: [ctx-form]
   Returns: Either[ctx-form]"
  (epred nil? "nil"))

(def etrue
  "Expects true
   Args: [ctx-form]
   Returns: Either[ctx-form]"
  (epred true? "true"))

(def efalse
  "Expects false
   Args: [ctx-form]
   Returns: Either[ctx-form]"
  (epred false? "false"))

(def ebool
  "Expects false
   Args: [ctx-form]
   Returns: Either[ctx-form]"
  (eor "bool" etrue efalse))

(def eint
  "Expects an integer
   Args: [ctx-form]
   Returns: Either[ctx-form]"
  (epred integer? "integer"))

(def estr
  "Expects a string
   Args: [ctx-form]
   Returns: Either[ctx-form]"
  (epred string? "string"))

(def ekey
  "Expects a keyword
   Args: [ctx-form]
   Returns: Either[ctx-form]"
  (epred keyword? "keyword"))

(def esym
  "Expects a symbol
   Args: [ctx-form]
   Returns: Either[ctx-form]"
  (epred symbol? "symbol"))

(def efn
  "Expects a function. Useful for e.g. evaluating lists
   Args: [ctx-form]
   Returns: Either[ctx-form]"
  (epred fn? "function"))

(def elist
  "Expects a list
   Args: [ctx-form]
   Returns: Either[ctx-form]"
  (epred list? "list"))

(def evec
  "Expects a vector
   Args: [ctx-form]
   Returns: Either[ctx-form]"
  (epred vector? "vector"))

(def emap
  "Expects a map
   Args: [ctx-form]
   Returns: Either[ctx-form]"
  (epred map? "map"))

;; ## More involved checkers

(defn ecount
  "Expects that the count of the form is either cnt or between low and high
   Args: [cnt] [low high]
   Returns: Either[ctx-form]"
  ([cnt]
     (epred (partial c/count= cnt) (str "collection of length " cnt)))
  ([low high]
     (epred (partial c/count<= low high)
            (str "collection of length between " low " and " high " (inc.)"))))

(defn etuple
  "Given some preds, returns a function that expects a vector and zips the
   epreds and the vector together returning either the first Left or else
   a new right with a vector made up of the contents of the rights
   Args: [& preds]
   Returns: Function:
       Args: [ctx-form]
       Returns: Either[ctx-form]"
  [& epreds]
  (fn [{:keys [form] :as cf}]
    (let [rs (map #(% (replace-form* %2 cf)) epreds form)]
      (if-let [l (ee/first-left rs)]
        l
        (-> (c/into-coll form (map (comp cf-form m/extract) rs))
            (replace-form* cf) right)))))

(defn etuple-chain
  "Given some preds, returns a function that expects a vector and zips the
   epreds and the vector together in a monadic chain (context propagates along
   the chain so earlier functions can affect the context for later functions.
   Returns either the first Left in the chain or a new right with a vector
   made up of the contents of the rights
   Args: [& preds]
   Returns: Function:
       Args: [ctx-form]
       Returns: Either"
  [& epreds]
  (fn [{:keys [form] :as cf}]
    (loop [prev (right cf)  acc []
           [e & es] epreds  [f & fs] form]
      (when-right* prev
        (let [r (->> prev m/extract (replace-form* f) e)
              new-acc (->> r m/extract cf-form (conj acc))]
          (when-right* r
            (if (every? seq [es fs])
              (recur r new-acc es fs)
              (right (replace-form* new-acc cf)))))))))

(defn ekeys
  "Returns a function which expects a map where all keys match a given epred
   Args: [epred]
   Returns: Function:
       Args: [ctx-form]
       Returns: Either[ctx-form]"
  [epred]
  (eithery (etuple epred right)))

(defn evals
  "Returns a function which expects a map where all vals match a given epred
   Args: [epred]
   Returns: Function:
       Args: [ctx-form]
       Returns: Either[ctx-form]"
  [epred]
  (eithery (etuple right epred)))

;; ## Utilities

(defn eload-sym
  "Takes a context form which is expected to be a fully qualified symbol.
   Namespace will be required and the symbol will be resolved if it exists
   On success, updates the ctx-form with the value of the symbol and wraps in
   a Right, else returns a Left.
   Args: [cf]
   Returns: Either[ctx-form]"
  [{:keys [form] :as cf}]
  (or (try (-> form namespace symbol require)
           (-> @(find-var form) (replace-form* cf) right)
           (catch Exception e nil))
      (fail {:expected "loadable symbol"} cf)))

(defn read-file
  "Reads file at path as edn
   Args: [path]
   Returns: clojure form"
  [path]
  (-> path slurp rt/indexing-push-back-reader edn/read))

(defn read-str
  "Reads the given string as edn
   Args: [str]
   Returns: clojure form"
  [str]
  (-> str rt/indexing-push-back-reader edn/read))

