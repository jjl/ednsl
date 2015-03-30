(ns ednsl
  (:require [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as rt]
            [cats.core :as m :refer [>>=]]
            [cats.monad.either :as e]
            [collectible :as c]
            [ednsl.util.either :as ee]
            [ednsl.util :as u]))

;; # ednsl
;; 
;; A super-simple library for creating DSLs with EDN syntax
;; 
;; ## Quickstart
;; 
;; ## Basics
;; 
;; Let's get some definitions out of the way:
;; 
;; Either
;; : A protocol implemented by two records, Left and Right
;; 
;; Left
;; : A record which indicates failure of a process. Contains useful info
;; 
;; Right
;; : A record which indicates success. Contains a result
;; 
;; ctx-form
;; : A 'context form', contains a context object and a form. Currently
;;   implemented with clj-tuple, but don't rely on this, use the `cf-ctx`
;;   and `cf-form` accessor functions
;; 
;; epred
;; : An expecter function. Takes a ctx-form and returns an Either[ctx-form]
;; 
;; ## Either Quickstart
;; 
;; We've stolen the 'Either' datastructure from Haskell and implemented
;; it in clojure. Put simply, you use the `left` and `right` functions
;; to create one, with Left symbolising failure and Right symbolising
;; success. We'll walk you through using these first.
;; 
;; We can create a Right with the 'right' function, passing as an argument
;; the item we wish to be wrapped. Similarly for the 'left' function
;; 
;; ```clojure
;; (right :foo) ; represents a successful match of `:foo`
;; (left :foo)  ; represents a failing match against `:foo`
;; ```
;; 
;; Since we intend to behave differently dependening on whether we are
;; working with a left or a right (failure or success), we need a way to
;; switch behaviour based on the value. The easiest way is `branch`, which
;; takes two functions. If the value is a left, the left function is
;; called and if it is a right, the right function is called.
;;
;; ```clojure
;; (def either-to-bool (branch (constantly false) (constantly true)))
;; ```
;;
;; A couple of things to note:
;; - The return type is a function
;; - The arguments are both functions
;; 
;; This is a little convoluted, but hopefully in the next section, you'll
;; even notice what's underpinning 

;; We can use `>>=` (pronounced 'bind') to chain a function to the Either
;; What this means is that if the Either is a left, the left will be returned
;; but if it is a right, the function will be executed, passing in e


;; since a successful parse result is anything other than a failure token,
;; the simplest spectre that always works is 'constantly':
;;
;; ```(constantly 4) ; match anything, always return 4```
;;
;; ednsl includes some simple spectres that assert their arguments are of
;; a given type (eg. `eint` succeeds only when given an integer)
;;
;; `eint` also has an optional second argument, a chained spectre that will
;; be called with the input if the first spectre succeeds.
;; For example, here's how we could check f is a positive integer:
;;
;; ```(eint f #(if (> 0 %) % (fail % "positive integer"))
;;
;; This mechanism allows us to perform automatic type conversion, further
;; validation and allows for easy composition of spectres

;; ## Convenience functions

(def ctx-form u/ctx-form)

(def left e/left)
(def right e/right)

(defn branch
  "Given two functions, returns a function which executes one depending on
   whether the value passed to it is a left or a right and returns the result
   Provided functions are expected to take the value in the monad and are
   easily chained by returning an Either
   Args: [l-f r-f]
   Returns: Function:
       Args: [Either]
       Returns: Any"
  [l-f r-f]
  (partial ee/branch l-f r-f))

(defn when-left
  "Given a function, returns a function which executes with the provided either
   only when it is Left or else returns the right. when-right is spelled '>>-'
   NOTE: you cannot bind into this, because bind will never reach it
   Correct example: `(when-left (>>- my monadic chain) (left nil))
   Args: [f]
   Returns: Function:
      Args: [either]
      Return:"
  [f]
  (branch f right))

(defn add-context
  "Given a key and a value, returns a function which adds the context to the
   context form it is given.
   Args: [k v]
   Returns: Function:
       Args: [ctx-form]
       Returns: right[ctx-form]"
  [k v]
  #(right (assoc-in % [k] v)))

(defn invert
  "Wraps the provided function in a function which inverts the either result
   provided, that is if it returns a left, return a right and vice versa.
   Eg: `(>>= (invert #(left nil)) handle-error)`"
  [f]
  (fn [e]
    (ee/invert (f e))))

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
  (partial u/ctx-pred p exp-desc))

;; ## Built in simple epreds

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

(defn ecount
  "Expects that the count of the form is either cnt or between low and high
   Args: [cnt] [low high]
   Returns: Either[ctx-form]"
  ([cnt]
     (epred (partial c/count= cnt) (str "collection of length " cnt)))
  ([low high]
     (epred (partial c/count<= low high)
            (str "collection of length between " low " and " high " (inc.)"))))

;; (defn enth-in 

;; (defn enth
;;   ""
;;   [ep nth]
;;   (fn [cf] ;;;;; WTF?
;;     (update-form (branch (ep (first f)) left) cf))
;;                                                     (replace-nth-in nth (
;;   #(update-form (-> coll empty (into %) right)))

(defn etuple
  "Given some preds, returns a function that expects a vector of that
   many items, zips the epreds and the received vector and binds them
   into a chain, returning either the first received Left or else a new
   vector made up from the Rights, in a Right.
   Args: [& preds]
   Returns: Function:
       Args: [ctx-form]
       Returns: Either"
  [& epreds]
  (fn [cf]
    (let [form (u/cf-form cf)
          rs (map #(% (u/replace-form %2 cf)) epreds form)]
      (or (ee/first-left rs)
          (-> form
              (c/into-coll (map (comp u/cf-form m/extract) rs))
              (u/replace-form cf)
              right)))))

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
  (partial u/eithery? epred))

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
          (u/fail {:expected desc} cf)))))

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

