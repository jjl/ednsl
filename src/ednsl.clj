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
(declare eor) ; Move declaration order for didactic purposes

;; ## Convenience functions


(def ctx-form u/ctx-form)
(def left e/left)
(def right e/right)

(def left?  "Is this a left? Any -> Bool."  (every-pred e/either? e/left?))
(def right? "Is this a right? Any -> Bool." (every-pred e/either? e/right?))

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

(defmacro when-left*
  ""
  [e & exprs]
  `(let [e# ~e]
     (if (left? e#)
       (do ~@exprs)
       e#)))

(defmacro when-not-left*
  ""
  [e & exprs]
  `(let [e# ~e]
     (if (left? e#)
       e# 
       (do ~@exprs))))

(defmacro when-right*
  ""
  [e & exprs]
  `(let [e# ~e]
     (if (right? e#)
       (do ~@exprs)
       e#)))

(defmacro when-not-right*
  ""
  [e & exprs]
  `(let [e# ~e]
     (if (right? e#)
       e# 
       (do ~@exprs))))

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
      Return: Right, or return-typeof(f)"
  [f]
  #(when-right* % (f m/extract %)))

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
  #(ee/invert (f %)))

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

;; ## Built in simple epreds

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
       Returns: Either"

  [& epreds]
  (fn [{:keys [form] :as cf}]

    (let [rs (map #(% (u/replace-form %2 cf)) epreds form)]
      (if-let [l (ee/first-left rs)]
        l
        (-> (c/into-coll form (map (comp u/cf-form m/extract) rs))
            (u/replace-form cf) right)))))

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
    (loop [prev cf
           acc []
           [e & es] epreds
           [f & fs] form]
      (when-right* prev
                   (let [r (e (u/replace-form f prev))
                         new-acc (conj acc (m/extract r))]
                     (when-right* r
                                  (if (every? seq [es fs])
                                    (recur r new-acc es fs)
                                    (right (u/replace-form new-acc cf)))))))))

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

