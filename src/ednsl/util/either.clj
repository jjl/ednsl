(ns ednsl.util.either
 (:require [cats.core :as m :refer [>>=]]
           [cats.builtin]
           [cats.monad.either :as e]))

(defn branch
  "Given an Either, execute either `(l-f val)` or `(r-f val)` where val is
   the value boxed inside the Either
   Args: [e l-f r-f]"
  [l-f r-f e]
  (let [f (-> e e/left? (if l-f r-f))]
    (-> e m/extract f)))

(defn !>= 
  "The inverse of >>=, chains forward when e is a Left.
   Args: [e & fs]"
  [e & [f & fs]]
  (if (e/left? e)
    (let [r (-> e m/extract f)]
      (if fs
        (recur r fs)
        r))
    e))

(defn >>-
  ""
  [& args]
  #(apply >>= % args))

(defn !>-
  ""
  [& args]
  #(apply !>= % args))

(def lefts
  "Given a list of Eithers, return the Lefts
   Args: [eithers]
   Returns: [eithers]"
  (partial filter e/left?))

(def rights
  "Given a list of Eithers, return the Rights
   Args: [eithers]
   Returns: [eithers]"
  (partial filter e/right?))

(def first-left
  "Returns the first left from a sequence"
  (comp first lefts))

(def first-right
  "Returns the first right from a sequence"
  (comp first rights))

(defn invert
  "Sometimes, you just want to make monadic chaining work in reverse
   This makes eor much easier to implement but it's a bit naughty
   Args: e
   Returns: if Left, Right, if Right, Left, preserving content"
  [e]
  (let [f (if (e/left? e) e/right e/left)]
    (-> e m/extract f)))
