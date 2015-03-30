(ns ednsl.util
 (:require [cats.core :as m :refer [>>=]]
           [cats.monad.either :as e]
           [collectible :as c]
           [ednsl.util.either :as ee]))

(defrecord CtxForm [form])

(defn ctx-form
  "Constructs a context form. If arg is a map, expect a :form argument
   else assume it is a form. Be careful with map data.
   Args: []
   Returns: ctx-form"
  [arg]
  (map->CtxForm 
   (if (and (map? arg) (contains? arg :form))
     arg
     {:form arg})))

(def cf-form
  "Returns the form from a given context form or else a default (default: nil)
   Args: [ctx-form default?]
   Returns: "
  :form)

(defn update-form
  "Given a function f and a ctx-form cf, applies the form inside cf and returns
   a new result with the form mutated to the return value of (f form) 
   Args: [f cf]"
  [f cf]
  (update-in cf [:form] f))

(defn replace-form
  [new cf]
  (update-form (constantly new) cf))

(defn fail
  "Fails because some data was invalid
   Args: [form extra-data?]
   Returns: Right"
  ([cf]
     (fail {} cf))
  ([extra cf]
     (e/left (into cf extra))))

(defn index-meta [form]
  (try
    (meta form)
    (catch Exception e {})))

(defn ctx-pred
  "Returns a function which applies the predicate to the incoming data and
   if truthy, returns the data, else fails with exp-desc
   Args: [p exp-desc]
   Returns: Function:
       Args: [ctx-form]
       Returns: Either[ctx-form]"
  [p exp-desc cf]
  (let [form (cf-form cf)]
    (if (p form)
      (e/right cf)
        (fail (merge (index-meta form) {:expected exp-desc}) cf))))

(defn collect
  "Given a collection, returns a function which takes a collection in a
   ctx-form and casts it to the type of the original collection
   Args: [coll]
   Returns: Function:
       Args: [cf]
       Returns: Either[ctx-form[typeof(coll)]]"
  [coll]
  (fn [cf]
    (let [rs (cf-form cf)]
      (or (ee/first-left rs)
          (e/right (update-form (constantly (c/into-coll coll (map m/extract rs))) cf))))))

(defn eithery?
  "Sorry for the pun.
   Takes a function and a collection and returns either the first Left result
   of calling the f with an element of the collection or a new collection
   made from the results in a Right. This means you can mutate in-transit.
   Args: [f coll ctx-form]
   Returns: Either[ctx-form]"
   [f cf]
   (let [form (cf-form cf)
         rs (reduce (fn [acc v]
                      (let [r (f (replace-form v cf))]
                        (if (e/left? r)
                          (reduced r)
                          (conj acc (-> r m/extract cf-form)))))
                    [] form)]
     (if (e/either? rs)
       rs
       (e/right (replace-form (c/into-coll form rs) cf)))))


;; (loop [cf2 cf fs form]
;;               (if (seq fs)
;;                 (recur (>>= (update-form (first fs) cf2) f)
                
              
;;               map (fn [i]
;;                    (f (update-form cf
;;      ;; If it's right, wrap it in the context and cast
;;      (>>= (m/sequence (apply >>= cf (map #(partial f %)) form))
;;           #(e/right (replace-form (c/into-coll form %) cf)))))
