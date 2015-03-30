

```clojure
(ns oolong
  (:use [ednsl]))

;; First, we'll define the context structure we need and some utilities
(defn context [env config]
  [env config])

(def limit [key]
  (fn [cf]
    (update-ctx cf #(get % key)))

(def component
(def oolong (eor 
(def system (eor (>>= emap (ekeys ekey) (evals system)


(def oolong (
```
