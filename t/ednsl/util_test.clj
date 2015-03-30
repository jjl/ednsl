(ns ednsl.util-test
  (:use [midje.sweet]
        [cats.core :refer [>>= extract]]
        [cats.monad.either :refer [left left? right right?]]
        [ednsl.util]))

(facts :ednsl.util
  (fact :ctx-form
    (ctx-form :abc) => {:form :abc}
    (ctx-form :abc) => (map->CtxForm {:form :abc}))
  (fact :cf-form
    (cf-form {:form :foo}) => :foo
    (cf-form {}) => nil)
  (fact :update-form
    (update-form identity (ctx-form :abc)) => {:form :abc}
    (update-form (partial * 2) (ctx-form 2)) => {:form 4}
    (update-form identity (ctx-form :abc)) => (map->CtxForm {:form :abc}))
  (fact :fail
    (fail {}) => left?
    (extract (fail (ctx-form 123))) => {:form 123})
  (fact :ctx-pred
    (ctx-pred integer? "integer" (ctx-form 1)) => (right (ctx-form 1))
    (ctx-pred integer? "integer" (ctx-form :a))
    => (left (ctx-form {:form :a :expected "integer"})))
  (facts :collect
    (fact "returns a function"
      (collect ()) => fn?)
    (fact "collects vectors"
      (let [c (collect [])]
        (c (ctx-form [(right :a) (right :b)]))
        => (right (ctx-form [:a :b]))
        (c (ctx-form [(right :a) (right :b)]))
        => (right (ctx-form [:a :b]))))
    (fact "collects maps"
      (let [c (collect {})]
        (c (ctx-form [(right [:a :b]) (right [:c :d])]))
        => (right (ctx-form {:a :b :c :d})))))
  (fact "eithery?"
    (eithery? left  (ctx-form [1 2])) => (left (ctx-form 1))
    (eithery? right (ctx-form [1 2]))  => (right (ctx-form [1 2]))
    (eithery? right (ctx-form {1 2}))  => (right (ctx-form {1 2}))))

