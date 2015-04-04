(ns ednsl-test
  (:use [midje.sweet]
        [ednsl])
  (:require [cats.core :as m]
            [cats.monad.either :as e]))

(def existent-symbol :bar)

(facts :context
  (fact :ctx-form
    (ctx-form :abc) => {:form :abc}
    (ctx-form :abc) => (map->CtxForm {:form :abc}))
  (fact :cf-form
    (cf-form {:form :foo}) => :foo
    (cf-form {}) => nil)
  (fact :update-form*
    (update-form* identity (ctx-form :abc)) => {:form :abc}
    (update-form* (partial * 2) (ctx-form 2)) => {:form 4}
    (update-form* identity (ctx-form :abc)) => (map->CtxForm {:form :abc}))
  (fact :fail
    (fail {}) => left?
    (m/extract (fail (ctx-form 123))) => {:form 123})
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
  (fact "eithery*"
    (eithery* left  (ctx-form [1 2])) => (left (ctx-form 1))
    (eithery* right (ctx-form [1 2]))  => (right (ctx-form [1 2]))
    (eithery* right (ctx-form {1 2}))  => (right (ctx-form {1 2}))))

(facts :utilities
  (fact :left?
    (left? (e/left 1))  => true
    (left? (e/right 0)) => false
    (left? nil)         => false)
  (fact :right?
    (right? (e/left 1))  => false
    (right? (e/right 0)) => true
    (right? nil)         => false)
  (facts :add-context
    (let [ac (add-context :foo :bar)]
      (fact "returns a function"
        ac => fn?)
      (fact "logic"
        (ac (ctx-form [])) => (e/right (ctx-form {:form [] :foo :bar}))))))
(facts :building-blocks
  (facts :branch
    (let [b (branch (constantly :left) (constantly :right))]
      (fact "returns a function"
        b => fn?)
      (fact "logic"
        (b (e/left  1)) => :left
        (b (e/right 1)) => :right)))
  (facts :when-left
    (let [wl (when-left (constantly :left))]
      (fact "returns a function"
        wl => fn?)
      (fact "logic"
        (wl (e/left nil)) => :left
        (wl (e/right :right)) => (e/right :right))))
  (facts :invert
    (fact "returns a function"
      (invert nil) => fn?)
    (fact "logic"
      ((invert identity) (e/left  1)) => (e/right 1)
      ((invert identity) (e/right 1)) => (e/left 1)))
  (facts :epred
    (fact "returns a function"
      (epred nil? nil) => fn?)
    ((epred integer? "foo") (ctx-form 1))
    => (e/right (ctx-form 1))
    ((epred integer? "foo") (ctx-form :foo))
    => (e/left (ctx-form {:form :foo
                          :expected "foo"})))
  (facts :eor
    (let [e (eor "string or key" estr ekey)]
      (fact "returns a function"
        e => fn?)
      (fact "logic"
        (e (ctx-form "foo")) => (right (ctx-form "foo"))
        (e (ctx-form :foo))  => (right (ctx-form :foo))
        (e (ctx-form 123))   => (left  (ctx-form {:form 123
                                                  :expected "string or key"}))))))
(facts :epreds
  (fact :enil
    (enil (ctx-form nil))  => (e/right (ctx-form nil))
    (enil (ctx-form :foo)) => (e/left  (ctx-form {:form :foo
                                                  :expected "nil"})))
  (fact :etrue
    (etrue (ctx-form true)) => (e/right (ctx-form true))
    (etrue (ctx-form :foo)) => (e/left  (ctx-form {:form :foo
                                                   :expected "true"})))
  (fact :false
    (efalse (ctx-form false)) => (e/right (ctx-form false))
    (efalse (ctx-form true))  => (e/left  (ctx-form {:form true
                                                     :expected "false"})))
  (fact :ebool
    (ebool (ctx-form true))  => (e/right (ctx-form true))
    (ebool (ctx-form false)) => (e/right (ctx-form false))
    (ebool (ctx-form :foo))  => (e/left  (ctx-form {:form :foo
                                                    :expected "bool"})))
  (fact :eint
    (eint (ctx-form 1))    => (e/right (ctx-form 1))
    (eint (ctx-form :foo)) => (e/left  (ctx-form {:form :foo
                                                  :expected "integer"})))
  (fact :estr
    (estr (ctx-form "foo")) => (e/right (ctx-form "foo"))
    (estr (ctx-form :foo))  => (e/left  (ctx-form {:form :foo
                                                   :expected "string"})))
  (fact :ekey
    (ekey (ctx-form :foo)) => (e/right (ctx-form :foo))
    (ekey (ctx-form 1))    => (e/left  (ctx-form {:form 1
                                                  :expected "keyword"})))
  (fact :esym
    (esym (ctx-form 'a))   => (e/right (ctx-form 'a))
    (esym (ctx-form :foo)) => (e/left  (ctx-form {:form :foo
                                                  :expected "symbol"})))
  (fact :efn
    (efn (ctx-form cons)) => (e/right (ctx-form cons))
    (efn (ctx-form :foo)) => (e/left  (ctx-form {:form :foo
                                                 :expected "function"})))
  (fact :elist
    (elist (ctx-form '(:a))) => (e/right (ctx-form '(:a)))
    (elist (ctx-form :foo))  => (e/left  (ctx-form {:form :foo
                                                    :expected "list"})))
  (fact :evec
    (evec (ctx-form [:a])) => (e/right (ctx-form [:a]))
    (evec (ctx-form :foo)) => (e/left  (ctx-form {:form :foo
                                                  :expected "vector"})))
  (fact :emap
    (emap (ctx-form {:a :b})) => (e/right (ctx-form {:a :b}))
    (emap (ctx-form :foo))    => (e/left  (ctx-form {:form :foo
                                                     :expected "map"})))
  (facts :ecount
    (fact "returns a function"
      (ecount 1) => fn?)
    (fact "logic"
      ((ecount 1) (ctx-form [1])) => (e/right (ctx-form [1]))
      ((ecount 1 2) (ctx-form [1])) => (e/right (ctx-form [1]))
      ((ecount 1) (ctx-form [1 2]))
      => (e/left (ctx-form {:form [1 2]
                            :expected "collection of length 1"}))
      ((ecount 1 2) (ctx-form [1 2 3]))
      => (e/left (ctx-form {:form [1 2 3]
                            :expected "collection of length between 1 and 2 (inc.)"})))))
(facts :data-structures
  (facts :etuple
    (let [e (etuple e/right e/left)]
      (fact "returns a fn"
        e => fn?)
      (fact "results"
        (e (ctx-form [1 2])) => (e/left (ctx-form 2))
        ((etuple e/right e/right)
         (ctx-form [1 2])) => (e/right (ctx-form [1 2])))))
  (facts :etuple-chain
    (let [e (etuple-chain e/right e/left)]
      (fact "returns a fn"
        e => fn?)
      (fact "results"
        (e (ctx-form [1 2])) => (e/left (ctx-form 2))
        ((etuple-chain e/right e/right)
         (ctx-form [1 2])) => (e/right (ctx-form [1 2]))))
    (fact "chains nicely"
      (let [ec (etuple-chain (add-context :foo :bar) #(right (replace-form* (:foo %) %)))]
        ec => fn?
        (ec (ctx-form [1 2])) => (right (ctx-form [1 :bar])))))
  (facts :eithery
    (let [ei (eithery eint)]
      (fact "returns a function"
        ei => fn?)
      (fact "logic"
        (ei (ctx-form [1 2]))
        => (e/right (ctx-form [1 2]))
        (ei (ctx-form [:foo :bar]))
        => (e/left (ctx-form {:form :foo, :expected "integer"}))))
    (fact "maps"
      (let [ei (eithery (etuple eint eint))]
        (ei (ctx-form {1 2 3 4})) => (e/right (ctx-form {1 2 3 4})))))
  (facts :ekeys
    (let [ek (ekeys eint)]
      (fact "returns a function"
        ek => fn?)
      (fact "logic"
        (ek (ctx-form {1 :a 2 :b})) => (e/right (ctx-form {1 :a 2 :b}))
        (ek (ctx-form {:a 1 2 :b}))
        => (e/left (ctx-form {:form :a, :expected "integer"})))))
  (facts :evals
    (let [ev (evals eint)]
      (fact "returns a function"
        ev => fn?)
      (fact "logic"
        (ev (ctx-form {:a 1 :b 2})) => (e/right (ctx-form {:a 1 :b 2}))
        (ev (ctx-form {:a 1 2 :b}))
        => (e/left (ctx-form {:form :b, :expected "integer"}))))))
(facts :utilities
  (fact :eload-sym
    (eload-sym (ctx-form 'nonexistent/symbol))
    =>  (left (ctx-form {:form 'nonexistent/symbol
                         :expected "loadable symbol"}))
    (eload-sym (ctx-form `existent-symbol))
    =>  (right (ctx-form existent-symbol)))
  (facts :reading
    (let [f (read-file "t-data/test.edn")]
      f => {:foo '(bar baz) :quux ["foo"]}
      (read-str (slurp "t-data/test.edn")) => f)))
