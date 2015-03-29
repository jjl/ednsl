(ns ednsl.util.either-test
  (:use [midje.sweet]
        [cats.monad.either :refer [left right]]
        [ednsl.util.either]))

(facts :either-util
  (facts :branch
    (branch (constantly :left) (constantly :right) (left nil)) => :left
    (branch (constantly :left) (constantly :right) (right nil)) => :right)
  (facts :!>=
    (!>= (left 1) (comp left inc) inc) => 3
    (!>= (right 1) (comp left inc) inc) => (right 1))
  (facts :>>-
    ((>>- (comp right inc) inc)
     (right 1)) => 3)
  (facts :!>-
    ((!>- (comp left inc) inc)
     (left 1)) => 3)
  (facts :lefts
    (lefts [(left 1) (left 2) (right 3) (left 4)])
    => [(left 1) (left 2) (left 4)])
  (facts :rights
    (rights [(right 1) (right 2) (left 3) (right 4)])
    => [(right 1) (right 2) (right 4)])
  (facts :first-left
    (first-left [(left 1) (left 2) (right 3) (left 4)]) => (left 1)
    (first-left []) => nil)
  (facts :right-right
    (first-right [(right 1) (right 2) (left 3) (right 4)]) => (right 1)
    (first-right []) => nil)
  (facts :invert
    (invert (left :foo)) => (right :foo)
    (invert (right :foo)) => (left :foo)))

