(ns town.lilac.refold.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [reagent.ratom :as ratom]
   [town.lilac.refold.core :as refold]))


(deftest t-reduce
  (let [*a (ratom/atom 0)
        *r (refold/reduce + 0 *a)]
    (is (= 0 @*r))
    (swap! *a inc) ; 1
    (ratom/flush!)
    (swap! *a inc) ; 2
    (is (= 2 @*r) "always inits when not connected to another reaction")
    (reset! *a 0)  ; 0
    (let [*fx (ratom/run! @*r)]
      (swap! *a inc) ; 1
      (ratom/flush!)
      (swap! *a inc) ; 2
      (ratom/flush!)
      (swap! *a inc) ; 3
      (is (= 6 @*r) "reduces over values when connected")

      (ratom/dispose! *fx)
      (is (= 3 @*r) "removes state on dispose")
      (swap! *a inc)
      (ratom/flush!) ; 4
      (swap! *a inc)
      (ratom/flush!) ; 5
      (is (= 5 @*r) "goes back to initing after disposal")))
  (testing "reduced"
    (let [*a (ratom/atom 0)
          *r (refold/reduce
              (fn [acc n]
                (if (< n 4)
                  (+ acc n)
                  (reduced acc)))
              0 *a)]
      (ratom/run! @*r)
      (swap! *a inc) ; 1
      (ratom/flush!)
      (swap! *a inc) ; 2
      (ratom/flush!)
      (swap! *a inc) ; 3
      (ratom/flush!)
      (is (= 6 @*r))
      (swap! *a inc) ; 4
      (ratom/flush!)
      (is (= 6 @*r))
      (swap! *a inc) ; 5
      (ratom/flush!)
      (is (= 6 @*r)))))


(deftest t-reduce!
  (let [*a (ratom/atom 0)
        *r (refold/reduce! + 0 *a)]
    (is (= 0 @*r))
    (swap! *a inc) ; 1
    (ratom/flush!)
    (swap! *a inc) ; 2
    (is (= 3 @*r) "reduces over values on instantiation")
    (let [*fx (ratom/run! @*r)]
      (swap! *a inc) ; 3
      (ratom/flush!)
      (swap! *a inc) ; 4
      (ratom/flush!)
      (swap! *a inc) ; 5
      (is (= 15 @*r) "reduces over values when connected")

      (ratom/dispose! *fx)
      (is (= 15 @*r))
      (swap! *a inc)
      (ratom/flush!) ; 6
      (swap! *a inc)
      (ratom/flush!) ; 7
      (is (= 28 @*r) "continues reducing state after dependent disposal"))
    (ratom/dispose! *r)
    (is (= 7 @*r) "inits after being disposed directly")))


(deftest t-transduce
  (let [*a (ratom/atom 0)
        *r (refold/transduce (filter odd?) + 0 *a)]
    (is (= 0 @*r))
    (swap! *a inc) ; 1
    (ratom/flush!)
    (swap! *a inc) ; 2
    (ratom/flush!)
    (swap! *a inc) ; 3
    (is (= 3 @*r) "always inits when not connected to another reaction")
    (reset! *a 0)  ; 0
    (let [*fx (ratom/run! @*r)]
      (swap! *a inc) ; 1
      (ratom/flush!)
      (swap! *a inc) ; 2
      (ratom/flush!)
      (swap! *a inc) ; 3
      (is (= 4 @*r) "reduces over values when connected")

      (ratom/dispose! *fx)
      (is (= 3 @*r) "removes state on dispose")
      (swap! *a inc)
      (ratom/flush!) ; 4
      (swap! *a inc)
      (ratom/flush!) ; 5
      (is (= 5 @*r) "goes back to initing after disposal")))
  (testing "transducer state"
    (let [*a (ratom/atom 0)
          *r (refold/transduce
              (take 3)
              + 0 *a)]
      (is false "TODO")))
  (testing "reduced"
    (let [*a (ratom/atom 0)
          *r (refold/transduce
              (map inc)
              (fn [acc n]
                (if (< n 5)
                  (conj acc n)
                  (reduced acc)))
              [] *a)]
      (ratom/run! @*r)
      (swap! *a inc) ; 2
      (ratom/flush!)
      (swap! *a inc) ; 3
      (ratom/flush!)
      (swap! *a inc) ; 4
      (ratom/flush!)
      (is (= [1 2 3 4] @*r))
      (swap! *a inc) ; 5
      (ratom/flush!)
      (is (= [1 2 3 4] @*r))
      (swap! *a inc) ; 5
      (ratom/flush!)
      (is (= [1 2 3 4] @*r)))))


(clojure.test/run-tests)
