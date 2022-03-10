(ns town.lilac.async.iteration
  (:require [kitchen-async.promise :as p]))


(defrecord AsyncIteration [step! vf kf some? k v]
  IDeref
  (-deref [_] v))


(defn iteration?
  [x]
  (instance? AsyncIteration x))


(defn result
  "If x is an AsyncIteration, returns (deref x), else returns x"
  [x]
  (if (iteration? x) @x x))


(defn iteration
  "Creates a new AsyncIteration object representing a process which, when
  executed, will call `step!` with a continuation token and return a promise
  containing the next call until complete.

  When executing, the first call to `step!` will be passed `initk` and should
  return a promise resolving to `ret`. The execution will return a promise that:
  * If (somef ret) is true, and (kf ret) returns a non-nil value `k`, resolves
    to an AsyncIteration `iter` representing the next call to (step! k). To get
    the result of (vf ret), deref `iter`
  * If (somef ret) is true and (kf ret) is nil, resolves to (vf ret)
  * If (somef ret) is false, resolves to nil and does not call `kf` or `vf`

  See `exec!` for executing an iteration.

  The following arguments can be passed to customize the behavior of the
  iteration:

   step! - (possibly impure) fn of a continuation token `k` to a promise
           of result `ret`

   :somef - fn of `ret` -> logical true/false, default `some?`
   :vf - fn of `ret` -> `v`, a value produced by the iteration, default
         `identity`
   :kf - fn of `ret` -> `next-k` or nil (signaling do not continue), default
         `identity`
   :initk - the first value passed to step!, default nil"
  [step! & {:keys [vf kf initk some?]
            :or {vf identity
                 kf identity
                 some? some?}}]
  (->AsyncIteration step! vf kf some? initk nil))


(defn exec!
  "Given an AsyncIteration object, executes it and returns a promise. The result
  will be either another AsyncIteration object to execute the next step, or the
  result of the final step. To get the result of each iteration, dereference the
  AsyncIteration."
  [async-iter]
  (let [{:keys [step! vf kf somef k v]} async-iter]
    (-> (step! k)
        (.then (fn [x]
                 (when (somef x)
                   (if-some [k (kf x)]
                     (->AsyncIteration step! vf kf somef k (vf x))
                     (vf v))))))))


(defn collect
  ([xform f iter] (collect xform f (f) iter))
  ([xform f init iter]
   (let [rf (xform f)
         do-collect! (fn do-collect! [iter results]
                       (-> (exec! iter)
                           (.then (fn [x]
                                    (if (some? x)
                                      (let [results (rf results (result x))]
                                        (cond
                                          (reduced? results) (unreduced results)

                                          (iteration? x) (do-collect! x results)

                                          :else results))
                                      results)))))]
     (do-collect! iter init))))


(defn collect-into
  ([to from]
   (collect identity conj to from))
  ([to xform from]
   (collect xform conj to from)))


;;
;; Example
;;


(def ^:private pages
  "Some example page data, keyed by the `:page`.
  The next page is contained in the payload at the `:next` key."
  (into {} (map (juxt :page identity)) (for [n (range 10)]
                                         {:page n
                                          :next (inc n)})))


(defn- fetch-page
  "Fetch a particular page"
  [n]
  (js/Promise.resolve (get pages n)))


(def ^:private page-iter
  "Create an iteration based on `fetch-page`."
  (iteration
   fetch-page
   :initk 0
   :kf :next))


;; using recursive fn calls

(defn fetch-pages
  ([n] (fetch-pages n [] page-iter))
  ([n results async-iter]
   (if (and (pos? n) (iteration? async-iter))
     (.then (exec! async-iter)
            (fn [x]
              (if (some? x)
                (let [results (conj results (result x)) ]
                  (if (iteration? x)
                    (fetch-pages (dec n) results x)
                    x))
                results)))
     (js/Promise.resolve results))))

#_(.then (fetch-pages 1) prn)

#_(.then (fetch-pages 21) prn)


;; using `collect` and a transducer

#_(.then (collect (take 3) conj [] page-iter) prn)

#_(.then (collect-into [] (take 3) page-iter) prn)


;; using kitchen-async

#_(.then (p/loop [pages 4
                  results []
                  x (exec! page-iter)]
           (if (and (pos? pages) (iteration? x))
             (p/recur (dec pages) (conj results @x) (exec! x))
             results))
         prn)
