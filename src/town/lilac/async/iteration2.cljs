(ns town.lilac.async.iteration2
  (:refer-clojure :exclude [cons first lazy-seq next])
  (:require-macros [town.lilac.async.iteration2 :refer [lazy-seq]]))


(defprotocol IAsyncSeq
  (await-first [as])
  (await-next [as]))


(defprotocol IAsyncSeqable
  (aseq [x]))


(extend-type nil
  IAsyncSeqable
  (aseq [_] (js/Promise.resolve nil)))


(deftype AsyncLazySequence [thunk ^:mutable cached-aseq]
  IPending
  (-realized? [_] (some? cached-aseq))
  IAsyncSeqable
  (aseq [_]
    (if cached-aseq
      cached-aseq
      (doto (let [s (or cached-aseq (thunk))]
              (if (instance? AsyncLazySequence s)
                (aseq s)
                s))
        (->> (set! cached-aseq)))))
  IAsyncSeq
  (await-first [this]
    (.then (aseq this) await-first))
  (await-next [this]
    (.then (aseq this) await-next)))


(deftype AsyncCons [head next]
  IAsyncSeqable
  (aseq [this] (js/Promise.resolve this))
  IAsyncSeq
  (await-first [_] (js/Promise.resolve head))
  (await-next [_] (aseq next)))


(defn cons
  ([v thunk]
   (->AsyncCons v thunk)))


(defn first [aseq] (await-first aseq))


(defn next [aseq] (await-next aseq))


(defn transmute
  ([xform f src] (transmute xform f (f) src))
  ([xform f init src]
   (let [rf (xform f)
         step
         (fn step [results iter]
           (-> (aseq iter)
               (.then (fn [x]
                        (if (some? x)
                          (.then (first x)
                                 #(let [results (rf results %)]
                                    (if (reduced? results)
                                      (unreduced results)
                                      (.then (next x) (partial step results)))))
                          results)))))]
     (step init src))))



(defn collect
  ([to from] (transmute identity conj to from))
  ([to xform from] (transmute xform conj to from)))


(defn iteration
  [step & {:keys [somef vf kf initk]
           :or {somef some?
                vf identity
                kf identity}}]
  (lazy-seq
   ((fn do-next [p]
      (.then p (fn [ret]
                 (when (somef ret)
                   (cons
                    (vf ret)
                    (when-some [k (kf ret)]
                      (lazy-seq (do-next (step k)))))))))
    (step initk))))


#_(-> (cons
     (js/Promise.resolve 1)
     #(cons (js/Promise.resolve 2) nil))
    (next)
    (.then first)
    (.then prn))


#_(-> (->AsyncLazySequence
     (fn []
       (cons
        (js/Promise.resolve 1)
        (->AsyncLazySequence
         (fn []
           (cons (js/Promise.resolve 2) nil))
         nil)))
     nil)
    (next)
    #_(.then first)
    (.then next)
    (.then prn))


#_(lazy-seq
   (cons
    (js/Promise.resolve 1)
    nil))


(def ^:private pages-data
  "Some example page data, keyed by the `:page`.
  The next page is contained in the payload at the `:next` key."
  (into {} (map (juxt :page identity)) (for [n (range 10)]
                                         {:page n
                                          :next (inc n)})))


(defn- fetch-page
  "Fetch a particular page"
  [n]
  (prn "Fetching " n)
  (js/Promise.resolve (get pages-data n)))


(def ^:private pages
  "Create an async iteration based on `fetch-page`."
  (iteration
   fetch-page
   :initk 0
   :kf :next))


#_(-> pages
      (aseq)
      (.then next)
      (.then next)
      (.then first)
      (.then prn))


(defn fetch-pages
  ([n] (.then (first pages) #(fetch-pages (dec n) [%] (next pages))))
  ([n results p]
   (if (pos? n)
     (.then p (fn [aseq]
                (if aseq
                  (.then (first aseq)
                         #(fetch-pages (dec n) (conj results %) (next aseq)))
                  results)))
     results)))

#_(.then (fetch-pages 4) prn)


#_(.then (transmute (comp (take 3) (map :page)) conj [] pages) prn)
