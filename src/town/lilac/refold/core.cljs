(ns town.lilac.refold.core
  (:require
   [reagent.ratom :as ratom])
  (:refer-clojure :exclude [into reduce transduce]))


(defn- track-with-history
  [f init opts]
  (let [*prev (atom init)]
    (ratom/make-reaction
     #(let [prev @*prev]
        (if (reduced? prev)
          @prev
          (let [ret (f @*prev)]
            (when ratom/*ratom-context*
              (reset! *prev ret))
            (unreduced ret))))
     :on-dispose (fn []
                   (reset! *prev init)
                   (when-let [on-dispose (:on-dispose opts)]
                     (on-dispose)))
     :auto-run (:auto-run opts))))


(defn reduce
  ([f r] (reduce f (f) r))
  ([f init r]
   (track-with-history #(f % @r) init {})))


(defn reduce!
  ([f r] (reduce! f (f) r))
  ([f init r]
   (doto (track-with-history #(f % @r) init {:auto-run true}) deref)))


(defn transduce
  ([xform f r]
   (transduce xform f (f) r))
  ([xform f init r]
   (let [*f (atom (xform f))]
     (track-with-history
      #(@*f % @r)
      init
      {:on-dispose #(reset! *f (xform f))}))))


(defn transduce!
  ([xform f r]
   (transduce! xform f (f) r))
  ([xform f init r]
   (let [*f (atom (xform f))]
     (doto (track-with-history
            #(@*f % @r)
            init
            {:on-dispose #(reset! *f (xform f))
             :auto-run true})
       deref))))


(defn into
  ([to r]
   (reduce conj to r))
  ([to xform r]
   (transduce xform conj to r)))


(defn into!
  ([to r]
   (reduce! conj to r))
  ([to xform r]
   (transduce! xform conj to r)))


(defn scan
  [xform init r]
  (transduce xform (fn [_ x] x) init r))


(defn scan!
  [xform init r]
  (transduce! xform (fn [_ x] x) init r))
