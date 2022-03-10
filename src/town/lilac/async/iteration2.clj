(ns town.lilac.async.iteration2
  (:refer-clojure :exclude [cons lazy-seq]))

(defmacro lazy-seq
  [& body]
  (list 'new 'town.lilac.async.iteration2/AsyncLazySequence (list* 'fn* [] body) nil))
