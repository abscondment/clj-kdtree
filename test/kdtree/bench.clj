(ns kdtree.bench
  (:require [kdtree :as kd]
            [criterium.core :as cr])
  (:import [net.sf.javaml.core.kdtree KDTree]))

(defprotocol GenericKDTree
  (delete [this point])
  (nearest-neighbor [this point n])
  (insert [this point])
  (delete [this point])
  (find-min [this dimension]))

(defn build-tree [points type]
  (case type
    :clojure (kd/build-tree points)
    :java (let [tree (KDTree. 2)]
            (doseq [point points]
              (.insert tree (double-array point) point))
            tree)
    (throw (IllegalArgumentException. (str "Not supported tree type: " type)))))

(extend-protocol GenericKDTree
  kdtree.Node
  (delete [this point] (kd/delete this point))
  (nearest-neighbor [this point n] (kd/nearest-neighbor this point n))
  (insert [this point] (kd/insert this point))
  (delete [this point] (kd/delete this point))
  (find-min [this dimension] (kd/find-min this dimension))
  KDTree
  (delete [this point]
    (->> (into-array Double/TYPE point)
         (.delete this))
    this)
  (nearest-neighbor [this point n]
    (let [key (into-array Double/TYPE point)
          neibs (seq (.nearest this key n))
          dst-sqrd (fn [p2]
                     (apply + (map (fn [v1 v2]
                                     (* (- v1 v2)
                                        (- v1 v2)))
                                   point p2)))]
      (map (fn [p] {:point p :dist-squared (dst-sqrd p)}) neibs))))


(defn rand-point []
  [(rand-int 1000000) (rand-int 1000000)])

(def points (doall (repeatedly 5000 rand-point)))

(defn report [name [mean]]
  (let [[scale unit] (cr/scale-time mean)]
   (printf "%10s: %s\n" name (str (cr/format-value mean scale unit)))))

(def ^:dynamic *bench-opts* cr/*default-quick-bench-opts*)

(defmacro bench-with-tree [name type expr]
  `(let [~'tree (build-tree points ~type)]
     (->> (cr/benchmark ~expr *bench-opts*)
          :sample-mean
          (report ~name))))

(defn bench-build [type]
  (->> (cr/benchmark (build-tree points type) *bench-opts*)
       :sample-mean
       (report "build")))

(defn bench-nearest-neighbor [type]
  (bench-with-tree "near neigh" type
                   (nearest-neighbor tree (rand-point) 10)))

(defn bench-find-min [type]
  (when-not (= :java type)
   (bench-with-tree "find min" type
                    (find-min tree (rand-int (count (first points)))))))

(defn bench-insert [type]
  (when-not (= :java type)
   (bench-with-tree "insert" type
                    (insert tree (rand-point)))))

(defn bench-delete [type]
  (when-not (= :java type)
   (bench-with-tree "delete" type
                    (delete tree (rand-nth points)))))

(defn bench-all [type & options]
  (let [bench-opts (if (= (first options) :full) {} *bench-opts*)]
    (binding [*bench-opts* bench-opts
              ;;; Probably it bad thing to raise gc threshold
              ;;; but I want to avoid annoying WARN messages.
              cr/*final-gc-problem-threshold* 0.5]
      (println \newline "Benchmarking" (name type) "implementation")
      (doto type
        bench-build
        bench-nearest-neighbor
        bench-find-min
        bench-insert
        bench-delete))))



#_(

   (bench-build :clojure)

   (bench-nearest-neighbor :clojure)

   (bench-find-min :clojure)

   (bench-insert :clojure)

   (bench-delete :clojure)

   (bench-all :clojure)

   )
