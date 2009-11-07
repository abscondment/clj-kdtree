(ns clojure.lang.kdtree)

(use 'clojure.test)

(defstruct node :v :l :r)

(defn build-tree
  ([points] (build-tree points 0))
  ([points depth]
     (let [point-count (count points)]
       (cond (= 0 point-count) nil
             (= 1 point-count) (nth points 0)
             true
             (let [k (count (nth points 0))
                   dimension (mod depth k)
                   points (vec (sort-by #(nth % dimension) points))
                   median (quot point-count 2)]
               (struct-map node
                 :v (nth points median)
                 :l (build-tree (subvec points 0 median) (inc depth))
                 :r (build-tree (subvec points (inc median)) (inc depth))))))))

(comment
  ;;; This doesn't actually work, but I can't think straight and need to go home.
(defn- distance-between [a b]
  (reduce + (map #(let [n (apply - %)] (* n n))
                 (partition 2 (interleave a b)))))

(defn nearest-neighbor
  ([tree point] (nearest-neighbor tree point 0 '()))
  ([tree point depth path]
     (cond (nil? tree) path
           (not (map? tree))
           (take 3
                 (sort-by :dist (cons {:node tree :dist (distance-between tree point)} path)))
           ;;; Otherwise...
           true
           (let [dimension (mod depth (count point))
                 dir (if (> (nth (:v tree) dimension)
                            (nth point dimension)) :r :l)
                 odir (if (= :r dir) :l :r)
                 best (take 3 (nearest-neighbor
                               (dir tree)
                               point
                               (inc depth)
                               (sort-by
                                :dist
                                (cons {:node (:v tree)
                                       :dist (distance-between (:v tree) point)}
                                      path))))]
                 best))))
                 
                                            

(def tree (build-tree [[1 11] [2 5] [4 8] [6 4] [5 0] [7 9] [8 2]]))

(println tree)
(println (nearest-neighbor tree [3 9]))
)

;;; TESTS

(deftest Build-2d-Example-Wikipedia
  (let [tree (build-tree [[4 7] [2 3] [5 4] [9 6] [8 1] [7 2]])]
    (is (= tree
           {:v [7,2]
            :l {:v [5,4]
                :l [2,3]
                :r [4,7]}
            :r {:v [9,6]
                :l [8,1]
                :r nil}}))))

(deftest Build-3d-Example-A
  (let [tree (build-tree [[5 5 5] [2 2 2] [6 6 6] [7 7 7]
                          [4 4 4] [1 1 1] [3 3 3] [8 8 8]])]
    (is (= tree
           {:v [5 5 5]
            :l {:v [3 3 3]
                :l {:v [2 2 2]
                    :l [1 1 1]
                    :r nil}
                :r [4 4 4]}
            :r {:v [7 7 7]
                :l [6 6 6]
                :r [8 8 8]}}))))

(deftest Build-3d-Example-B
  (let [tree (build-tree [[5 5 5] [2 2 2] [6 6 6] [7 7 7]
                          [4 4 4] [1 1 1] [3 3 3]])]
    (is (= tree
           {:v [4 4 4]
            :l {:v [2 2 2]
                :l [1 1 1]
                :r [3 3 3]}
            :r {:v [6 6 6]
                :l [5 5 5]
                :r [7 7 7]}}))))

(deftest Build-3d-Example-C
  (let [tree (build-tree [[1 9 9] [2 3 1] [3 1 4] [4 7 6]
                          [5 2 3] [6 8 7] [7 6 5] [8 5 4]])]
    (is (= tree
           {:v [5 2 3]
            :l {:v [4 7 6]
                :l {:v [3 1 4]
                    :l [2 3 1]
                    :r nil}
                :r [1 9 9]}
            :r {:v [7 6 5]
                :l [8 5 4]
                :r [6 8 7]}}))))

;(run-tests)
