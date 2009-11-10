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
                 :l (build-tree (subvec points 0 median)
                                (inc depth))
                 :r (build-tree (subvec points (inc median))
                                (inc depth))))))))


;;; There is probably a faster way to compute k-dimensional distance
(defn- dist-squared [a b]
  (reduce + (map #(let [n (apply - %)] (* n n))
                 (partition 2 (interleave a b)))))

(defn nearest-neighbor
  ([tree point] (first (nearest-neighbor tree point 1 0 nil)))
  ([tree point n] (nearest-neighbor tree point n 0 nil))
  ([tree point n depth best]
     (take n
      (sort-by :dist-squared
       (cond
        ;;; Empty tree? The best list is unchanged.
        (nil? tree) best

        ;;; Not a map? We hit an edge - try to stick it on the best list.
        (not (map? tree))
        (cons {:node tree :dist-squared (dist-squared tree point)} best)
      
        ;;; Otherwise, recurse!
        true
        (let [dimension (mod depth (count point))
              dim-dist (- (nth point dimension) (nth (:v tree) dimension))
              search-order (if (> dim-dist 0) '(:r :l) '(:l :r))

              ;;; Compute a best list for the near-side of the search order
              best-near
              (nearest-neighbor ((first search-order) tree)
                                point
                                n
                                (inc depth)
                                (cons {:node
                                       (:v tree)
                                       :dist-squared
                                       (dist-squared (:v tree) point)}
                                      best))]

          ;;; If the square distance of our search node to point in the current
          ;;; dimension is still better than the *worst* of the near-side best
          ;;; list, there may be a better solution on the far side. Compute &
          ;;; combine with near-side solutions.
          (if (< (* dim-dist dim-dist) (:dist-squared (last best-near)))
            (concat best-near
                    (nearest-neighbor
                     ((last search-order) tree) point n (inc depth) nil))
            best-near)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS


;;; TREE CREATION

;;; 2d-tree example found at: http://en.wikipedia.org/wiki/Kd-tree
(deftest- Build-2d-Example-Wikipedia
  (let [tree (build-tree [[4 7] [2 3] [5 4] [9 6] [8 1] [7 2]])]
    (is (= tree
           {:v [7,2]
            :l {:v [5,4]
                :l [2,3]
                :r [4,7]}
            :r {:v [9,6]
                :l [8,1]
                :r nil}}))))

;;; Naïve 3d-tree
(deftest- Build-3d-Example-A
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

;;; Variation on naïve 3d-tree
(deftest- Build-3d-Example-B
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

;;; Slightly more complicated 3d-tree
(deftest- Build-3d-Example-C
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


;;; NEAREST-NEIGHBOR SEARCH

;;; Simple example based on the search animation from:
;;; http://en.wikipedia.org/wiki/Kd-tree
(deftest- Neighbors-2d-Example-Wikipedia
  (let [tree (build-tree [[1 11] [2 5] [4 8] [6 4] [5 0] [7 9] [8 2]])]
    (is (= (nearest-neighbor tree [3 9])
           {:node [4 8] :dist-squared 2}))))

;;; A naïve 2d example.
(deftest- Neighbors-2d-Example
  (let [points [[8 8] [3 1] [1 1] [6 6] [7 7] [3 3] [1 3] [4 4] [5 5]]
        tree (build-tree points)]
    ;;; Confirm that the nearest hit is one of the four
    ;;; points that are root 2 away.
    (is (= 2 (:dist-squared (nearest-neighbor tree [2 2]))))
    ;; Confirm that the four nearest are our points on the 1 and 3 lines.
    (is (= (sort (map :node (nearest-neighbor tree [2 2] 4)))
           '([1 1] [1 3] [3 1] [3 3])))
    ;; Confirm that the ones after our first 4 = rest in ascending order.
    (is (= (drop 4 (map :node (nearest-neighbor tree [2 2] (count points))))
           (drop 4 (sort points))))))

;;; Which cities are closest to Seattle?
(deftest Neighbors-2d-Seattle
  (let [cities { [47.6203   -122.34932]  :seattle
                 [37.810054 -122.477876] :san-francisco
                 [41.007252  28.978135]  :binbirdirek
                 [40.69     -74.045]     :new-york
                 [48.8577    2.295]      :paris }
        tree (build-tree (keys cities))
        point [43.880154 -103.4593]]
    (is (= #{:seattle :san-francisco}
           (apply hash-set
                  (map #(get cities %)
                       (map :node (nearest-neighbor tree point 2))))))))

;;; We search a 4-d tree; its coordinates are provided by four unbounded
;;; functions. We look for neighbors using a point whose coordinates are
;;; *below* n=1 for each function, ensuring that the smallest point of the
;;; tree will be the first neighbor.
(deftest- Neighbors-4d-Example
  (let [points (map #(list (Math/pow Math/PI (/ % 2))
                           (Math/pow Math/PI %)
                           (Math/sqrt (* % % Math/E))
                           (Math/pow Math/E %))
                    (range 1 4000))
        tree (build-tree points)]
    (is (= (first points)
           (:node (nearest-neighbor tree [0.1 0.2 0.3 0.4]))))))


;(run-tests)
