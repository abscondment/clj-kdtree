(ns kdtree.test
  (:use [kdtree] :reload)
  (:import [kdtree Node Result])
  (:use [clojure.test]))

;; pull in private function
(def dist-squared (ns-resolve 'kdtree 'dist-squared))
(def insert-sorted! (ns-resolve 'kdtree 'insert-sorted!))

(defn- dist-squared-vec [a b]
  (dist-squared (double-array a) (double-array b)))

(defn- point-to-ints [p]
  ((comp vec (partial map int)) p))

(defn- results-to-int-points [r]
  (map (comp point-to-ints :point) r))

(defn- legible-tree [t]
  (if (not (nil? t))
    (let [val (point-to-ints (seq (:value t)))
          left (:left t)
          right (:right t)]
      (if (or left right)
        (list val (legible-tree left) (legible-tree right))
        (list val)))))

(defn- ds
  [& args]
  (double-array args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Test kD distance-squared function
(deftest- Distance-Squared
  ;;; Simple 2-d point
  (is (== (dist-squared-vec [0 0] [1 1]) 2))
  ;;; 2-d using floating points
  (is (== (dist-squared-vec [Math/PI -1.0] [1.0 Math/PI])
          (+ (Math/pow (- Math/PI 1) 2) (Math/pow (inc Math/PI) 2))))
  ;;; Simple 5-d distance
  (is (== (dist-squared-vec [1 1 1 1 1] [2 2 2 2 2]) 5))
  ;;; 10-d, floating point distance
  (is (== (dist-squared-vec [0   1   2   3   4   5   6   7   8   9]
                            [1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2.0])
          (+ (* 1.1 1.1)
             (* 0.2 0.2)
             (* 0.7 0.7)
             (* 1.6 1.6)
             (* 2.5 2.5)
             (* 3.4 3.4)
             (* 4.3 4.3)
             (* 5.2 5.2)
             (* 6.1 6.1)
             (* 7.0 7.0)))))


(deftest- Insert-Sorted
  (letfn [(to-res [vals]
            (mapv #(Result. % % nil nil) vals))]
    (are [vals new-val n expected] (= (-> (to-res vals)
                                          transient
                                          (insert-sorted! (Result. new-val new-val nil nil) n)
                                          persistent!)
                                      (to-res expected))
         [1 2 3] 4 5 [1 2 3 4]
         [2 3 4] 1 5 [1 2 3 4]
         [1 2 3] 4 3 [1 2 3]
         [1 2 3] 0 3 [0 1 2]
         [1 3 4] 2 3 [1 2 3]
         [] 1 5 [1])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tree-building tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2d-tree example found at: http://en.wikipedia.org/wiki/Kd-tree
(deftest- Build-2d-Example-Wikipedia
  (let [tree (legible-tree
              (build-tree [[4 7] [2 3] [5 4] [9 6] [8 1] [7 2]]))]
    (is (= tree
           '([7 2]
               ([5 4] ([2 3]) ([4 7]))
               ([9 6] ([8 1]) nil))))))

;;; Set of points with duplicate x and y coordinates
(deftest- Build-2d-Example-B
  (let [tree (legible-tree
              (build-tree [[0 0] [0 1] [0 2] [2 2] [2 0]]))]
    (is (= tree
           '([0 0]
               nil
               ([0 2]
                  ([2 0] ([0 1]) nil)
                  ([2 2])))))))

;;; Simple 3d-tree
(deftest- Build-3d-Example-A
  (let [tree (legible-tree
              (build-tree [[5 5 5] [2 2 2] [6 6 6] [7 7 7]
                           [4 4 4] [1 1 1] [3 3 3] [8 8 8]]))]
    (is (= tree
           '([5 5 5]
               ([3 3 3]
                  ([2 2 2] ([1 1 1]) nil)
                  ([4 4 4]))
               ([7 7 7] ([6 6 6]) ([8 8 8])))))))

;;; Variation on simple 3d-tree
(deftest- Build-3d-Example-B
  (let [tree (legible-tree
              (build-tree [[5 5 5] [2 2 2] [6 6 6] [7 7 7]
                           [4 4 4] [1 1 1] [3 3 3]]))]
    (is (= tree
           '([4 4 4]
               ([2 2 2] ([1 1 1]) ([3 3 3]))
               ([6 6 6] ([5 5 5]) ([7 7 7])))))))

;;; Slightly more complicated 3d-tree
(deftest- Build-3d-Example-C
  (let [tree (legible-tree
              (build-tree [[1 9 9] [2 3 1] [3 1 4] [4 7 6]
                           [5 2 3] [6 8 7] [7 6 5] [8 5 4]]))]
    (is (= tree
           '([5 2 3]
               ([4 7 6]
                  ([3 1 4] ([2 3 1]) nil)
                  ([1 9 9]))
               ([7 6 5] ([8 5 4]) ([6 8 7])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nearest-neighbor tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Test that search results match the naïve sort-by-distance algorithm.
(deftest- Neighbors-Sorting-Example
  (let [points (for [x (range 2 20)
                     y (range 2 20)]
                 [(* 0.95 x) (* -1.23 y)])
        cpoint [Math/PI Math/E]
        tree (build-tree points)]
    (is (= (take 5 (sort-by #(dist-squared-vec cpoint %) points))
           (map :point (nearest-neighbor tree cpoint 5))))))

;;; Simple example based on the search animation from:
;;; http://en.wikipedia.org/wiki/Kd-tree
(deftest- Neighbors-2d-Example-Wikipedia
  (let [tree (build-tree [[1 11] [2 5] [4 8] [6 4] [5 0] [7 9] [8 2]])]
    (is (= (nearest-neighbor tree [3 9])
           (kdtree/Result. [4.0 8.0] 2.0)))))

;;; A simple 2d example.
(deftest- Neighbors-2d-Example
  (let [points [[8 8] [3 1] [1 1] [6 6] [7 7] [3 3] [1 3] [4 4] [5 5]]
        tree (build-tree points)]
    ;;; Confirm that the nearest hit is one of the four
    ;;; points that are root 2 away.
    (is (== 2 (:dist-squared (nearest-neighbor tree [2 2]))))
    ;; Confirm that the four nearest are our points on the 1 and 3 lines.
    (is (= (sort
            (results-to-int-points (nearest-neighbor tree [2 2] 4)))
           '([1 1] [1 3] [3 1] [3 3])))
    ;; Confirm that the ones after our first 4 = rest in ascending order.
    (is (= (drop 4 (results-to-int-points (nearest-neighbor tree [2 2] (count points))))
           (drop 4 (sort points))))))

;;; Some real-world location comparisons.
;;; http://maps.google.com/maps?q=47.6203+-122.34932
(deftest- Neighbors-2d-Seattle
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
                       (map :point (nearest-neighbor tree point 2))))))))

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
           (:point (nearest-neighbor tree [0.1 0.2 0.3 0.4]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insertion tests that repeat prior point combinations, but build the
;;; underlying trees using insert.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Mimic Neighbors-2d-Example, but build the tree normally and add one point
;;; via insert.
(deftest- Insert-2d-Example
  (let [points [[8 8] [3 1] [6 6] [7 7] [3 3] [1 3] [4 4] [5 5]]
        tree (insert (build-tree points) [1 1])]
    (is (== 2 (:dist-squared (nearest-neighbor tree [2 2]))))
    (is (= (sort
            (results-to-int-points
             (nearest-neighbor tree [2 2] 4)))
           '([1 1] [1 3] [3 1] [3 3])))
    (is (= (drop 4 (results-to-int-points
                    (nearest-neighbor tree [2 2] (inc (count points)))))
           (drop 4 (sort (conj points [1 1])))))))

;;; Mimic Neighbors-2d-Example, but build the tree entirely by using insert.
(deftest- Insert-Build-2d-Example
  (let [points [[8 8] [3 1] [1 1] [6 6] [7 7] [3 3] [1 3] [4 4] [5 5]]
        tree (reduce insert nil points)]
    (is (== 2 (:dist-squared (nearest-neighbor tree [2 2]))))
    (is (= (sort
            (results-to-int-points (nearest-neighbor tree [2 2] 4)))
           '([1 1] [1 3] [3 1] [3 3])))
    (is (= (drop 4 (results-to-int-points (nearest-neighbor tree [2 2] (count points))))
           (drop 4 (sort points))))))

;;; Mimic Neighbors-4d-Example, but build the tree half by build-tree and
;;; half by insert.
(deftest- Insert-4d-Example
  (let [points (vec (map #(list (Math/pow Math/PI (/ % 2))
                                (Math/pow Math/PI %)
                                (Math/sqrt (* % % Math/E))
                                (Math/pow Math/E %))
                         (range 1 4000)))
        median (quot (count points) 2)
        half-tree (build-tree (subvec points (inc median)))
        tree (reduce insert
                     half-tree
                     (subvec points 0 median))]
    (is (= (first points)
           (:point (nearest-neighbor tree [0.1 0.2 0.3 0.4]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deletion tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Test find-min in various dimensions
(deftest- Test-find-min
  (let [points [[0.0 1.4 1.9 3.4]
                [123 0.1 4.3 6.6]
                [0.1 0.2 0.3 0.4]
                [5.0 6.0 7.0 0.1]
                [0.2 0.3 0.4 0.5]
                [1.8 1.9 101 1.5]
                [0.2 0.3 0.4 0.5]
                [0.3 0.4 0.5 0.6]
                [0.4 0.5 0.6 0.7]
                [0.5 0.6 0.7 0.8]
                [0.6 0.7 0.8 0.9]
                [0.7 0.8 0.9 1.0]
                [1.0 2.0 3.0 4.0]
                [999 999 999 999]]
        tree (build-tree points)]
    (doall
     (for [n (range (count (first points)))]
       (is (= (map double (nth points n))
              (find-min tree n)))))))

(deftest- Test-delete-root
  (let [tree
        (Node.
         (Node.
          (Node.
           nil
           (Node. nil nil (ds 20 20))
           (ds 10 35))
          nil
          (ds 20 45))
         (Node.
          (Node.
           (Node.
            (Node.
             (Node. nil nil (ds 60 10))
             nil
             (ds 70 20))
            nil
            (ds 50 30))
           (Node. nil nil (ds 90 60))
           (ds 80 40))
          nil
          (ds 60 80))
         (ds 35 60))]
    (is (= (legible-tree
            (delete tree [35 60]))
           '([50 30]
               ([20 45]
                  ([10 35] nil ([20 20]))
                  nil)
               ([60 80]
                  ([80 40]
                     ([60 10] nil ([70 20]))
                     ([90 60]))
                  nil))))))

(deftest- Test-delete-same-x
  (let [ points [[0 0] [0 0.5] [0 1] [1 1] [1 0]]
        tree (kdtree/build-tree points)
        tree-del-00 (kdtree/delete tree [0 0])]
    (is (= (map double [0 0.5])
           (:point (kdtree/nearest-neighbor tree-del-00 [0 0]))))))


(deftest- Find-Min-Retains-Metadata
  (let [tree (->> [[0 1 2] [2 3 0] [3 3 3] [4 4 4] [4 0 2]]
                  (map #(map double %))
                  (map #(with-meta % {:value %}))
                  (build-tree))]
    (doseq [dimension (range 3)]
      (let [min (find-min tree dimension)]
        (is (= min (:value (meta min))))))))


(deftest- Retains-Metadata
  (let [metadata {:arbitrary "Data!"}
        tree (build-tree [[1 11] [2 5] (with-meta [4 8] metadata) [6 4] [5 0] [7 9] [8 2]])]
    (is (= (nearest-neighbor tree [3 9])
           (kdtree/Result. [4.0 8.0] 2.0)))
    (is (= (meta (nearest-neighbor tree [3 9]))
           metadata))))

(deftest- Delete-Retains-Metadata
  ;; create a tree, delete nothing, assert that all metas are non-null
  (is (every?
       (comp not nil?)
       (-> (for [i (range 5)]
             (with-meta [i i] {:value i}))
           (build-tree)
           (delete [0 0])
           (nearest-neighbor [0 0] 4)
           (->> (map meta)))))
  ;; create a tree and where each point has meta equals to itself
  ;; randomly delete points and make sure all nodes have correct metadata
  (letfn [(correct-meta? [{:keys [left right value] :as node}]
            (if (nil? node) true
                (and (= (vec value) (:value (meta node)))
                     (correct-meta? left)
                     (correct-meta? right))))]
    (let [points (->> (for [i (range 10) j (range 10)] [(double i) (double j)])
                      (map #(with-meta % {:value %}))
                      shuffle)
          tree (build-tree points)]
      ;; Just to be sure we have correct meta BEFORE test
      (is (correct-meta? tree))
      (is (->> (reductions delete tree points)
               (map correct-meta?)
               (every? true?))))))

(deftest- Insert-Retains-Metadata
  ;; create a tree, insert, assert that all metas are non-null
  (is (every?
       (comp not nil?)
       (-> (for [i (range 5)]
             (with-meta [i i] {:value i}))
           (build-tree)
           (insert (with-meta [0 0] {:value 0}))
           (nearest-neighbor [0 0] 5)
           (->> (map meta))))))


(def inside-interval? (ns-resolve 'kdtree 'inside-interval?))

(deftest- Inside-Interval?
  (let [interval (map double-array [[0 10] [5 10]])]
    (are [point expected] (= expected (inside-interval? interval (double-array point)))
         [5 7] true
         [10 10] true
         [0 0] false
         [20 7] false
         [0 5] true
         [9 6] true)))

(deftest- Interval-Search
  (let [points (for [x (range 11) y (range 11)] [(double x) (double y)])
        tree (build-tree points)
        naive-interval-search (fn [interval]
                                (->> (map double-array points)
                                     (filter #(inside-interval? (map double-array interval) %))
                                     (map vec)
                                     set))]
    (are [interval] (= (naive-interval-search interval)
                       (set (interval-search tree interval)))
         [[0 11] [0 11]]
         [[20 30] [0 11]]
         [[0 0] [0 0]]
         [[5 7] [5 7]]
         [[-100 100] [-100 100]]
         [[5 5] [0 10]])))

(deftest- Interval-Search-Retains-Metadata
  (let [tree (->> (for [x (range 5) y (range 5)] [(double x) (double y)])
                  (map #(with-meta % {:value %}))
                  build-tree)
        valid-meta? (fn [points]
                      (every? #(= (:value (meta %)) %) points))]
    (are [interval] (valid-meta? (interval-search tree interval))
         [[0 5] [0 5]]
         [[2 2] [2 2]]
         [[2 2] [1 4]]
         [[20 30] [0 10]])))
