(ns kdtree.test
  (:use [kdtree] :reload)
  (:import [kdtree Node Result])
  (:use [clojure.test]))

;; pull in private function
(def dist-squared (ns-resolve 'kdtree 'dist-squared))

(defn- legible-tree [t]
  (if (not (nil? t))
    (let [val (.value t)
          left (.left t)
          right (.right t)]
      (if (or left right)
        (list val (legible-tree left) (legible-tree right))
        (list val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Test kD distance-squared function
(deftest- Distance-Squared
  ;;; Simple 2-d point
  (is (= (dist-squared [0 0] [1 1]) 2))
  ;;; 2-d using floating points
  (is (= (dist-squared [Math/PI -1.0] [1.0 Math/PI])
         (+ (Math/pow (- Math/PI 1) 2) (Math/pow (inc Math/PI) 2))))
  ;;; Simple 5-d distance
  (is (= (dist-squared [1 1 1 1 1] [2 2 2 2 2]) 5))
  ;;; 10-d, floating point distance
  (is (= (dist-squared [0   1   2   3   4   5   6   7   8   9]
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

;;; 2d-tree example found at: http://en.wikipedia.org/wiki/Kd-tree
(deftest- Build-2d-Example-Wikipedia
  (let [tree (legible-tree
              (build-tree [[4 7] [2 3] [5 4] [9 6] [8 1] [7 2]]))]
    (is (= tree
           '([7 2]
               ([5 4] ([2 3]) ([4 7]))
               ([9 6] ([8 1]) nil))))))


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

;;; Test that search results match the naïve sort-by-distance algorithm.
(deftest- Neighbors-Sorting-Example
  (let [points (for [x (range 2 20)
                     y (range 2 20)]
                 [(* 0.95 x) (* -1.23 y)])
        cpoint [Math/PI Math/E]
        tree (build-tree points)]
    (is (= (take 5 (sort-by #(dist-squared cpoint %) points))
           (map :point (nearest-neighbor tree cpoint 5))))))

;;; Simple example based on the search animation from:
;;; http://en.wikipedia.org/wiki/Kd-tree
(deftest- Neighbors-2d-Example-Wikipedia
  (let [tree (build-tree [[1 11] [2 5] [4 8] [6 4] [5 0] [7 9] [8 2]])]
    (is (= (nearest-neighbor tree [3 9])
           (kdtree/Result. [4 8] 2)))))

;;; A simple 2d example.
(deftest- Neighbors-2d-Example
  (let [points [[8 8] [3 1] [1 1] [6 6] [7 7] [3 3] [1 3] [4 4] [5 5]]
        tree (build-tree points)]
    ;;; Confirm that the nearest hit is one of the four
    ;;; points that are root 2 away.
    (is (= 2 (:dist-squared (nearest-neighbor tree [2 2]))))
    ;; Confirm that the four nearest are our points on the 1 and 3 lines.
    (is (= (sort (map :point (nearest-neighbor tree [2 2] 4)))
           '([1 1] [1 3] [3 1] [3 3])))
    ;; Confirm that the ones after our first 4 = rest in ascending order.
    (is (= (drop 4 (map :point (nearest-neighbor tree [2 2] (count points))))
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
