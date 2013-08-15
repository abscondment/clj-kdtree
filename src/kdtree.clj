;; Copyright (C) 2009-2011 Brendan Ribera. All rights reserved.
;; Distributed under the MIT License; see the file LICENSE
;; at the root of this distribution.
(ns kdtree)

(defrecord Node [left right value])
(defrecord Result [point ^double dist-squared])


(defn- dist-squared [^doubles a ^doubles b]
  "Compute the K-dimensional distance between two points"
  (loop [res (double 0.0)
         ind (long 0)]
    (if (== ind (alength a))
      res
      (let [v (- (aget a ind) (aget b ind))]
        (recur (+ res (* v v)) (inc ind))))))


(defn- build-tree-internal [points depth]
  (if (empty? points) nil
      (let [point-count (count points)
            k (count (nth points 0))
            dimension (mod depth k)
            points (vec (sort-by #(nth % dimension) points))
            median (quot point-count 2)
            split-point (loop [n median]
                          (cond
                           (= 0 n) n
                           (= (nth (points n) dimension)
                              (nth (points (dec n)) dimension))
                           (recur (dec n))
                           :else n))
            left-tree (build-tree-internal
                       (subvec points 0 split-point)
                       (inc depth))
            right-tree (build-tree-internal
                        (subvec points (inc split-point))
                        (inc depth))]
        (Node. left-tree
               right-tree
               (double-array (nth points split-point))
               (meta (nth points split-point))
               nil))))

(defn build-tree
  "Construct a Kd-tree from points. Assumes all
points are of the same dimension."
  [points]
  (build-tree-internal points 0))



(defn- insert-internal [tree ^doubles point depth point-meta]
  (let [k (alength point)
        dimension (mod depth k)]
    (if (nil? tree)
      (Node. nil nil point point-meta nil)
      (let [^doubles value (:value tree)
            go-to-left? (< (aget point dimension)
                           (aget value dimension))
            left (if go-to-left?
                   (insert-internal (:left tree) point (inc depth) point-meta)
                   (:left tree))
            right (if-not go-to-left?
                    (insert-internal (:right tree) point (inc depth) point-meta)
                    (:right tree))]
        (Node. left right value (meta tree) nil)))))

(defn insert
  "Adds a point to an existing tree."
  [tree point]
  (insert-internal tree (double-array point) 0 (meta point)))



(defn- find-min-internal [tree dimension depth]
  (when tree
    (let [k (count (:value tree))
          min-node (fn [node1 node2]
                     (let [^doubles value1 (:value node1)
                           ^doubles value2 (:value node2)]
                       (if (or (nil? value2)
                               (< (aget value1 dimension)
                                  (aget value2 dimension)))
                         node1 node2)))]
      (if (= dimension (mod depth k))
        ;; if we're at the dimension of interest, follow the left branch or
        ;; take the value - left is always smaller in the current dimension
        (if (:left tree)
          (recur (:left tree) dimension (inc depth))
          tree)
        ;; otherwise, compare min of self & children
        (-> tree
            (min-node (find-min-internal (:left tree) dimension (inc depth)))
            (min-node (find-min-internal (:right tree) dimension (inc depth))))))))

(defn find-min
  "Locate the point with the smallest value in a given dimension.
Used internally by the delete function. Runs in O(√n) time for a
balanced tree."
  [tree dimension]
  (let [res (find-min-internal tree dimension 0)]
      (with-meta (vec (:value res))
                 (meta res))))



(defn- points=
  "Compares 2 points represented by arrays of doubles and return true if they are equal"
  [^doubles a ^doubles b]
  (loop [i 0]
    (cond (== i (alength a)) true
          (== (aget a i) (aget b i)) (recur (inc i))
          :else false)))

(defn- delete-internal
  [tree ^doubles point depth]
  (when tree
    (let [^doubles value (:value tree)
          k (alength value)
          dimension (mod depth k)]
      (cond

       ;; point is not here
       (not (points= point value))
       (let [go-to-left? (< (aget point dimension) (aget value dimension))
             left (if go-to-left?
                    (delete-internal (:left tree) point (inc depth))
                    (:left tree))
             right (if-not go-to-left?
                     (delete-internal (:right tree) point (inc depth))
                     (:right tree))]
         (Node. left right (:value tree) (meta tree) nil))

       ;; point is here... three cases:

       ;; right is not null.
       (:right tree)
       (let [min (find-min-internal (:right tree) dimension (inc depth))]
         (Node.
          (:left tree)
          (delete-internal (:right tree) (:value min) (inc depth))
          (:value min)
          (meta min)
          nil))

       ;; left if not null
       (:left tree)
       (let [min (find-min-internal (:left tree) dimension (inc depth))]
         (Node.
          nil
          (delete-internal (:left tree) (:value min) (inc depth))
          (:value min)
          (meta min)
          nil))

       ;; both left and right are null
       :default nil))))

(defn delete
  "Delete value at the given point. Runs in O(log n) time for a balanced tree."
  [tree point]
  (delete-internal tree (double-array point) 0))



(defn- insert-sorted!
  "Inserts value to sorted transient vector. Vector will not grow
bigger than n elements."
  [vec value ^long n]
  (if (and (== (count vec) n)
           (> (:dist-squared value) (:dist-squared (nth vec (dec n)))))
    vec
    (loop [ind (long 0)
           value value
           vec vec]
     (cond (= ind n) vec
           (= ind (count vec)) (conj! vec value)
           :default (let [existing (nth vec ind)]
                      (if (< (:dist-squared value)
                             (:dist-squared existing))
                        (recur (inc ind) existing (assoc! vec ind value))
                        (recur (inc ind) value vec)))))))

(defn- nearest-neighbor-internal [tree ^doubles point n dimension best]
  (if ;; Empty tree? The best list is unchanged.
         (nil? tree) best

         ;; Otherwise, recurse!
         (let [dimension (long dimension)
               next-dimension (unchecked-remainder-int (unchecked-inc dimension) (alength point))
               ^doubles v (:value tree)
               dim-dist (double (- (aget point dimension)
                                   (aget v dimension)))
               closest-semiplane ((if (> dim-dist 0.0) :right :left) tree)
               farthest-semiplane ((if (> dim-dist 0.0) :left :right) tree)
               ;; Compute best list for the near-side of the search order
               best-with-cur (insert-sorted! best (Result. v
                                                           (dist-squared v point)
                                                           (meta tree)
                                                           nil)
                                             n)
               best-near (nearest-neighbor-internal closest-semiplane point n next-dimension best-with-cur)
               worst-nearest (->> (dec (count best-near))
                                  (nth best-near)
                                  :dist-squared)]

           ;; If the square distance of our search node to point in the
           ;; current dimension is still better than the *worst* of the near-
           ;; side best list, there may be a better solution on the far
           ;; side. Compute & combine with near-side solutions.
           (if (< (* dim-dist dim-dist) worst-nearest)
             (recur farthest-semiplane point n next-dimension best-near)
             best-near))))

(defn nearest-neighbor
  "Compute n nearest neighbors for a point. If n is
omitted, the result is the nearest neighbor;
otherwise, the result is a list of length n."
  ([tree point] (first (nearest-neighbor tree point 1)))
  ([tree point n]
     (->> (transient [])
          (nearest-neighbor-internal tree (double-array point) n 0)
          (persistent!)
          (map #(update-in % [:point] vec)))))
