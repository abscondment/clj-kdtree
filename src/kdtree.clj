;; Copyright (C) 2009-2011 Brendan Ribera. All rights reserved.
;; Distributed under the MIT License; see the file LICENSE
;; at the root of this distribution.
(ns kdtree)

(defrecord Node [left right #^doubles value #^long depth])
(defrecord Result [point dist-squared])

(defn- dist-squared [a b]
  "Compute the K-dimensional distance between two points"
  (reduce + (for [i (range (count a))]
              (let [v (- (nth a i) (nth b i))]
                (* v v)))))

(defn build-tree
  "Construct a Kd-tree from points. Assumes all
points are of the same dimension."
  ([points]
     (build-tree points 0))
  ([points depth]
     (let [point-count (count points)]
       (if (= 0 point-count) nil
           (let [k (count (nth points 0))
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
                 left-tree (build-tree
                            (subvec points 0 split-point)
                            (inc depth))
                 right-tree (build-tree
                             (subvec points (inc split-point))
                             (inc depth))]
             (Node. left-tree
                    right-tree
                    (into-array Double/TYPE (nth points split-point))
                    depth
                    (meta (nth points split-point))
                    nil))))))

(defn insert
  "Adds a point to an existing tree."
  ([tree point] (insert tree point 0))
  ([tree point depth]
     (let [k (count point)
           dimension (mod depth k)]
      (if (nil? tree)
        (Node. nil nil (into-array Double/TYPE point) depth)
        (if (< (nth point dimension) (nth (:value tree) dimension))
          (Node.
           (insert (:left tree) point (inc depth))
           (:right tree)
           (:value tree)
           (:depth tree))
          (Node.
           (:left tree)
           (insert (:right tree) point (inc depth))
           (:value tree)
           (:depth tree)))))))

(defn find-min
  "Locate the point with the smallest value in a given dimension.
Used internally by the delete function. Runs in O(√n) time for a
balanced tree."
  ([tree dimension] (find-min tree dimension 0))
  ([tree dimension depth]
     (if (identity tree)
       (let [k (count (:value tree))]
         (if (= dimension (mod depth k))
           ;; if we're at the dimension of interest, follow the left branch or
           ;; take the value - left is always smaller in the currend dimension
           (if (nil? (:left tree))
             (:value tree)
             (find-min (:left tree) dimension (inc depth)))
           ;; otherwise, compare min of self & children
           (first
            (sort-by #(nth % dimension)
             (filter identity
              (list (:value tree)
                    (find-min (:left tree) dimension (inc depth))
                    (find-min (:right tree) dimension (inc depth)))))))))))

(defn delete
  "Delete value at the given point. Runs in O(log n) time for a balanced tree."
  ([tree point] (delete tree point 0))
  ([tree point depth]
     (if (identity tree)
       (let [k (count (:value tree))
             dimension (mod depth k)]
         (cond
          ;; point is to the left
          (< (nth point dimension)
             (nth (:value tree) dimension))
          (Node.
           (delete (:left tree) point (inc depth))
           (:right tree)
           (:value tree)
           (:depth tree))
          
          ;; point is to the right
          (and
            (>= (nth point dimension)
                (nth (:value tree) dimension))
            (not= (map double point) (seq (:value tree))))
          (Node.
           (:left tree)
           (delete (:right tree) point (inc depth))
           (:value tree)
           (:depth tree))

          ;; point is here... three cases:
          
          ;; leaf node - delete case, so return nil
          (= nil (:left tree) (:right tree))
          nil

          ;; right is not null.
          (not (nil? (:right tree)))
          (let [value (find-min (:right tree) dimension (inc depth))]
            (Node.
             (:left tree)
             (delete (:right tree) value (inc depth))
             value
             (:depth tree)))

          ;; right is null, left must not be.
          true
          (let [value (find-min (:left tree) dimension (inc depth))]
            (Node.
             nil
             (delete (:left tree) value (inc depth))
             value
             (:depth tree))))))))


(defn nearest-neighbor
  "Compute n nearest neighbors for a point. If n is
omitted, the result is the nearest neighbor;
otherwise, the result is a list of length n."
  ([tree point] (first (nearest-neighbor tree point 1 0 nil)))
  ([tree point n] (nearest-neighbor tree point n 0 nil))
  ([tree point n depth best]
     (if ;; Empty tree? The best list is unchanged.
         (nil? tree) best

         ;; Otherwise, recurse!
         (take n
          (sort-by :dist-squared
           (let [dimension (mod depth (count point))
                 dim-dist (- (nth point dimension)
                             (nth (:value tree) dimension))
                 search-order (if (> dim-dist 0)
                                (list :right :left)
                                (list :left :right))

                 ;; Compute best list for the near-side of the search order
                 best-near (nearest-neighbor
                            ((first search-order) tree)
                            point
                            n
                            (inc depth)
                            (cons
                             (Result. (vec (:value tree))
                                      (dist-squared (:value tree) point)
                                      (meta tree)
                                      nil)
                             best))]
             
             ;; If the square distance of our search node to point in the
             ;; current dimension is still better than the *worst* of the near-
             ;; side best list, there may be a better solution on the far
             ;; side. Compute & combine with near-side solutions.
             (if (< (* dim-dist dim-dist) (:dist-squared (last best-near)))
               (concat best-near
                       (nearest-neighbor
                        ((last search-order) tree) point n (inc depth) nil))
               best-near)))))))
