;;; Copyright (C) 2009 Brendan Ribera. All rights reserved.
;;; Distributed under the MIT License; see the file LICENSE
;;; at the root of this distribution.
(ns kdtree)

(defn dist-squared [a b]
  "Compute the K-dimensional distance between two points"
  (reduce + (for [i (range (count a))]
              (let [v (- (nth a i) (nth b i))]
                (* v v)))))

;;; Simple accessors
(defn- node-value [n] (first n))
(defn- node-left  [n] (first (rest n)))
(defn- node-right [n] (first (rest (rest n))))

(defn build-tree
  "Construct a Kd-tree from points. Assumes all points are of the same dimension."
  ([points]
     (build-tree points 0))
  ([points depth]
     (let [point-count (count points)]
       (if (= 0 point-count) nil
           (let [k (count (nth points 0))
                 dimension (mod depth k)
                 points (vec (sort-by #(nth % dimension) points))
                 median (quot point-count 2)
                 left-tree (build-tree (subvec points 0 median) (inc depth))
                 right-tree (build-tree (subvec points (inc median)) (inc depth))]
             (if (or left-tree right-tree)
               (list (nth points median) left-tree right-tree)
               (list (nth points median))))))))

(defn nearest-neighbor
  "Compute n nearest neighbors for a point. If n is omitted, the result is the nearest neighbor; otherwise, the result is a list of length n."
  ([tree point] (first (nearest-neighbor tree point 1 0 nil)))
  ([tree point n] (nearest-neighbor tree point n 0 nil))
  ([tree point n depth best]
     (if ;;; Empty tree? The best list is unchanged.
         (nil? tree) best

         ;;; Otherwise, recurse!
         (take n
          (sort-by :dist-squared
           (let [dimension (mod depth (count point))
                 dim-dist (- (nth point dimension) (nth (node-value tree) dimension))
                 search-order (if (> dim-dist 0)
                                (list node-right node-left)
                                (list node-left node-right))

                 ;;; Compute a best list for the near-side of the search order
                 best-near
                 (nearest-neighbor ((first search-order) tree)
                                   point
                                   n
                                   (inc depth)
                                   (cons {:point (node-value tree)
                                          :dist-squared (dist-squared
                                                         (node-value tree)
                                                         point)}
                                         best))]
             
             ;;; If the square distance of our search node to point in the
             ;;; current dimension is still better than the *worst* of the near-
             ;;; side best list, there may be a better solution on the far
             ;;; side. Compute & combine with near-side solutions.
             (if (< (* dim-dist dim-dist) (:dist-squared (last best-near)))
               (concat best-near
                       (nearest-neighbor
                        ((last search-order) tree) point n (inc depth) nil))
               best-near)))))))
