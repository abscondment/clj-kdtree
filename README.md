# clj-kdtree

## Overview

A Kd-tree is a special type of binary tree that partitions points in a
k-dimensional space. It can be used for efficient nearest-neighbor
searches.

For more detail, refer to [Wikipedia on Kd-trees](http://en.wikipedia.org/wiki/Kd-tree).

## Usage

    (require 'kdtree)

    (def points [[8 8] [3 1] [6 6] [7 7] [1 3] [4 4] [5 5]])
    ;; Build a kdtree from a set of points
    (def tree (kdtree/build-tree points))
    (println "Tree:" tree)
    
    ;; Use tree to find neighbors for one point given a set of points.
    (println "\n\nFour points closest to [2 2]:\n"
             (kdtree/nearest-neighbor tree [2 2] 4))

    ;; Same operation, but delete two of the closest points.
    (println "\n\nFour points with deletion:\n"
             (kdtree/nearest-neighbor
              (kdtree/delete
               (kdtree/delete tree [1 3])
               [3 1])
              [2 2] 4))

    ;; Same operation, but adding a close point.
    (println "\n\nFour points with insertion:\n"
             (kdtree/nearest-neighbor
              (kdtree/insert tree [1.5 1.5])
              [2 2] 4))

## License

Copyright (C) 2009-2010 Brendan Ribera. All rights reserved.

Distributed under the MIT License; see the file LICENSE at the root of
this distribution.
