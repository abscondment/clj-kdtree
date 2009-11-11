# clj-kdtree

## Overview

A Kd-tree is a special type of binary tree that partitions points in a
k-dimensional space. It can be used for efficient nearest-neighbor
searches.

For more detail, refer to [Wikipedia on Kd-trees](http://en.wikipedia.org/wiki/Kd-tree).

## Usage

   ;;; Use a kdtree to find neighbors for one point given a set of points.
   (let [points [[8 8] [3 1] [6 6] [7 7] [1 3] [4 4] [5 5]]
         tree (build-tree points)
         neighbors (nearest-neighbor tree [2 2] 4)]
         (println "Tree:" tree
                  "\n\nFour points closest to [2 2]:\n"
                  neighbors))

## License

Copyright (C) 2009 Brendan Ribera. All rights reserved.

Distributed under the MIT License; see the file LICENSE at the root of
this distribution.
