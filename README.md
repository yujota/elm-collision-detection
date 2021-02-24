# elm-collision-detection

This Elm library provides Quadtree space partitioning data structure for efficient collision detection on a 2D system.

[Live demo](https://yujota.github.io/elm-collision-detection/)

![example-demo-image](https://github.com/yujota/elm-collision-detection/blob/master/images/example-demo.gif?raw=true)

## Overview

Let's suppose there are six objects and you want to detect collisions. 
On a naive approach which is the method to check all possible combinations, it needs to inspect `6C2 = 15` patterns.  
As the number of objects increases, this process takes much longer(`nC2 ≒ n^2`). 
So, it is necessary to reduce the number of checks.

One solution is that check objects only nearby.
For example, space partitioning.

In the figure below there is a purple circle. 
In a naive approach, we have to check all the other 5 objects to find collisions with the purple one
but these triangles are far away from it. 
 
![overview](https://github.com/yujota/elm-collision-detection/blob/master/images/overview.png?raw=true)
![overview-with-grid](https://github.com/yujota/elm-collision-detection/blob/master/images/overview-with-grid.png?raw=true)

Instead, we divide 2D space into small regions and check objects in each region.
This is how space partitioning works and this library adopts Quadtree as a data structure to store objects. 
The implementation of Quadtree is capsulated and you don't have to aware of it.

(If you're an expert on this topic and find some problems in this code, please pull-request :D)


## Performance

This module has not completely optimized yet, 
but it seems that `detectCollisions` for QuadTree container is faster than one for the naive approach. 
The benchmark code is available on [elm-collision-detection/benchmarks/Benchmarks/DetectCollisionsWithSmallRectangle.elm](https://github.com/yujota/elm-collision-detection/blob/master/benchmarks/Benchmarks/DetectCollisionsWithSmallRectangle.elm).

![performance-result](https://github.com/yujota/elm-collision-detection/blob/master/images/performance-result.png?raw=true)
