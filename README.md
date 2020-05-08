# elm-collision-detection

This Elm library provides Quadtree space partitioning data structure for efficient collision detection on a 2D system.

[Live demo](https://yujota.github.io/elm-collision-detection/)

[Code of live demo](https://github.com/yujota/elm-collision-detection/examples/Example.elm)

![example-demo-image](https://github.com/yujota/elm-collision-detection/images/example-demo.gif)


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
 
![overview](https://github.com/yujota/elm-collision-detection/images/overview.png)
![overview-with-grid](https://github.com/yujota/elm-collision-detection/images/overview-with-grid.png)
 
Instead, we divide 2D space into small regions and check objects in each region.
This is how space partitioning works and this library adopts Quadtree as a data structure to store objects. 
The implementation of Quadtree is capsulated and you don't have to aware of it.

(If you're an expert on this topic and find some problems in this code, please pull-request :D)


## How to use

You can use any type for an object and its bounding box to detect collisions, 
and this library nicely works with `ianmackenzi/elm-geometry` which is a great library to handle geometric data in Elm. 
For example, here is `MyObject` declaration which has two variants; rectangle and circle.

```elm
import Circle2d exposing (Circle2d)
import Rectangle2d exposing (Rectangle2d)
import Pixels exposing (Pixels, inPixels)
import Point2d


type MyObject 
    = RectObject (Rectangle2d Pixels MyCoordinates)
    | CircleObject (Circle2d Pixels MyCoordinates)
```

You need to define a function that checks two objects have collided or not.
Also, it is required to have those three functions; 

 - `extrema : boundingBox -> { minX : Float, minY : Float, maxX: Float, maxY : Float}`
 - `intersects : boundingBox -> boundingBox -> Bool`
 - `getBoundingBox : object -> boundingBox`

```elm
check : MyObject -> MyObject -> Bool
check objA objB = 
    case (objA, objB) of
        (CircleObject circleA, CircleObject circleB) ->
                Point2d.equalWithin
                    (Quantity.plus (Circle2d.radius circleA) (Circle2d.radius circleB))
                    Circle2d.centerPoint circleA
                    Circle2d.centerPoint circleB

        (RectObject rectA, RectObject rectB) ->
                ...


        (RectObject rect, CircleObject circle) ->
                ...


        (CircleObject _, RectObject _) ->
                check objB objA


extrema : BoundingBox2d Pixels MyCoordinates -> { minX : Float, minX : Float, maxX : Float, maxY : Float }
extrema =
    BoundingBox2d.extrema
        >> (\r -> { minX = inPixels r.minX, minY = inPixels r.minY, maxX = inPixels r.maxX, maxY = inPixels r.maxY })


intersects : BoundingBox2d Pixels MyCoordinates -> BoundingBox2d Pixels MyCoordinates -> Bool
intersects = 
    BoundingBox2d.intersects


getBoundingBox : MyObject -> BoundingBox2d Pixels MyCoordinates
getBoundingBox obj = 
    case obj of
        RectObject rect ->
            Rectangle2d.boundingBox circle

        CircleObject circle ->
            Circle2d.boundingBox circle
```

Then,  you can create a container that uses QuadTree and register objects to it. 
Finally, collided objects are acquired with `CollisionDetection2d.detectCollisions` function.

```elm
container : CollisionDetection2d String MyObject (BoundingBox2d Pixels MyCoordinates)
container = CollisionDetection2d.quadTree 
    { extrema = extrema
    , intersects = intersects
    , getBoundingBox = getBoundingBox
    , boundary = { minX = 0, minY = 0, maxX = 200, maxY = 200 }
    }
    |> CollisionDetection2d.insert "sampleA" sampleA
    |> CollisionDetection2d.insert "sampleB" sampleB
    |> CollisionDetection2d.insert "sampleC" sampleC



collidedObjects : List ( { key : String, object : MyObject }, { key : String, object : MyObject } )
collidedObjects =
    CollisionDetection2d.detectCollisions check container
```

## Performance

This module has not completely optimized yet, 
but it seems that `detectCollisions` for QuadTree container is faster than one for the naive approach. 
The benchmark code is available on [].

![performance-result](https://github.com/yujota/elm-collision-detection/images/performance-result.png)
