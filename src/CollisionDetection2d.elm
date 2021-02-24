module CollisionDetection2d exposing
    ( Container
    , Constraint
    , DataStructure, default, fixedDepthTree, merelyDict
    , empty, fromDict
    , detect, mapWithDetection
    , get, insert, remove, size
    , toDict, map, foldl, foldr
    )

{-| Collision detection library for 2D objects.


# Containers

@docs Container


# Constraints

@docs Constraint


# Data Structures

@docs DataStructure, default, fixedDepthTree, merelyDict


# Constructors

@docs empty, fromDict


# Collision detection

@docs detect, mapWithDetection


# Query

@docs get, insert, remove, size


# Transform

@docs toDict, map, foldl, foldr

-}

import CollisionDetection2d.Internal.FixedDepthTree2d as FixedDepthTree2d
import CollisionDetection2d.Internal.MerelyDict as MerelyDict
import Dict exposing (Dict)



-- Containers


{-| Representation of a container that stores multiple triads of a key, object, and objects' bounding box.
The type of key must be comparable.
-}
type Container comparable object
    = FixedDepthTree2d (FixedDepthTree2d.Container comparable object)
    | MerelyDict (MerelyDict.Container comparable object)



-- Constraints


{-| These parameters are used to evaluate which area an object belongs to.
For example, if you want to handle circular object `{ x: Float, y: Float, r: Float }` on a canvas 640x480,
`Constraints` would be as shown:

    { range = { minX = 0, minY = 0, maxX = 639, maxY = 479 }
    , boundingBox = \c -> { minX = c.x - c.r, minY = c.y - c.r, maxX = c.x + c.r, maxY = c.y + c.r }
    , isCollided = \c0 c1 -> getLength ( c0.x, c0.y ) ( c1.x, c1.y ) |> lessThan (c0.r + c1.r)
    }

-}
type alias Constraint object =
    { range : { minX : Float, maxX : Float, minY : Float, maxY : Float }
    , boundingBox : object -> { minX : Float, maxX : Float, minY : Float, maxY : Float }
    , isCollided : object -> object -> Bool
    }



-- Data Structures


{-| Represents data structure used for space partitioning.
-}
type DataStructure
    = DefaultStructure
    | FixedDepthTree2dStructure { depth : Int }
    | MerelyDictStructure


{-| Default data structure.
-}
default : DataStructure
default =
    DefaultStructure


{-| Use quadtree as a data structure. Tree-depth is fixed from start to end.
-}
fixedDepthTree : { depth : Int } -> DataStructure
fixedDepthTree p =
    FixedDepthTree2dStructure p


{-| Use `Dict` as a data structure.
-}
merelyDict : DataStructure
merelyDict =
    MerelyDictStructure



-- Constructors


{-| Create an empty container.
-}
empty : DataStructure -> Constraint object -> Container comparable object
empty dataStructure constraint =
    case dataStructure of
        DefaultStructure ->
            empty (FixedDepthTree2dStructure { depth = FixedDepthTree2d.defaultDepth }) constraint

        FixedDepthTree2dStructure { depth } ->
            FixedDepthTree2d (FixedDepthTree2d.empty depth constraint)

        MerelyDictStructure ->
            MerelyDict (MerelyDict.empty { isCollided = constraint.isCollided })


{-| Convert a dictionary into a container.
-}
fromDict : DataStructure -> Constraint object -> Dict comparable object -> Container comparable object
fromDict dataStructure constraint d =
    case dataStructure of
        DefaultStructure ->
            fromDict (FixedDepthTree2dStructure { depth = FixedDepthTree2d.defaultDepth }) constraint d

        FixedDepthTree2dStructure { depth } ->
            FixedDepthTree2d (FixedDepthTree2d.fromDict depth constraint d)

        MerelyDictStructure ->
            MerelyDict (MerelyDict.empty { isCollided = constraint.isCollided })



-- Collision detection


{-| Detect collisions.
-}
detect : Container comparable object -> Dict comparable { object : object, collidedWith : List ( comparable, object ) }
detect cont =
    case cont of
        FixedDepthTree2d container ->
            FixedDepthTree2d.detect container

        MerelyDict container ->
            MerelyDict.detect container


{-| Map with collision results.
For example, if you want to swap a color of object with the one of collided, the code would be as shown.

    swapColor : List ( key, obj ) -> key -> obj -> obj
    swapColor collidedObjs key obj =
        collidedObjs |> List.head |> Maybe.map (\cObj -> { obj | color = cObj }) |> Maybe.withDefault obj

    swapped =
        mapWithDetection swapColor container

-}
mapWithDetection :
    (List ( comparable, object ) -> comparable -> object -> object)
    -> Container comparable object
    -> Container comparable object
mapWithDetection f cont =
    case cont of
        FixedDepthTree2d container ->
            FixedDepthTree2d.mapWithDetection f container |> FixedDepthTree2d

        MerelyDict container ->
            MerelyDict.mapWithDetection f container |> MerelyDict



-- Query


{-| Insert key and object to a container.
-}
insert : comparable -> object -> Container comparable object -> Container comparable object
insert key object cont =
    case cont of
        FixedDepthTree2d container ->
            FixedDepthTree2d.insert key object container |> FixedDepthTree2d

        MerelyDict container ->
            MerelyDict.insert key object container |> MerelyDict


{-| Remove an object from a container.
-}
remove : comparable -> Container comparable object -> Container comparable object
remove key cont =
    case cont of
        FixedDepthTree2d container ->
            FixedDepthTree2d.remove key container |> FixedDepthTree2d

        MerelyDict container ->
            MerelyDict.remove key container |> MerelyDict


{-| Get an object from a container if it exists.
-}
get : comparable -> Container comparable object -> Maybe object
get key cont =
    case cont of
        FixedDepthTree2d container ->
            FixedDepthTree2d.get key container

        MerelyDict container ->
            MerelyDict.get key container


{-| Returns number of objects a container stores.
-}
size : Container comparable object -> Int
size cont =
    case cont of
        FixedDepthTree2d container ->
            FixedDepthTree2d.size container

        MerelyDict container ->
            MerelyDict.size container



-- transform


{-| Convert a container to a `Dict`
-}
toDict :
    Container comparable object
    -> Dict comparable object
toDict cont =
    case cont of
        FixedDepthTree2d container ->
            FixedDepthTree2d.toDict container

        MerelyDict container ->
            MerelyDict.toDict container


{-| Apply functions to all objects in a container.
-}
map : (comparable -> object -> object) -> Container comparable object -> Container comparable object
map f cont =
    case cont of
        FixedDepthTree2d container ->
            FixedDepthTree2d.map f container |> FixedDepthTree2d

        MerelyDict container ->
            MerelyDict.map f container |> MerelyDict


{-| Foldl. It is equivalent to `toDict |> Dict.foldl`
-}
foldl : (comparable -> object -> a -> a) -> a -> Container comparable object -> a
foldl f r cont =
    toDict cont |> Dict.foldl f r


{-| Foldr. It is equivalent to `toDict |> Dict.foldr`
-}
foldr : (comparable -> object -> a -> a) -> a -> Container comparable object -> a
foldr f r cont =
    toDict cont |> Dict.foldr f r
