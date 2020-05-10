module CollisionDetection2d exposing
    ( Container
    , quadTree
    , detectCollisions, collideWith
    , get, insert, update, remove, keys, values, size, toDict
    , map, foldl, foldr, filter
    , naive, customQuadTree
    )

{-| Collision detection library for 2D objects.


# Types

@docs Container


# Constructor

@docs quadTree


# Collision detection

@docs detectCollisions, collideWith


# `Dict` like APIs

@docs get, insert, update, remove, keys, values, size, toDict


# Transform

@docs map, foldl, foldr, filter


# For advanced usages

@docs naive, customQuadTree

-}

import Array exposing (Array)
import CollisionDetection2d.Internal.CollisionDetection2d as C
import Dict exposing (Dict)



-- Types


{-| Representation of a container that stores multiple triads of a key, object, and objects' bounding box.
The type of key must be comparable.
-}
type Container comparable object boundingBox
    = QuadTree (QuadTreeRecord comparable object boundingBox)
    | Naive
        { extrema : boundingBox -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
        , intersects : boundingBox -> boundingBox -> Bool
        , getBoundingBox : object -> boundingBox
        , objects : Dict comparable { object : object, boundingBox : boundingBox }
        }


type alias QuadTreeRecord comparable object boundingBox =
    { extrema : boundingBox -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
    , intersects : boundingBox -> boundingBox -> Bool
    , getBoundingBox : object -> boundingBox
    , keyToIndex : Dict comparable { lqt : Int, offset : Int }
    , objects : Array (Array { key : comparable, object : object, boundingBox : boundingBox })
    , depth : Int
    , unitWidth : Int
    , unitHeight : Int
    , boundary : Boundary
    , truncateX : Float -> Float
    , truncateY : Float -> Float
    }


type alias Boundary =
    { minX : Float, minY : Float, maxX : Float, maxY : Float }



-- Constructors


{-| Construct a container that uses a quadtree algorithm to detect collisions.
The field `extrema` of the first argument is a function that extracts parameters from a bounding box
to register an object to each cell of the quadtree.
The filed `boundary` represents a range of x and y coordinates.
You can use any type for both `boundingBox` and `object`.
For instance, if you use ianmackenzi/elm-geometry, the function is called like the following.

    import Circle2d exposing (Circle2d)
    import BoundingBox2d exposing (BoundingBox2d)
    import Pixels exposing (inPixels, Pixels)

    extrema =
        BoundingBox2d.extrema
        >> (\r -> { minX = inPixels r.minX, minY = inPixels r.minY, maxX = inPixels r.maxX, maxY = inPixels r.maxY })

    quadTree
        { extrema = extrema
        , intersects = BoundingBox2d.intersects
        , getBoundingBox = Circle2d.boundingBox
        , boundary = { minX = 0, minY = 0, maxX = 200, maxY = 200 }
        }
        == Container key (Circle2d Pixels SomeCoordinates) (BoundingBox2d Pixels SomeCoordinates)

-}
quadTree :
    { extrema : boundingBox -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
    , intersects : boundingBox -> boundingBox -> Bool
    , getBoundingBox : object -> boundingBox
    , boundary : { minX : Float, minY : Float, maxX : Float, maxY : Float }
    }
    -> Container comparable object boundingBox
quadTree { extrema, intersects, getBoundingBox, boundary } =
    let
        ( width, height ) =
            ( boundary.maxX - boundary.minX |> floor, boundary.maxY - boundary.minY |> floor )

        tmpUnitSize =
            256

        depth =
            (1 - logBase 2 tmpUnitSize + logBase 2 (toFloat <| max width height)) |> floor |> max 2

        ( unitWidth, unitHeight ) =
            ( width // 2 ^ (depth - 1) + 1, height // 2 ^ (depth - 1) + 1 )
    in
    { extrema = extrema
    , intersects = intersects
    , getBoundingBox = getBoundingBox
    , keyToIndex = Dict.empty
    , objects = Array.repeat ((4 ^ depth - 1) // 3) Array.empty
    , depth = depth
    , unitWidth = unitWidth
    , unitHeight = unitHeight
    , boundary = boundary
    , truncateX = max boundary.minX >> min boundary.maxX
    , truncateY = max boundary.minY >> min boundary.maxY
    }
        |> QuadTree


{-| You can specify a depth of the quadtree and width and height of most leaf-level cells.
A value of depth must be equal to or larger than 2.
-}
customQuadTree :
    { extrema : boundingBox -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
    , intersects : boundingBox -> boundingBox -> Bool
    , getBoundingBox : object -> boundingBox
    , boundary : Boundary
    , depth : Int
    , cellWidth : Int
    , cellHeight : Int
    }
    -> Container comparable object boundingBox
customQuadTree { extrema, intersects, getBoundingBox, boundary, depth, cellWidth, cellHeight } =
    if depth >= 2 then
        { extrema = extrema
        , intersects = intersects
        , getBoundingBox = getBoundingBox
        , keyToIndex = Dict.empty
        , objects = Array.repeat ((4 ^ depth - 1) // 3) Array.empty
        , depth = depth
        , unitWidth = cellWidth
        , unitHeight = cellHeight
        , boundary = boundary
        , truncateX = max boundary.minX >> min boundary.maxX
        , truncateY = max boundary.minY >> min boundary.maxY
        }
            |> QuadTree

    else
        naive { extrema = extrema, intersects = intersects, getBoundingBox = getBoundingBox }


{-| Construct a container that check all combinations of objects to detect collisions.
-}
naive :
    { extrema : boundingBox -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
    , intersects : boundingBox -> boundingBox -> Bool
    , getBoundingBox : object -> boundingBox
    }
    -> Container comparable object boundingBox
naive { extrema, intersects, getBoundingBox } =
    Naive { extrema = extrema, intersects = intersects, getBoundingBox = getBoundingBox, objects = Dict.empty }



-- Collision detection


{-| Detect collisions.
A first argument is a function to check a collision with objects.

    circleA =
        Circle2d.atPoint (Point2d.pixels 80 80) (pixels 15)

    circleB =
        Circle2d.atPoint (Point2d.pixels 60 60) (pixels 20)

    circleC =
        Circle2d.atPoint (Point2d.pixels 200 200) (pixels 5)

    container =
        emptyContainer |> insert "a" circleA |> insert "b" circleB |> insert "c" circleC

    check objA objB =
        Point2d.equalWithin
            (Quantity.plus (Circle2d.radius objA) (Circle2d.radius objB))
            Circle2d.centerPoint objA
            Circle2d.centerPoint objB

    detectCollisions check container
            == [ ({ key = "a", object = circleA }, { key = "b", object = circleB } ]

-}
detectCollisions :
    (object -> object -> Bool)
    -> Container comparable object boundingBox
    -> List ( { key : comparable, object : object }, { key : comparable, object : object } )
detectCollisions checkCollision cont =
    let
        check intersects itemA itemB =
            if intersects itemA.boundingBox itemB.boundingBox then
                -- MEMO: We suppose that execution of given checkCollision takes much longer time than intersects
                checkCollision itemA.object itemB.object

            else
                False
    in
    case cont of
        QuadTree container ->
            let
                collect =
                    quadTreeDetectCollisionsForNode
                        { check = check container.intersects
                        , depth = container.depth
                        , objects = container.objects
                        }
            in
            Array.foldl collect { lqtIndex = 0, result = [] } container.objects
                |> .result

        Naive container ->
            naiveDetectCollisionsLoop (check container.intersects) (Dict.toList container.objects) []


quadTreeDetectCollisionsForNode :
    { check :
        { key : comparable, object : object, boundingBox : boundingBox }
        -> { key : comparable, object : object, boundingBox : boundingBox }
        -> Bool
    , objects : Array (Array { key : comparable, object : object, boundingBox : boundingBox })
    , depth : Int
    }
    -> Array { key : comparable, object : object, boundingBox : boundingBox }
    -> { lqtIndex : Int, result : List ( { key : comparable, object : object }, { key : comparable, object : object } ) }
    -> { lqtIndex : Int, result : List ( { key : comparable, object : object }, { key : comparable, object : object } ) }
quadTreeDetectCollisionsForNode opt items { lqtIndex, result } =
    let
        parentToRoot =
            C.parentToRootLevelLqtIndices { depth = opt.depth } lqtIndex
                |> List.foldr
                    (\i ary -> Array.append ary (Maybe.withDefault Array.empty (Array.get i opt.objects)))
                    Array.empty
    in
    { lqtIndex = lqtIndex + 1
    , result = quadTreeDetectCollisionsForNodeLoop opt.check items parentToRoot result
    }


quadTreeDetectCollisionsForNodeLoop :
    ({ key : comparable, object : object, boundingBox : boundingBox }
     -> { key : comparable, object : object, boundingBox : boundingBox }
     -> Bool
    )
    -> Array { key : comparable, object : object, boundingBox : boundingBox }
    -> Array { key : comparable, object : object, boundingBox : boundingBox }
    -> List ( { key : comparable, object : object }, { key : comparable, object : object } )
    -> List ( { key : comparable, object : object }, { key : comparable, object : object } )
quadTreeDetectCollisionsForNodeLoop check items parentToRootItems result =
    let
        collect itm1 itm2 rlt =
            if check itm1 itm2 then
                ( { key = itm1.key, object = itm1.object }, { key = itm2.key, object = itm1.object } ) :: rlt

            else
                rlt

        remains =
            Array.slice 1 (Array.length items) items
    in
    case Array.get 0 items of
        Just item ->
            Array.foldr (collect item) result (Array.append remains parentToRootItems)
                |> quadTreeDetectCollisionsForNodeLoop check remains parentToRootItems

        Nothing ->
            result


naiveDetectCollisionsLoop :
    ({ object : object, boundingBox : boundingBox } -> { object : object, boundingBox : boundingBox } -> Bool)
    -> List ( comparable, { object : object, boundingBox : boundingBox } )
    -> List ( { key : comparable, object : object }, { key : comparable, object : object } )
    -> List ( { key : comparable, object : object }, { key : comparable, object : object } )
naiveDetectCollisionsLoop check l result =
    case l of
        ( k, item ) :: remaining ->
            List.filterMap (detectCollisionsLoopFilter check ( k, item )) remaining
                ++ result
                |> naiveDetectCollisionsLoop check remaining

        [] ->
            result


detectCollisionsLoopFilter :
    ({ object : object, boundingBox : boundingBox } -> { object : object, boundingBox : boundingBox } -> Bool)
    -> ( comparable, { object : object, boundingBox : boundingBox } )
    -> ( comparable, { object : object, boundingBox : boundingBox } )
    -> Maybe ( { key : comparable, object : object }, { key : comparable, object : object } )
detectCollisionsLoopFilter check ( keyA, itemA ) ( keyB, itemB ) =
    if check itemA itemB then
        Just ( { key = keyA, object = itemA.object }, { key = keyB, object = itemB.object } )

    else
        Nothing


{-| Return pairs of key and object which satisfy conditions
that an objects' bounding box share some points with a given bounding box as a second argument
and a result of a first argument is `True`.

    circleA =
        Circle2d.atPoint (Point2d.pixels 80 80) (pixels 15)

    circleB =
        Circle2d.atPoint (Point2d.pixels 60 60) (pixels 20)

    container =
        emptyContainer |> insert "a" circleA |> insert "b" circleB

    collideWith (Circle2d.centerPoint >> Point2d.equalWithin (pixels 10) (Point2d.pixels 50 50)) someBox container
        == [ { key = "b", object = circleB } ]

-}
collideWith :
    (object -> Bool)
    -> boundingBox
    -> Container comparable object boundingBox
    -> List { key : comparable, object : object }
collideWith checkCollision boundingBox cont =
    case cont of
        QuadTree container ->
            let
                getCollided item result =
                    if container.intersects boundingBox item.boundingBox then
                        if checkCollision item.object then
                            { key = item.key, object = item.object } :: result

                        else
                            result

                    else
                        result

                collect lqtIndex collidedItems =
                    case Array.get lqtIndex container.objects of
                        Just ary ->
                            Array.foldr getCollided [] ary ++ collidedItems

                        Nothing ->
                            collidedItems
            in
            C.containedLqtIndices
                { extrema = container.extrema
                , truncateX = container.truncateX
                , truncateY = container.truncateY
                , unitWidth = container.unitWidth
                , unitHeight = container.unitHeight
                , depth = container.depth
                }
                boundingBox
                |> List.foldr collect []

        Naive container ->
            let
                filterItem ( key, item ) =
                    if container.intersects boundingBox item.boundingBox then
                        if checkCollision item.object then
                            Just { key = key, object = item.object }

                        else
                            Nothing

                    else
                        Nothing
            in
            Dict.toList container.objects |> List.filterMap filterItem



-- Queries


{-| Insert key and object to a container.
-}
insert : comparable -> object -> Container comparable object boundingBox -> Container comparable object boundingBox
insert key object cont =
    case cont of
        QuadTree container ->
            quadTreeInsert key (container.getBoundingBox object) object container |> QuadTree

        Naive container ->
            { container
                | objects =
                    Dict.insert key
                        { boundingBox = container.getBoundingBox object, object = object }
                        container.objects
            }
                |> Naive


quadTreeInsert :
    comparable
    -> boundingBox
    -> object
    -> QuadTreeRecord comparable object boundingBox
    -> QuadTreeRecord comparable object boundingBox
quadTreeInsert key boundingBox object container =
    let
        target =
            C.truncatedExtrema
                { extrema = container.extrema, truncateX = container.truncateX, truncateY = container.truncateY }
                boundingBox

        newLqtIndex =
            C.linerQuaternaryTreeIndex
                { depth = container.depth, unitWidth = container.unitWidth, unitHeight = container.unitHeight }
                target

        newRecord =
            { key = key, boundingBox = boundingBox, object = object }
    in
    case Dict.get key container.keyToIndex of
        Just old ->
            if old.lqt == newLqtIndex then
                -- MEMO: The new object is belongs to same cell as old one.
                { container | objects = quadTreeUpdateObject old newRecord container.objects }

            else
                let
                    ( oldRemovedObjects, maybeKeyToRewriteOffset ) =
                        quadTreeRemoveObject old container.objects

                    ( newObjects, maybeNewOffset ) =
                        quadTreeAddObject newLqtIndex newRecord oldRemovedObjects
                in
                case ( maybeNewOffset, maybeKeyToRewriteOffset ) of
                    ( Just offset, Just ( rKey, rOffset ) ) ->
                        { container
                            | objects = newObjects
                            , keyToIndex =
                                container.keyToIndex
                                    |> Dict.insert key { lqt = newLqtIndex, offset = offset }
                                    |> Dict.insert rKey { lqt = old.lqt, offset = rOffset }
                        }

                    ( Just offset, Nothing ) ->
                        { container
                            | objects = newObjects
                            , keyToIndex = Dict.insert key { lqt = newLqtIndex, offset = offset } container.keyToIndex
                        }

                    _ ->
                        container

        Nothing ->
            let
                ( newObjects, maybeNewOffset ) =
                    quadTreeAddObject newLqtIndex newRecord container.objects
            in
            case maybeNewOffset of
                Just offset ->
                    { container
                        | objects = newObjects
                        , keyToIndex = Dict.insert key { lqt = newLqtIndex, offset = offset } container.keyToIndex
                    }

                Nothing ->
                    container


{-| Update an object if it exists.
-}
update :
    comparable
    -> (Maybe object -> Maybe object)
    -> Container comparable object boundingBox
    -> Container comparable object boundingBox
update key f cont =
    case get key cont |> f of
        Just newObj ->
            insert key newObj cont

        Nothing ->
            cont


{-| Update an object from a container if it exists.
-}
remove : comparable -> Container comparable object boundingBox -> Container comparable object boundingBox
remove key cont =
    case cont of
        QuadTree container ->
            case Dict.get key container.keyToIndex of
                Just index ->
                    let
                        ( newObjects, maybeKeysToRewriteOffset ) =
                            quadTreeRemoveObject index container.objects
                    in
                    case maybeKeysToRewriteOffset of
                        Just ( rKey, rOffset ) ->
                            { container
                                | keyToIndex =
                                    container.keyToIndex
                                        |> Dict.insert rKey { lqt = index.lqt, offset = rOffset }
                                        |> Dict.remove key
                                , objects = newObjects
                            }
                                |> QuadTree

                        Nothing ->
                            { container | keyToIndex = container.keyToIndex |> Dict.remove key, objects = newObjects }
                                |> QuadTree

                Nothing ->
                    QuadTree container

        Naive container ->
            Naive { container | objects = Dict.remove key container.objects }


quadTreeAddObject :
    Int
    -> { key : comparable, object : object, boundingBox : boundingBox }
    -> Array (Array { key : comparable, object : object, boundingBox : boundingBox })
    -> ( Array (Array { key : comparable, object : object, boundingBox : boundingBox }), Maybe Int )
quadTreeAddObject lqt item arrays =
    case Array.get lqt arrays of
        Just ary ->
            ( Array.set lqt (Array.push item ary) arrays, Just (Array.length ary) )

        Nothing ->
            ( arrays, Nothing )


quadTreeUpdateObject :
    { lqt : Int, offset : Int }
    -> { key : comparable, object : object, boundingBox : boundingBox }
    -> Array (Array { key : comparable, object : object, boundingBox : boundingBox })
    -> Array (Array { key : comparable, object : object, boundingBox : boundingBox })
quadTreeUpdateObject index item arrays =
    case Array.get index.lqt arrays of
        Just ary ->
            Array.set index.lqt (Array.set index.offset item ary) arrays

        Nothing ->
            arrays


quadTreeRemoveObject :
    { lqt : Int, offset : Int }
    -> Array (Array { key : comparable, object : object, boundingBox : boundingBox })
    -> ( Array (Array { key : comparable, object : object, boundingBox : boundingBox }), Maybe ( comparable, Int ) )
quadTreeRemoveObject index arrays =
    case Array.get index.lqt arrays of
        Just ary ->
            let
                lastOffset =
                    Array.length ary - 1
            in
            if index.offset == lastOffset then
                ( Array.set index.lqt (Array.slice 0 -1 ary) arrays, Nothing )

            else
                case Array.get lastOffset ary of
                    Just item ->
                        -- MEMO: The last object's offset index is set to be removed object's offset index.
                        ( Array.set index.lqt (Array.set index.offset item (Array.slice 0 -1 ary)) arrays
                        , Just ( item.key, index.offset )
                        )

                    Nothing ->
                        -- MEMO: There is no object other than selected (to remove) object in this lqt cell.
                        ( Array.set index.lqt Array.empty arrays, Nothing )

        Nothing ->
            ( arrays, Nothing )


{-| Get an object from a container if it exists.
-}
get : comparable -> Container comparable object boundingBox -> Maybe object
get key cont =
    case cont of
        QuadTree container ->
            case Dict.get key container.keyToIndex of
                Just { lqt, offset } ->
                    case Array.get lqt container.objects of
                        Just ary ->
                            Array.get offset ary |> Maybe.map .object

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Naive container ->
            Dict.get key container.objects |> Maybe.map .object


{-| Convert a container to a `Dict`
-}
toDict :
    Container comparable object boundingBox
    -> Dict comparable object
toDict cont =
    case cont of
        QuadTree container ->
            Array.foldr Array.append Array.empty container.objects
                |> Array.map (\r -> ( r.key, r.object ))
                |> Array.toList
                |> Dict.fromList

        Naive container ->
            Dict.map (\_ v -> v.object) container.objects


{-| Returns number of objects a container stores.
-}
size :
    Container comparable object boundingBox
    -> Int
size cont =
    case cont of
        QuadTree container ->
            Dict.size container.keyToIndex

        Naive container ->
            Dict.size container.objects



-- lists


{-| Get all keys in a container.
-}
keys : Container comparable object boundingBox -> List comparable
keys cont =
    case cont of
        QuadTree container ->
            Dict.keys container.keyToIndex

        Naive container ->
            Dict.keys container.objects


{-| Get all objects in a container.
-}
values : Container comparable object boundingBox -> List object
values cont =
    case cont of
        QuadTree container ->
            Array.foldr
                (\ary objs -> List.append objs (Array.foldr (\itm l -> itm.object :: l) [] ary))
                []
                container.objects

        Naive container ->
            Dict.values container.objects |> List.map .object



-- transform


{-| Apply functions to all objects in a container.
-}
map :
    (comparable -> object -> object)
    -> Container comparable object boundingBox
    -> Container comparable object boundingBox
map f cont =
    case cont of
        QuadTree container ->
            let
                ( newObjects, newKeyToIndex ) =
                    quadTreeMap
                        { extrema = container.extrema
                        , getBoundingBox = container.getBoundingBox
                        , depth = container.depth
                        , unitWidth = container.unitWidth
                        , unitHeight = container.unitHeight
                        , truncateX = container.truncateX
                        , truncateY = container.truncateY
                        }
                        f
                        container.objects
            in
            QuadTree { container | objects = newObjects, keyToIndex = newKeyToIndex }

        Naive container ->
            let
                g k v =
                    let
                        newObject =
                            f k v.object
                    in
                    { object = newObject, boundingBox = container.getBoundingBox newObject }
            in
            Naive { container | objects = Dict.map g container.objects }


quadTreeMap :
    { extrema : boundingBox -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
    , getBoundingBox : object -> boundingBox
    , depth : Int
    , unitWidth : Int
    , unitHeight : Int
    , truncateX : Float -> Float
    , truncateY : Float -> Float
    }
    -> (comparable -> object -> object)
    -> Array (Array { key : comparable, object : object, boundingBox : boundingBox })
    -> ( Array (Array { key : comparable, object : object, boundingBox : boundingBox }), Dict comparable { lqt : Int, offset : Int } )
quadTreeMap opt f objects =
    let
        mapItem itm =
            f itm.key itm.object |> (\o -> { itm | object = o, boundingBox = opt.getBoundingBox o })

        flattenObjects =
            Array.foldr (\ary objs -> Array.append objs (Array.map mapItem ary)) Array.empty objects

        loop itm ({ keyToIndex, objs } as result) =
            let
                newLqtIndex =
                    itm.boundingBox
                        |> C.truncatedExtrema
                            { extrema = opt.extrema
                            , truncateX = opt.truncateX
                            , truncateY = opt.truncateY
                            }
                        |> C.linerQuaternaryTreeIndex
                            { depth = opt.depth
                            , unitWidth = opt.unitWidth
                            , unitHeight = opt.unitHeight
                            }
            in
            case Array.get newLqtIndex objs of
                Just ary ->
                    { keyToIndex = Dict.insert itm.key { lqt = newLqtIndex, offset = Array.length ary } keyToIndex
                    , objs = Array.set newLqtIndex (Array.push itm ary) objs
                    }

                Nothing ->
                    result
    in
    Array.foldl loop { keyToIndex = Dict.empty, objs = Array.repeat (Array.length objects) Array.empty } flattenObjects
        |> (\r -> ( r.objs, r.keyToIndex ))


{-| Foldl. It is equivalent to `toDict |> Dict.foldl`
-}
foldl :
    (comparable -> object -> a -> a)
    -> a
    -> Container comparable object boundingBox
    -> a
foldl f r cont =
    toDict cont |> Dict.foldl f r


{-| Foldr. It is equivalent to `toDict |> Dict.foldr`
-}
foldr :
    (comparable -> object -> a -> a)
    -> a
    -> Container comparable object boundingBox
    -> a
foldr f r cont =
    toDict cont |> Dict.foldr f r


{-| Keep only objects which passed the given test.
-}
filter :
    (comparable -> object -> Bool)
    -> Container comparable object boundingBox
    -> Container comparable object boundingBox
filter f cont =
    case cont of
        QuadTree container ->
            let
                ( newObjects, newKeyToIndex ) =
                    quadTreeFilter f container.objects
            in
            QuadTree { container | objects = newObjects, keyToIndex = newKeyToIndex }

        Naive container ->
            let
                g k v =
                    f k v.object
            in
            Naive { container | objects = Dict.filter g container.objects }


quadTreeFilter :
    (comparable -> object -> Bool)
    -> Array (Array { key : comparable, object : object, boundingBox : boundingBox })
    -> ( Array (Array { key : comparable, object : object, boundingBox : boundingBox }), Dict comparable { lqt : Int, offset : Int } )
quadTreeFilter f objects =
    let
        loop ary { lqt, keyToIndex, objs } =
            let
                newAry =
                    Array.filter (\o -> f o.key o.object) ary

                indexDict =
                    Array.toList newAry |> List.indexedMap (\i item -> ( item.key, { lqt = lqt, offset = i } )) |> Dict.fromList
            in
            { lqt = lqt + 1, keyToIndex = Dict.union keyToIndex indexDict, objs = Array.push newAry objs }
    in
    Array.foldl loop { lqt = 0, keyToIndex = Dict.empty, objs = Array.empty } objects
        |> (\r -> ( r.objs, r.keyToIndex ))
