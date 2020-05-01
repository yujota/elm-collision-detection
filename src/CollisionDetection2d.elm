module CollisionDetection2d exposing
    ( Container(..)
    , quadTree
    , detectCollisions, collideWith
    , get, insert, remove, size, toDict
    , naive, customQuadTree
    )

{-| Collision detection 2D


# Types

@docs Container, Boundary


# Constructor

@docs quadTree


# CollisionDetection

@docs detectCollisions, collideWith


# Queries

@docs get, insert, remove, size, toDict


# For advanced usages

@docs naive, customQuadTree

-}

import Array exposing (Array)
import CollisionDetection2d.Internal.CollisionDetection2d as C
import Dict exposing (Dict)



-- Types


type Container comparable object boundingBox
    = QuadTree (QuadTreeRecord comparable object boundingBox)
    | Naive
        { extrema : boundingBox -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
        , intersects : boundingBox -> boundingBox -> Bool
        , objects : Dict comparable { object : object, boundingBox : boundingBox }
        }


type alias QuadTreeRecord comparable object boundingBox =
    { extrema : boundingBox -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
    , intersects : boundingBox -> boundingBox -> Bool
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


quadTree :
    { extrema : boundingBox -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
    , intersects : boundingBox -> boundingBox -> Bool
    , boundary : Boundary
    }
    -> Container comparable object boundingBox
quadTree { extrema, intersects, boundary } =
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


customQuadTree :
    { extrema : boundingBox -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
    , intersects : boundingBox -> boundingBox -> Bool
    , boundary : Boundary
    , depth : Int
    , unitWidth : Int
    , unitHeight : Int
    }
    -> Container comparable object boundingBox
customQuadTree { extrema, intersects, boundary, depth, unitWidth, unitHeight } =
    if depth >= 2 then
        { extrema = extrema
        , intersects = intersects
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

    else
        naive { extrema = extrema, intersects = intersects }


naive :
    { extrema : boundingBox -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
    , intersects : boundingBox -> boundingBox -> Bool
    }
    -> Container comparable object boundingBox
naive { extrema, intersects } =
    Naive { extrema = extrema, intersects = intersects, objects = Dict.empty }



-- Collision detection


{-| Detect collisions
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


{-| CollideWith
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
                filter ( key, item ) =
                    if container.intersects boundingBox item.boundingBox then
                        if checkCollision item.object then
                            Just { key = key, object = item.object }

                        else
                            Nothing

                    else
                        Nothing
            in
            Dict.toList container.objects |> List.filterMap filter



-- Queries


{-| insert
-}
insert :
    comparable
    -> boundingBox
    -> object
    -> Container comparable object boundingBox
    -> Container comparable object boundingBox
insert key boundingBox object cont =
    case cont of
        QuadTree container ->
            quadTreeInsert key boundingBox object container |> QuadTree

        Naive container ->
            { container | objects = Dict.insert key { boundingBox = boundingBox, object = object } container.objects }
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


{-| remove
-}
remove :
    comparable
    -> Container comparable object boundingBox
    -> Container comparable object boundingBox
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
            case Array.get (Array.length ary - 1) ary of
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


{-| get item
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


{-| to dict
-}
toDict :
    Container comparable object boundingBox
    -> Dict comparable { object : object, boundingBox : boundingBox }
toDict cont =
    case cont of
        QuadTree container ->
            Array.foldr Array.append Array.empty container.objects
                |> Array.map (\r -> ( r.key, { object = r.object, boundingBox = r.boundingBox } ))
                |> Array.toList
                |> Dict.fromList

        Naive container ->
            container.objects


{-| size
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
