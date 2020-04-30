module QuadTree exposing
    ( QuadTree
    , collideWith
    , customQuadTree
    , get
    , insert
    , quadTree
    , remove
    , toDict
    )

import Array exposing (Array)
import BoundingBox2d exposing (BoundingBox2d)
import Dict exposing (Dict)
import QuadTree.Internal.QuadTree as Q
import Quantity exposing (Quantity)


type QuadTree comparable object units coordinates
    = QuadTree
        { toFloat : Quantity Float units -> Float
        , keyToIndex : Dict comparable { lqt : Int, offset : Int }
        , objects : Array (Array { key : comparable, object : object, boundingBox : BoundingBox2d units coordinates })
        , depth : Int
        , unitWidth : Int
        , unitHeight : Int
        , minX : Float
        , maxX : Float
        , minY : Float
        , maxY : Float
        , truncateX : Float -> Float
        , truncateY : Float -> Float
        }


quadTree :
    { toFloat : Quantity Float units -> Float, minX : Float, minY : Float, maxX : Float, maxY : Float }
    -> QuadTree comparable shape units coordinates
quadTree opt =
    let
        ( width, height ) =
            ( opt.maxX - opt.minX |> floor, opt.maxY - opt.minY |> floor )

        tmpUnitSize =
            256

        depth =
            (1 - logBase 2 tmpUnitSize + logBase 2 (toFloat <| max width height)) |> floor |> max 1

        ( unitWidth, unitHeight ) =
            ( width // 2 ^ (depth - 1) + 1, height // 2 ^ (depth - 1) + 1 )
    in
    { toFloat = opt.toFloat
    , keyToIndex = Dict.empty
    , objects = Array.repeat ((4 ^ depth - 1) // 3) Array.empty
    , depth = depth
    , unitWidth = unitWidth
    , unitHeight = unitHeight
    , minX = opt.minX
    , maxX = opt.maxX
    , minY = opt.minY
    , maxY = opt.maxY
    , truncateX = max opt.minX >> min opt.maxX
    , truncateY = max opt.minY >> min opt.maxY
    }
        |> QuadTree


customQuadTree :
    { toFloat : Quantity Float units -> Float
    , minX : Float
    , minY : Float
    , maxX : Float
    , maxY : Float
    , depth : Int
    , unitWidth : Int
    , unitHeight : Int
    }
    -> QuadTree comparable shape units coordinates
customQuadTree opt =
    { toFloat = opt.toFloat
    , keyToIndex = Dict.empty
    , objects = Array.repeat ((4 ^ opt.depth - 1) // 3) Array.empty
    , depth = opt.depth
    , unitWidth = opt.unitWidth
    , unitHeight = opt.unitHeight
    , minX = opt.minX
    , maxX = opt.maxX
    , minY = opt.minY
    , maxY = opt.maxY
    , truncateX = max opt.minX >> min opt.maxX
    , truncateY = max opt.minY >> min opt.maxY
    }
        |> QuadTree


detectCollisions :
    (object -> object -> Bool)
    -> QuadTree comparable object units coordinates
    -> List ( { key : comparable, object : object }, { key : comparable, object : object } )
detectCollisions checkCollision (QuadTree container) =
    let
        check itemA itemB =
            if BoundingBox2d.intersects itemA.boundingBox itemB.boundingBox then
                checkCollision itemA.object itemB.object

            else
                False

        toResult itemA itemB =
            ( { key = itemA.key, object = itemA.object }, { key = itemB.key, object = itemB.object } )

        collect =
            detectCollisionsLoop
                { check = check
                , toResult = toResult
                , depth = container.depth
                , objects = container.objects
                }
    in
    Array.foldr collect { index = 0, result = [] } container.objects
        |> .result


collideWith :
    (object -> Bool)
    -> BoundingBox2d units coordinates
    -> QuadTree comparable object units coordinates
    -> List { key : comparable, object : object }
collideWith checkCollision boundingBox (QuadTree container) =
    let
        getCollided item result =
            if BoundingBox2d.intersects boundingBox item.boundingBox then
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
    Q.containedLqtIndices
        { toFloat = container.toFloat
        , truncateX = container.truncateX
        , truncateY = container.truncateY
        , unitWidth = container.unitWidth
        , unitHeight = container.unitHeight
        , depth = container.depth
        }
        boundingBox
        |> List.foldr collect []


insert :
    comparable
    -> BoundingBox2d units coordinates
    -> object
    -> QuadTree comparable object units coordinates
    -> QuadTree comparable object units coordinates
insert key boundingBox object (QuadTree container) =
    let
        target =
            BoundingBox2d.extrema boundingBox
                |> (\r ->
                        { left = r.minX |> container.toFloat |> container.truncateX
                        , top = r.minY |> container.toFloat |> container.truncateY
                        , right = r.maxX |> container.toFloat |> container.truncateX
                        , bottom = r.maxY |> container.toFloat |> container.truncateY
                        }
                   )

        newLqtIndex =
            Q.linerQuaternaryTreeIndex
                { depth = container.depth, unitWidth = container.unitWidth, unitHeight = container.unitHeight }
                target

        newRecord =
            { key = key, boundingBox = boundingBox, object = object }
    in
    case Dict.get key container.keyToIndex of
        Just old ->
            if old.lqt == newLqtIndex then
                { container | objects = updateObject old newRecord container.objects }
                    |> QuadTree

            else
                let
                    ( oldRemovedObjects, maybeKeyToRewriteOffset ) =
                        removeObject old container.objects

                    ( newObjects, maybeNewOffset ) =
                        addObject newLqtIndex newRecord oldRemovedObjects
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
                            |> QuadTree

                    ( Just offset, Nothing ) ->
                        { container
                            | objects = newObjects
                            , keyToIndex = Dict.insert key { lqt = newLqtIndex, offset = offset } container.keyToIndex
                        }
                            |> QuadTree

                    _ ->
                        QuadTree container

        Nothing ->
            let
                ( newObjects, maybeNewOffset ) =
                    addObject newLqtIndex newRecord container.objects
            in
            case maybeNewOffset of
                Just offset ->
                    { container
                        | objects = newObjects
                        , keyToIndex = Dict.insert key { lqt = newLqtIndex, offset = offset } container.keyToIndex
                    }
                        |> QuadTree

                Nothing ->
                    QuadTree container


remove :
    comparable
    -> QuadTree comparable shape units coordinates
    -> QuadTree comparable shape units coordinates
remove key (QuadTree container) =
    case Dict.get key container.keyToIndex of
        Just index ->
            let
                ( newObjects, maybeKeysToRewriteOffset ) =
                    removeObject index container.objects
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


get :
    comparable
    -> QuadTree comparable object units coordinates
    -> Maybe object
get key (QuadTree container) =
    case Dict.get key container.keyToIndex of
        Just { lqt, offset } ->
            case Array.get lqt container.objects of
                Just ary ->
                    Array.get offset ary |> Maybe.map .object

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


toDict :
    QuadTree comparable object units coordinates
    -> Dict comparable { object : object, boundingBox : BoundingBox2d units coordinates }
toDict (QuadTree container) =
    Array.foldr Array.append Array.empty container.objects
        |> Array.map (\r -> ( r.key, { object = r.object, boundingBox = r.boundingBox } ))
        |> Array.toList
        |> Dict.fromList



-- Helpers


detectCollisionsLoop :
    { check : a -> a -> Bool, toResult : a -> a -> b, objects : Array (Array a), depth : Int }
    -> Array a
    -> { index : Int, result : List b }
    -> { index : Int, result : List b }
detectCollisionsLoop opt items { index, result } =
    let
        parentToRoot =
            Q.parentToRootLevelLqtIndices { depth = opt.depth } index
                |> List.foldr
                    (\i ary -> Array.append ary (Maybe.withDefault Array.empty (Array.get i opt.objects)))
                    Array.empty
    in
    { index = index + 1
    , result = detectCollisionsOfNode { check = opt.check, toResult = opt.toResult } items parentToRoot result
    }


detectCollisionsOfNode : { check : a -> a -> Bool, toResult : a -> a -> b } -> Array a -> Array a -> List b -> List b
detectCollisionsOfNode opt items parentToRootItems result =
    let
        collect itm1 itm2 rlt =
            if opt.check itm1 itm2 then
                opt.toResult itm1 itm2 :: rlt

            else
                rlt

        remains =
            Array.slice 1 (Array.length items) items
    in
    case Array.get 0 items of
        Just item ->
            Array.foldr (collect item) result (Array.append remains parentToRootItems)
                |> detectCollisionsOfNode opt remains parentToRootItems

        Nothing ->
            result


addObject :
    Int
    -> { key : comparable, object : object, boundingBox : BoundingBox2d units coordinates }
    -> Array (Array { key : comparable, object : object, boundingBox : BoundingBox2d units coordinates })
    -> ( Array (Array { key : comparable, object : object, boundingBox : BoundingBox2d units coordinates }), Maybe Int )
addObject lqt item arrays =
    case Array.get lqt arrays of
        Just ary ->
            ( Array.set lqt (Array.push item ary) arrays, Just (Array.length ary) )

        Nothing ->
            ( arrays, Nothing )


updateObject :
    { lqt : Int, offset : Int }
    -> { key : comparable, object : object, boundingBox : BoundingBox2d units coordinates }
    -> Array (Array { key : comparable, object : object, boundingBox : BoundingBox2d units coordinates })
    -> Array (Array { key : comparable, object : object, boundingBox : BoundingBox2d units coordinates })
updateObject index item arrays =
    case Array.get index.lqt arrays of
        Just ary ->
            Array.set index.lqt (Array.set index.offset item ary) arrays

        Nothing ->
            arrays


removeObject :
    { lqt : Int, offset : Int }
    -> Array (Array { key : comparable, object : object, boundingBox : BoundingBox2d units coordinates })
    -> ( Array (Array { key : comparable, object : object, boundingBox : BoundingBox2d units coordinates }), Maybe ( comparable, Int ) )
removeObject index arrays =
    case Array.get index.lqt arrays of
        Just ary ->
            case Array.get (Array.length ary - 1) ary of
                Just item ->
                    ( Array.set index.lqt (Array.set index.offset item (Array.slice 0 -1 ary)) arrays
                    , Just ( item.key, index.offset )
                    )

                Nothing ->
                    ( Array.set index.lqt Array.empty arrays, Nothing )

        Nothing ->
            ( arrays, Nothing )
