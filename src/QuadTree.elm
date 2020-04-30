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
import Binary
import BoundingBox2d exposing (BoundingBox2d)
import Dict exposing (Dict)
import QuadTree.Internal.QuadTree as Q
import Quantity exposing (Quantity)


type QuadTree comparable shape units coordinates
    = QuadTree
        { toFloat : Quantity Float units -> Float
        , shapes : Dict comparable { index : Int, shape : shape, boundingBox : BoundingBox2d units coordinates }
        , linerQuaternaryTree : Array (Array comparable) -- Hashtable for (Morton Order -> Annotation Id)
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
    , shapes = Dict.empty
    , linerQuaternaryTree = Array.repeat ((4 ^ depth - 1) // 3) Array.empty
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
    , shapes = Dict.empty
    , linerQuaternaryTree = Array.repeat ((4 ^ opt.depth - 1) // 3) Array.empty
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
    (shape -> shape -> Bool)
    -> QuadTree comparable shape units coordinates
    -> List ( comparable, comparable )
detectCollisions checkCollision (QuadTree container) =
    -- TODO
    []


collideWith :
    (shape -> Bool)
    -> BoundingBox2d units coordinates
    -> QuadTree comparable shape units coordinates
    -> List ( comparable, shape )
collideWith checkCollision boundingBox (QuadTree container) =
    let
        isCollidedWith obj =
            if BoundingBox2d.intersects boundingBox obj.boundingBox then
                checkCollision obj.shape

            else
                False

        getCollided key result =
            case Dict.get key container.shapes of
                Just obj ->
                    if isCollidedWith obj then
                        ( key, obj.shape ) :: result

                    else
                        result

                Nothing ->
                    result

        collect lqtIndex collidedItems =
            case Array.get lqtIndex container.linerQuaternaryTree of
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
        |> List.foldl collect []


insert :
    comparable
    -> BoundingBox2d units coordinates
    -> shape
    -> QuadTree comparable shape units coordinates
    -> QuadTree comparable shape units coordinates
insert key boundingBox shape (QuadTree container) =
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

        newIndex =
            linerQuaternaryTreeIndex
                { depth = container.depth, unitWidth = container.unitWidth, unitHeight = container.unitHeight }
                target

        newItem =
            { index = newIndex, boundingBox = boundingBox, shape = shape }
    in
    case Dict.get key container.shapes of
        Just old ->
            if old.index == newIndex then
                { container | shapes = Dict.insert key newItem container.shapes } |> QuadTree

            else
                { container
                    | shapes = Dict.insert key newItem container.shapes
                    , linerQuaternaryTree =
                        container.linerQuaternaryTree
                            |> Q.removeLqtIndex old.index key
                            |> Q.insertLqtIndex newIndex key
                }
                    |> QuadTree

        Nothing ->
            { container
                | shapes = Dict.insert key newItem container.shapes
                , linerQuaternaryTree = container.linerQuaternaryTree |> Q.insertLqtIndex newIndex key
            }
                |> QuadTree


remove :
    comparable
    -> QuadTree comparable shape units coordinates
    -> QuadTree comparable shape units coordinates
remove key (QuadTree container) =
    case Dict.get key container.shapes of
        Just data ->
            { container
                | shapes = Dict.remove key container.shapes
                , linerQuaternaryTree = Q.removeLqtIndex data.index key container.linerQuaternaryTree
            }
                |> QuadTree

        Nothing ->
            QuadTree container


get :
    comparable
    -> QuadTree comparable shape units coordinates
    -> Maybe shape
get key (QuadTree container) =
    Dict.get key container.shapes |> Maybe.map .shape


getWith :
    comparable
    -> QuadTree comparable shape units coordinates
    -> Maybe shape
getWith key (QuadTree container) =
    Dict.get key container.shapes |> Maybe.map .shape


toDict :
    QuadTree comparable shape units coordinates
    -> Dict comparable { shape : shape, boundingBox : BoundingBox2d units coordinates }
toDict (QuadTree container) =
    container.shapes |> Dict.map (\_ r -> { shape = r.shape, boundingBox = r.boundingBox })


{-| Calculate liner quaternary tree index. `depth` must be equal or larger than 2.

    bBox = { left = 10, top = 10, right = 15, bottom = 15 }
    linerQuaternaryTreeIndex { depth = 2, unitWidth = 20, unitHeight = 20 } bBox == 1  -- Number 0 element of Level 1.

-}
linerQuaternaryTreeIndex :
    { depth : Int, unitWidth : Int, unitHeight : Int }
    -> { left : Float, top : Float, right : Float, bottom : Float }
    -> Int
linerQuaternaryTreeIndex { depth, unitWidth, unitHeight } { left, top, right, bottom } =
    let
        ( topLeft, bottomRight ) =
            ( Q.quadKey { unitWidth = unitWidth, unitHeight = unitHeight } { x = left, y = top }
            , Q.quadKey { unitWidth = unitWidth, unitHeight = unitHeight } { x = right, y = bottom }
            )

        padZeros l =
            List.repeat (max 0 (2 * (depth - 1) - List.length l)) 0 ++ l

        totalTilesOfUpperLayers : Int -> Int
        totalTilesOfUpperLayers currentLevel =
            -- Root Level is 0; Start counting with 0
            (4 ^ currentLevel - 1) // 3

        pairByLevel : List Int -> List ( Int, Int )
        pairByLevel l =
            case l of
                a :: b :: rest ->
                    ( a, b ) :: pairByLevel rest

                _ ->
                    []

        getIndex : List ( Int, ( Int, Int ) ) -> Int
        getIndex l =
            case l of
                ( i, ( a, b ) ) :: [] ->
                    if a == 0 && b == 0 then
                        -- When bounding box is contained in most-leaf level tile. ( i + 1 == depth ).
                        topLeft |> Binary.toDecimal |> (+) (totalTilesOfUpperLayers (i + 1))

                    else
                        -- When bounding box is contained in (depth - 1) level tile. ( Level == depth - 1 == i ).
                        Binary.shiftRightZfBy (2 * (depth - i - 1)) topLeft
                            |> Binary.toDecimal
                            |> (+) (totalTilesOfUpperLayers i)

                ( i, ( a, b ) ) :: rest ->
                    if a == 0 && b == 0 then
                        getIndex rest

                    else
                        -- When bounding box is contained in i level tile.
                        Binary.shiftRightZfBy (2 * (depth - i - 1)) topLeft
                            |> Binary.toDecimal
                            |> (+) (totalTilesOfUpperLayers i)

                [] ->
                    0
    in
    Binary.xor topLeft bottomRight
        |> Binary.toIntegers
        |> padZeros
        |> pairByLevel
        |> List.indexedMap (\i t -> ( i, t ))
        |> getIndex
