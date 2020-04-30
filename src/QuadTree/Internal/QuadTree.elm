module QuadTree.Internal.QuadTree exposing
    ( containedLqtIndices
    , linerQuaternaryTreeIndex
    , parentToRootLevelLqtIndices
    , quadKey
    , truncatedExtrema
    )

import Array exposing (Array)
import Binary
import BoundingBox2d exposing (BoundingBox2d)
import Quantity exposing (Quantity)
import Set


quadKey : { unitWidth : Int, unitHeight : Int } -> { x : Float, y : Float } -> Binary.Bits
quadKey { unitWidth, unitHeight } { x, y } =
    toQuadKey { rowId = floor x // unitWidth, colId = floor y // unitHeight }


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
            ( quadKey { unitWidth = unitWidth, unitHeight = unitHeight } { x = left, y = top }
            , quadKey { unitWidth = unitWidth, unitHeight = unitHeight } { x = right, y = bottom }
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


containedLqtIndices :
    { toFloat : Quantity Float units -> Float
    , truncateX : Float -> Float
    , truncateY : Float -> Float
    , unitWidth : Int
    , unitHeight : Int
    , depth : Int
    }
    -> BoundingBox2d units coordinates
    -> List Int
containedLqtIndices opt boundingBox =
    let
        { startRow, startCol, endRow, endCol } =
            truncatedExtrema { toFloat = opt.toFloat, truncateX = opt.truncateX, truncateY = opt.truncateY } boundingBox
                |> (\r ->
                        { startRow = floor r.left // opt.unitWidth
                        , startCol = floor r.top // opt.unitHeight
                        , endRow = floor r.right // opt.unitWidth
                        , endCol = floor r.bottom // opt.unitHeight
                        }
                   )

        highestLevelQuadKeys =
            -- Highest Level == depth - 1
            let
                rows c =
                    List.range startRow endRow
                        |> List.map (\r -> toQuadKey { rowId = r, colId = c } |> Binary.toDecimal)
            in
            List.range startCol endCol |> List.map rows |> List.concat
    in
    containedLqtIndicesLoop (opt.depth - 1) highestLevelQuadKeys []


containedLqtIndicesLoop : Int -> List Int -> List Int -> List Int
containedLqtIndicesLoop level quadKeys result =
    let
        addition =
            (4 ^ level - 1) // 3
    in
    if level < 0 then
        result

    else if level == 0 then
        List.map ((+) addition) quadKeys ++ result

    else
        let
            toSearch =
                quadKeys
                    |> List.map (Binary.fromDecimal >> Binary.shiftRightZfBy 2 >> Binary.toDecimal)
                    -- Remove duplicates
                    |> Set.fromList
                    |> Set.toList
        in
        containedLqtIndicesLoop (level - 1) toSearch (List.map ((+) addition) quadKeys ++ result)


parentToRootLevelLqtIndices : { depth : Int } -> Int -> List Int
parentToRootLevelLqtIndices { depth } lqtIndex =
    let
        bin =
            Binary.fromDecimal lqtIndex

        level =
            logBase 4 (3 * lqtIndex + 1 |> toFloat) |> floor
    in
    List.range 1 level
        |> List.map (\i -> Binary.shiftRightZfBy (2 * i) bin |> Binary.toDecimal)


toQuadKey : { rowId : Int, colId : Int } -> Binary.Bits
toQuadKey { rowId, colId } =
    Binary.or
        (Binary.fromDecimal rowId |> Binary.toIntegers |> List.concatMap (\i -> [ 0, i ]) |> Binary.fromIntegers)
        (Binary.fromDecimal colId |> Binary.toIntegers |> List.concatMap (\i -> [ i, 0 ]) |> Binary.fromIntegers)


truncatedExtrema :
    { toFloat : Quantity Float units -> Float, truncateX : Float -> Float, truncateY : Float -> Float }
    -> BoundingBox2d units coordinates
    -> { left : Float, top : Float, right : Float, bottom : Float }
truncatedExtrema opt boundingBox =
    BoundingBox2d.extrema boundingBox
        |> (\r ->
                { left = r.minX |> opt.toFloat |> opt.truncateX
                , top = r.minY |> opt.toFloat |> opt.truncateY
                , right = r.maxX |> opt.toFloat |> opt.truncateX
                , bottom = r.maxY |> opt.toFloat |> opt.truncateY
                }
           )
