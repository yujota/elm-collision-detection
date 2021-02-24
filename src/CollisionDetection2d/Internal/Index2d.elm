module CollisionDetection2d.Internal.Index2d exposing
    ( coordinateToIndex, sharedNode, toPath, fromPath
    , coordinateIdToMortonOrder, toIndex, fromIndex
    , Direction2d(..), Index2d
    )

{-| Index


# Types and Functions which other modules would call

@docs Index, Direction, coordinateToIndex, sharedNode, toPath, fromPath


# Functions exposed for tests

@docs coordinateIdToMortonOrder, toIndex, fromIndex

-}


{-| Index is calculated as following:

MortonOrder + Sum of number of items from root to previous layer

This index is used as a key to access a Leaf/Node of a Tree.

-}
type alias Index2d =
    Int


type alias LayerId =
    Int


type alias MortonOrder =
    Int


type alias Digit =
    Int


type alias QuadraticDigits =
    List Digit


type Direction2d
    = D0 -- GoTo TopLeft
    | D1 -- GoTo TopRight
    | D2 -- GoTo BottomLeft
    | D3 -- GoTo BottomRight


{-| Calculate index

depth must be one or larger.

-}
coordinateToIndex :
    { depth : Int, unitWidth : Int, unitHeight : Int }
    -> { x0 : Float, y0 : Float, x1 : Float, y1 : Float }
    -> Index2d
coordinateToIndex { depth, unitWidth, unitHeight } { x0, y0, x1, y1 } =
    let
        m0 =
            coordinateIdToMortonOrder depth (floor x0 // unitWidth) (floor y0 // unitHeight)

        m1 =
            coordinateIdToMortonOrder depth (floor x1 // unitWidth) (floor y1 // unitHeight)

        q0 =
            mortonOrderToQDigits m0
                -- NOTE: Number of digits is depth - 1 e.g. Depth = 1 -> qDigits is []
                |> padDigits (depth - 1)

        q1 =
            mortonOrderToQDigits m1
                |> padDigits (depth - 1)
    in
    sharedNodeHelper ( q0, q1 ) []
        |> (\ds -> toIndex (List.length ds) (qDigitsToMortonOrder ds))


coordinateIdToMortonOrder : Int -> Int -> Int -> MortonOrder
coordinateIdToMortonOrder depth rowId colId =
    let
        rowBits =
            fromDecimal 2 rowId
                |> padDigits (depth - 1)

        colBits =
            fromDecimal 2 colId
                |> padDigits (depth - 1)
    in
    coordinateIdToMortonOrderHelper rowBits colBits []
        |> toDecimal 2


coordinateIdToMortonOrderHelper : List Digit -> List Digit -> List Digit -> List Digit
coordinateIdToMortonOrderHelper xs ys acc =
    case xs of
        [] ->
            acc

        x :: xs_ ->
            case ys of
                [] ->
                    acc

                y :: ys_ ->
                    coordinateIdToMortonOrderHelper xs_ ys_ (y :: x :: acc)


sharedNodeHelper : ( QuadraticDigits, QuadraticDigits ) -> QuadraticDigits -> QuadraticDigits
sharedNodeHelper ( xs, ys ) acc =
    -- xs and ys must be padded
    case xs of
        [] ->
            acc

        x :: [] ->
            case ys of
                [] ->
                    acc

                y :: _ ->
                    if x == y then
                        x :: acc

                    else
                        acc

        x :: xs_ ->
            case ys of
                [] ->
                    acc

                y :: [] ->
                    if x == y then
                        x :: acc

                    else
                        acc

                y :: ys_ ->
                    if x == y then
                        sharedNodeHelper ( xs_, ys_ ) (x :: acc)

                    else
                        acc


{-| Get path of given index.
Path is a list of directions from a root node to the node/leaf of the index.
Path to a root node should be empty list.
For example, index 5 points out a tile of top-left on layer 2.
The path index=5 would be root -> GoToTopLeft(D0) -> GoToTopLeft which is [](Root) ++ [D0] ++ [D0].

    toPath 5 == [ D0, D0 ]

The path index=5 would be root -> GoToTopRight(D1) -> GoToTopLeft which is [](Root) ++ [D1] ++ [D0].

    toPath 9 == [ D1, D0 ]

-}
toPath : Index2d -> List Direction2d
toPath i =
    let
        ( layerId, mOrder ) =
            fromIndex i
    in
    if layerId <= 0 then
        []

    else
        mOrder
            |> mortonOrderToQDigits
            |> padDigits layerId
            |> List.map digitToDirection


{-| Calculate index from given path.
-}
fromPath : List Direction2d -> Index2d
fromPath path =
    List.map directionToDigit path
        |> (\ds -> toIndex (List.length ds) (qDigitsToMortonOrder ds))


{-| Get index of parent node which is the farthest from a root node
and have both two nodes of given index as a (grand \* n)-child.

If given two index are same, it returns the index.
If given one index is child/parent of the other, it returns the index of closer node to root.

-}
sharedNode : Index2d -> Index2d -> Index2d
sharedNode x y =
    let
        getQDigits i =
            fromIndex i |> (\( l, m ) -> mortonOrderToQDigits m |> padDigits l)
    in
    sharedNodeHelper ( getQDigits x, getQDigits y ) []
        |> (\ds -> toIndex (List.length ds) (qDigitsToMortonOrder ds))



-- Conversion


toIndex : LayerId -> MortonOrder -> Index2d
toIndex layerId mOrder =
    sumNumItemsOfLayer layerId + mOrder


fromIndex : Index2d -> ( LayerId, MortonOrder )
fromIndex i =
    let
        layerId =
            calcLayerId i
    in
    ( layerId, i - sumNumItemsOfLayer layerId )


{-| Summation of num\_items of L\_0, L\_1, .. L\_givenLayerIdMinusOne
-}
sumNumItemsOfLayer : LayerId -> Int
sumNumItemsOfLayer l =
    (4 ^ l - 1) // 3


calcLayerId : Index2d -> LayerId
calcLayerId i =
    logBase 4 (3 * toFloat i + 1) |> floor


{-| Convert quadratic number to Morton Order(decimal).

    toDecimal [ 1, 1 ] == 5

-}
qDigitsToMortonOrder : QuadraticDigits -> MortonOrder
qDigitsToMortonOrder =
    toDecimal 4


{-| Convert Morton order (decimal) to quadratic number.

    toDecimal 5 == [ 1, 1 ]

-}
mortonOrderToQDigits : MortonOrder -> QuadraticDigits
mortonOrderToQDigits =
    fromDecimal 4


padDigits : Int -> List Digit -> List Digit
padDigits numDigits xs =
    if numDigits > 0 then
        List.repeat (max 0 (numDigits - List.length xs)) 0 ++ xs

    else
        []



-- Utils


digitToDirection : Digit -> Direction2d
digitToDirection i =
    case i of
        0 ->
            D0

        1 ->
            D1

        2 ->
            D2

        3 ->
            D3

        _ ->
            D0


directionToDigit : Direction2d -> Digit
directionToDigit d =
    case d of
        D0 ->
            0

        D1 ->
            1

        D2 ->
            2

        D3 ->
            3


toDecimal : Int -> List Int -> Int
toDecimal base digits =
    digits |> List.indexedMap (\i d -> d * base ^ i) |> List.sum


fromDecimal : Int -> Int -> List Int
fromDecimal base =
    fromDecimalHelper base []


fromDecimalHelper : Int -> List Int -> Int -> List Int
fromDecimalHelper base acc n =
    let
        ( x, d ) =
            ( n // base, remainderBy base n )

        digits =
            modBy base d :: acc
    in
    if x > 0 then
        fromDecimalHelper base digits x

    else
        digits
