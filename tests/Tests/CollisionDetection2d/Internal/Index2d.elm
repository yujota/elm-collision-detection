module Tests.CollisionDetection2d.Internal.Index2d exposing (..)

import CollisionDetection2d.Internal.Index2d exposing (..)
import Expect exposing (equal, equalLists)
import Fuzz
import Test exposing (Test, describe, fuzz, test)


testCoordinateToIndex : Test
testCoordinateToIndex =
    let
        assert { args, desired } =
            equal desired
                (coordinateToIndex
                    { depth = args.d, unitWidth = args.w, unitHeight = args.h }
                    { x0 = args.x0, y0 = args.y0, x1 = args.x1, y1 = args.y1 }
                )

        testHelper params =
            test (Debug.toString params) (\() -> assert params)
    in
    describe "test Index.CoordinateToIndex"
        [ testHelper { args = { d = 1, w = 10, h = 10, x0 = 3, y0 = 4, x1 = 5, y1 = 6 }, desired = 0 }
        ]


testCoordinateIdToMortonOrder : Test
testCoordinateIdToMortonOrder =
    let
        assert { args, desired } =
            equal desired (coordinateIdToMortonOrder args.depth args.colId args.rowId)

        testHelper params =
            test (Debug.toString params) (\() -> assert params)
    in
    describe "test Index.CoordinateIdToMortonOrder"
        [ testHelper { args = { depth = 1, rowId = 0, colId = 0 }, desired = 0 }
        , testHelper { args = { depth = 2, rowId = 0, colId = 0 }, desired = 0 }
        , testHelper { args = { depth = 2, rowId = 1, colId = 0 }, desired = 1 }
        , testHelper { args = { depth = 2, rowId = 1, colId = 1 }, desired = 3 }
        , testHelper { args = { depth = 3, rowId = 2, colId = 1 }, desired = 6 }
        ]


testSharedNode : Test
testSharedNode =
    let
        assert { args, desired } =
            equal desired (sharedNode args.i0 args.i1)

        testHelper params =
            test (Debug.toString params) (\() -> assert params)
    in
    describe "test Index.sharedNode"
        [ testHelper { args = { i0 = 0, i1 = 0 }, desired = 0 }
        , testHelper { args = { i0 = 1, i1 = 2 }, desired = 0 }
        , testHelper { args = { i0 = 2, i1 = 9 }, desired = 2 }
        , testHelper { args = { i0 = 3, i1 = 11 }, desired = 0 }
        , testHelper { args = { i0 = 37, i1 = 48 }, desired = 2 }
        , fuzz
            (Fuzz.intRange 0 1000)
            "Shared Node with Root is always root"
            (\i -> assert { args = { i0 = i, i1 = 0 }, desired = 0 })
        ]


testToPath : Test
testToPath =
    let
        assert { args, desired } =
            equalLists desired (toPath args.index)

        testHelper params =
            test (Debug.toString params) (\() -> assert params)
    in
    describe "test Index.toPath"
        [ testHelper { args = { index = 0 }, desired = [] }
        , testHelper { args = { index = 1 }, desired = [ D0 ] }
        , testHelper { args = { index = 4 }, desired = [ D3 ] }
        , testHelper { args = { index = 9 }, desired = [ D1, D0 ] }
        , testHelper { args = { index = 48 }, desired = [ D1, D2, D3 ] }
        ]


testFromIndex : Test
testFromIndex =
    let
        assert { args, desired } =
            equal desired (fromIndex args.index)

        testHelper params =
            test (Debug.toString params) (\() -> assert params)
    in
    describe "test Index.fromIndex"
        [ testHelper { args = { index = 0 }, desired = ( 0, 0 ) }
        , testHelper { args = { index = 1 }, desired = ( 1, 0 ) }
        , testHelper { args = { index = 9 }, desired = ( 2, 4 ) }
        , fuzz
            (Fuzz.intRange 0 1000)
            "fromIndex >> toIndex == identity"
            (\i -> fromIndex i |> (\( l, m ) -> toIndex l m |> equal i))
        ]
