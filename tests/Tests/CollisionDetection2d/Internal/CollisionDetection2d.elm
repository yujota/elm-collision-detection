module Tests.CollisionDetection2d.Internal.CollisionDetection2d exposing (..)

import Binary
import BoundingBox2d
import CollisionDetection2d.Internal.CollisionDetection2d as C
import Expect exposing (equal, equalLists)
import Pixels
import Point2d
import Test exposing (Test, describe, test)


testQuadKey : Test
testQuadKey =
    let
        testExampleOne () =
            let
                actual =
                    C.quadKey { unitWidth = 13, unitHeight = 13 } { x = 48, y = 80 }
                        |> Binary.toDecimal

                desired =
                    45
            in
            equal actual desired
    in
    describe "test QuadTree.Internal.QuadTree.quadKey" [ test "test Example 1" testExampleOne ]


testLinerQuaternaryTreeIndex : Test
testLinerQuaternaryTreeIndex =
    let
        testExampleOne () =
            let
                extrema =
                    { left = 10, top = 20, right = 70, bottom = 80 }

                actual =
                    C.linerQuaternaryTreeIndex { depth = 2, unitWidth = 100, unitHeight = 100 } extrema

                desired =
                    1
            in
            equal actual desired

        testExampleTwo () =
            let
                extrema =
                    { left = 10, top = 20, right = 70, bottom = 80 }

                actual =
                    C.linerQuaternaryTreeIndex { depth = 2, unitWidth = 50, unitHeight = 50 } extrema

                desired =
                    0
            in
            equal actual desired

        testExampleThree () =
            let
                extrema =
                    { left = 55, top = 15, right = 75, bottom = 35 }

                actual =
                    C.linerQuaternaryTreeIndex { depth = 4, unitWidth = 10, unitHeight = 10 } extrema

                desired =
                    2
            in
            equal actual desired

        testExampleFour () =
            let
                extrema =
                    { left = 65, top = 25, right = 75, bottom = 35 }

                actual =
                    C.linerQuaternaryTreeIndex { depth = 4, unitWidth = 10, unitHeight = 10 } extrema

                desired =
                    12
            in
            equal actual desired

        testExampleFive () =
            let
                extrema =
                    { left = 32, top = 12, right = 37, bottom = 18 }

                actual =
                    C.linerQuaternaryTreeIndex { depth = 4, unitWidth = 10, unitHeight = 10 } extrema

                desired =
                    28
            in
            equal actual desired

        testExampleSix () =
            let
                extrema =
                    { left = 35, top = 35, right = 45, bottom = 45 }

                actual =
                    C.linerQuaternaryTreeIndex { depth = 4, unitWidth = 10, unitHeight = 10 } extrema

                desired =
                    0
            in
            equal actual desired

        testExampleSeven () =
            let
                extrema =
                    { left = 15, top = 65, right = 25, bottom = 65 }

                actual =
                    C.linerQuaternaryTreeIndex { depth = 4, unitWidth = 10, unitHeight = 10 } extrema

                desired =
                    3
            in
            equal actual desired
    in
    describe "test QuadTree.Internal.QuadTree.linerQuaternaryTreeIndex"
        [ test "test Example 1" testExampleOne
        , test "test Example 2" testExampleTwo
        , test "test Example 3" testExampleThree
        , test "test Example 4" testExampleFour
        , test "test Example 5" testExampleFive
        , test "test Example 6" testExampleSix
        , test "test Example 7" testExampleSeven
        ]


testParentToRootLevelLqtIndices : Test
testParentToRootLevelLqtIndices =
    let
        testExampleOne () =
            let
                actual =
                    C.parentToRootLevelLqtIndices { depth = 2 } 0

                desired =
                    []
            in
            equalLists actual desired

        testExampleTwo () =
            let
                actual =
                    C.parentToRootLevelLqtIndices { depth = 2 } 1

                desired =
                    [ 0 ]
            in
            equalLists actual desired

        testExampleThree () =
            let
                actual =
                    C.parentToRootLevelLqtIndices { depth = 3 } 5

                desired =
                    [ 0, 1 ]
            in
            equalLists actual desired

        testExampleFour () =
            let
                actual =
                    C.parentToRootLevelLqtIndices { depth = 4 } 47

                desired =
                    [ 0, 2, 11 ]
            in
            equalLists actual desired

        testExampleFive () =
            let
                actual =
                    C.parentToRootLevelLqtIndices { depth = 2 } 4

                desired =
                    [ 0 ]
            in
            equalLists actual desired
    in
    describe "test QuadTree.Internal.QuadTree.parentToRootLevelLqtIndices"
        [ test "test Example 1" testExampleOne
        , test "test Example 2" testExampleTwo
        , test "test Example 3" testExampleThree
        , test "test Example 4" testExampleFour
        , test "test Example 5" testExampleFive
        ]


testContainedLqtIndices : Test
testContainedLqtIndices =
    let
        extrema =
            BoundingBox2d.extrema
                >> (\r ->
                        { minX = Pixels.inPixels r.minX
                        , minY = Pixels.inPixels r.minY
                        , maxX = Pixels.inPixels r.maxX
                        , maxY = Pixels.inPixels r.maxY
                        }
                   )

        testExampleOne () =
            let
                options =
                    { extrema = extrema
                    , truncateX = identity
                    , truncateY = identity
                    , unitWidth = 100
                    , unitHeight = 100
                    , depth = 2
                    }

                boundingBox =
                    BoundingBox2d.from (Point2d.pixels 10 20) (Point2d.pixels 70 80)

                actual =
                    C.containedLqtIndices options boundingBox

                desired =
                    [ 0, 1 ]
            in
            equalLists actual desired

        testExampleTwo () =
            let
                options =
                    { extrema = extrema
                    , truncateX = identity
                    , truncateY = identity
                    , unitWidth = 100
                    , unitHeight = 100
                    , depth = 3
                    }

                boundingBox =
                    BoundingBox2d.from (Point2d.pixels 10 20) (Point2d.pixels 70 80)

                actual =
                    C.containedLqtIndices options boundingBox

                desired =
                    [ 0, 1, 5 ]
            in
            equalLists actual desired

        testExampleThree () =
            let
                options =
                    { extrema = extrema
                    , truncateX = identity
                    , truncateY = identity
                    , unitWidth = 10
                    , unitHeight = 10
                    , depth = 3
                    }

                boundingBox =
                    BoundingBox2d.from (Point2d.pixels 15 15) (Point2d.pixels 25 15)

                actual =
                    C.containedLqtIndices options boundingBox

                desired =
                    [ 0, 1, 2, 8, 11 ]
            in
            equalLists actual desired
    in
    describe "test QuadTree.Internal.QuadTree.containedLqtIndices"
        [ test "test Example 1" testExampleOne
        , test "test Example 2" testExampleTwo
        , test "test Example 3" testExampleThree
        ]