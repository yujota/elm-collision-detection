module Tests.CollisionDetection2d exposing (..)

import BoundingBox2d
import CollisionDetection2d
import Expect exposing (equalLists)
import Fuzz
import Pixels
import Point2d
import Test exposing (Test, describe, test)


testCollisionDetection : Test
testCollisionDetection =
    let
        testExampleOne () =
            let
                boundary =
                    genBoundary 0 0 100 100

                bBox1 =
                    genBoundingBox 10 10 40 40

                bBox2 =
                    genBoundingBox 20 20 50 50

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 1 bBox1
                        |> CollisionDetection2d.insert 2 bBox2

                actual =
                    CollisionDetection2d.detectCollisions noCheck container
                        |> List.map (\( a, b ) -> sortKey ( a.key, b.key ))

                desired =
                    [ ( 1, 2 ) ]
            in
            equalLists actual desired

        testExampleTwo () =
            let
                boundary =
                    genBoundary 0 0 100 100

                bBox1 =
                    genBoundingBox 10 10 80 80

                bBox2 =
                    genBoundingBox 20 20 50 50

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 1 bBox1
                        |> CollisionDetection2d.insert 2 bBox2

                actual =
                    CollisionDetection2d.detectCollisions noCheck container
                        |> List.map (\( a, b ) -> sortKey ( a.key, b.key ))

                desired =
                    [ ( 1, 2 ) ]
            in
            equalLists actual desired
    in
    describe "test CollisionDetection2d.detectCollisions with simple arguments"
        [ test "test Example 1" testExampleOne
        , test "test Example 2" testExampleTwo
        ]


testCollisionDetectionForEdgeCases : Test
testCollisionDetectionForEdgeCases =
    let
        testExampleOne () =
            let
                boundary =
                    genBoundary 0 0 2000 2000

                v =
                    0.000001

                bBox1 =
                    genBoundingBox v v v v

                bBox2 =
                    genBoundingBox v v v v

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 1 bBox1
                        |> CollisionDetection2d.insert 2 bBox2

                actual =
                    CollisionDetection2d.detectCollisions noCheck container
                        |> List.map (\( a, b ) -> sortKey ( a.key, b.key ))

                actualNaive =
                    CollisionDetection2d.naive { extrema = extrema, intersects = intersects, getBoundingBox = identity }
                        |> CollisionDetection2d.insert 1 bBox1
                        |> CollisionDetection2d.insert 2 bBox2
                        |> CollisionDetection2d.detectCollisions noCheck
                        |> List.map (\( a, b ) -> sortKey ( a.key, b.key ))

                desired =
                    [ ( 1, 2 ) ]
            in
            equalLists desired actual
    in
    describe "test edge cases for CollisionDetection2d.detectCollisions"
        [ test "test Example 1" testExampleOne
        ]


fuzzTestCollisionDetection : Test
fuzzTestCollisionDetection =
    let
        testOneBox bBox =
            let
                boundary =
                    genBoundary 0 0 2000 2000

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 0 bBox

                actual =
                    CollisionDetection2d.detectCollisions noCheck container
                        |> List.map (\( a, b ) -> sortKey ( a.key, b.key ))

                desired =
                    []
            in
            equalLists actual desired

        testTwoBox bBox1 bBox2 =
            let
                boundary =
                    genBoundary 0 0 2000 2000

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 0 bBox1
                        |> CollisionDetection2d.insert 1 bBox2

                actual =
                    CollisionDetection2d.detectCollisions noCheck container
                        |> List.map (\( a, b ) -> sortKey ( a.key, b.key ))

                desired =
                    CollisionDetection2d.naive { extrema = extrema, intersects = intersects, getBoundingBox = identity }
                        |> CollisionDetection2d.insert 0 bBox1
                        |> CollisionDetection2d.insert 1 bBox2
                        |> CollisionDetection2d.detectCollisions noCheck
                        |> List.map (\( a, b ) -> sortKey ( a.key, b.key ))
            in
            equalLists desired actual

        testThreeBox bBox1 bBox2 bBox3 =
            let
                boundary =
                    genBoundary 0 0 2000 2000

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 0 bBox1
                        |> CollisionDetection2d.insert 1 bBox2
                        |> CollisionDetection2d.insert 2 bBox3

                actual =
                    CollisionDetection2d.detectCollisions noCheck container
                        |> List.map (\( a, b ) -> sortKey ( a.key, b.key ))
                        |> List.sort

                desired =
                    CollisionDetection2d.naive { extrema = extrema, intersects = intersects, getBoundingBox = identity }
                        |> CollisionDetection2d.insert 0 bBox1
                        |> CollisionDetection2d.insert 1 bBox2
                        |> CollisionDetection2d.insert 2 bBox3
                        |> CollisionDetection2d.detectCollisions noCheck
                        |> List.map (\( a, b ) -> sortKey ( a.key, b.key ))
                        |> List.sort
            in
            equalLists desired actual
    in
    describe "fuzz-test CollisionDetection2d.detectCollisions"
        [ Test.fuzz (boundingBoxFuzzer 2000 2000) "test one-box" testOneBox
        , Test.fuzz2 (boundingBoxFuzzer 2000 2000) (boundingBoxFuzzer 2000 2000) "test two-box" testTwoBox
        , Test.fuzz3
            (boundingBoxFuzzer 2000 2000)
            (boundingBoxFuzzer 2000 2000)
            (boundingBoxFuzzer 2000 2000)
            "test three-box"
            testThreeBox
        ]


testCollideWith : Test
testCollideWith =
    let
        testExampleOne () =
            let
                boundary =
                    genBoundary 0 0 100 100

                bBox1 =
                    genBoundingBox 10 10 40 40

                targetArea =
                    genBoundingBox 20 20 50 50

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 1 bBox1

                actual =
                    CollisionDetection2d.collideWith (always True) targetArea container
                        |> List.map .key

                desired =
                    [ 1 ]
            in
            equalLists actual desired

        testExampleTwo () =
            let
                boundary =
                    genBoundary 0 0 100 100

                bBox1 =
                    genBoundingBox 10 10 80 80

                bBox2 =
                    genBoundingBox 20 20 50 50

                targetArea =
                    genBoundingBox 0 0 90 90

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 1 bBox1
                        |> CollisionDetection2d.insert 2 bBox2

                actual =
                    CollisionDetection2d.collideWith (always True) targetArea container
                        |> List.map .key

                desired =
                    [ 1, 2 ]
            in
            equalLists actual desired
    in
    describe "test CollisionDetection2d.collideWith with simple arguments"
        [ test "test Example 1" testExampleOne
        , test "test Example 2" testExampleTwo
        ]


fuzzTestCollideWith : Test
fuzzTestCollideWith =
    let
        testOneBox bBox targetBox =
            let
                boundary =
                    genBoundary 0 0 2000 2000

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 0 bBox

                actual =
                    CollisionDetection2d.collideWith (always True) targetBox container
                        |> List.map .key
                        |> List.sort

                desired =
                    CollisionDetection2d.naive { extrema = extrema, intersects = intersects, getBoundingBox = identity }
                        |> CollisionDetection2d.insert 0 bBox
                        |> CollisionDetection2d.collideWith (always True) targetBox
                        |> List.map .key
                        |> List.sort
            in
            equalLists actual desired

        testTwoBox bBox1 bBox2 targetBox =
            let
                boundary =
                    genBoundary 0 0 2000 2000

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 0 bBox1
                        |> CollisionDetection2d.insert 1 bBox2

                actual =
                    CollisionDetection2d.collideWith (always True) targetBox container
                        |> List.map .key
                        |> List.sort

                desired =
                    CollisionDetection2d.naive { extrema = extrema, intersects = intersects, getBoundingBox = identity }
                        |> CollisionDetection2d.insert 0 bBox1
                        |> CollisionDetection2d.insert 1 bBox2
                        |> CollisionDetection2d.collideWith (always True) targetBox
                        |> List.map .key
                        |> List.sort
            in
            equalLists desired actual
    in
    describe "fuzz-test CollisionDetection2d.collideWith"
        [ Test.fuzz2 (boundingBoxFuzzer 2000 2000) (boundingBoxFuzzer 2000 2000) "test two-box" testOneBox
        , Test.fuzz3
            (boundingBoxFuzzer 2000 2000)
            (boundingBoxFuzzer 2000 2000)
            (boundingBoxFuzzer 2000 2000)
            "test three-box"
            testTwoBox
        ]


testInsert : Test
testInsert =
    let
        testExampleOne () =
            let
                boundary =
                    genBoundary 0 0 100 100

                bBox0 =
                    genBoundingBox 5 5 20 20

                bBox1 =
                    genBoundingBox 10 10 20 20

                bBox2 =
                    genBoundingBox 80 80 90 90

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 0 bBox0
                        |> CollisionDetection2d.insert 1 bBox1

                updatedContainer =
                    CollisionDetection2d.insert 1 bBox2 container

                checkKeys =
                    CollisionDetection2d.keys >> List.sort >> equalLists [ 0, 1 ]

                checkCollided =
                    CollisionDetection2d.collideWith (always True) (genBoundingBox 0 0 100 100)
                        >> List.sortBy .key
                        >> equalLists [ { key = 0, object = bBox0 }, { key = 1, object = bBox2 } ]
            in
            Expect.all [ checkKeys, checkCollided ] updatedContainer

        testExampleTwo () =
            let
                boundary =
                    genBoundary 0 0 100 100

                bBox0 =
                    genBoundingBox 5 5 20 20

                bBox1 =
                    genBoundingBox 10 10 20 20

                bBox2 =
                    genBoundingBox 80 80 90 90

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 0 bBox0
                        |> CollisionDetection2d.insert 2 bBox2

                updatedContainer =
                    CollisionDetection2d.insert 0 bBox1 container

                checkKeys =
                    CollisionDetection2d.keys >> List.sort >> equalLists [ 0, 2 ]

                checkCollided =
                    CollisionDetection2d.collideWith (always True) (genBoundingBox 0 0 100 100)
                        >> List.sortBy .key
                        >> equalLists [ { key = 0, object = bBox1 }, { key = 2, object = bBox2 } ]
            in
            Expect.all [ checkKeys, checkCollided ] updatedContainer

        testExampleThree () =
            let
                boundary =
                    genBoundary 0 0 100 100

                bBox0 =
                    genBoundingBox 5 5 20 20

                bBox1 =
                    genBoundingBox 10 10 20 20

                bBox2 =
                    genBoundingBox 80 80 90 90

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 0 bBox0
                        |> CollisionDetection2d.insert 0 bBox1
                        |> CollisionDetection2d.insert 0 bBox2

                checkKeys =
                    CollisionDetection2d.keys >> List.sort >> equalLists [ 0 ]

                checkCollided =
                    CollisionDetection2d.collideWith (always True) (genBoundingBox 0 0 100 100)
                        >> List.sortBy .key
                        >> equalLists [ { key = 0, object = bBox2 } ]
            in
            Expect.all [ checkKeys, checkCollided ] container
    in
    describe "test CollisionDetection2d.insert"
        [ test "test Example 1" testExampleOne
        , test "test Example 2" testExampleTwo
        , test "test Example 3" testExampleThree
        ]


fuzzTestInsert : Test
fuzzTestInsert =
    let
        testDuplicateInsertion bBox1 bBox2 =
            let
                boundary =
                    genBoundary 0 0 2000 2000

                targetBox =
                    genBoundingBox 0 0 2000 2000

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 0 bBox1
                        |> CollisionDetection2d.insert 0 bBox2

                actual =
                    CollisionDetection2d.collideWith (always True) targetBox container
                        |> List.map .key
                        |> List.sort

                desired =
                    CollisionDetection2d.naive { extrema = extrema, intersects = intersects, getBoundingBox = identity }
                        |> CollisionDetection2d.insert 0 bBox1
                        |> CollisionDetection2d.insert 0 bBox2
                        |> CollisionDetection2d.collideWith (always True) targetBox
                        |> List.map .key
                        |> List.sort
            in
            equalLists desired actual

        testDuplicateInsertionTwo bBox1 bBox2 bBox3 =
            let
                boundary =
                    genBoundary 0 0 2000 2000

                targetBox =
                    genBoundingBox 0 0 2000 2000

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 0 bBox1
                        |> CollisionDetection2d.insert 1 bBox2
                        |> CollisionDetection2d.insert 0 bBox3

                actual =
                    CollisionDetection2d.collideWith (always True) targetBox container
                        |> List.map .key
                        |> List.sort

                desired =
                    CollisionDetection2d.naive { extrema = extrema, intersects = intersects, getBoundingBox = identity }
                        |> CollisionDetection2d.insert 0 bBox1
                        |> CollisionDetection2d.insert 1 bBox2
                        |> CollisionDetection2d.insert 0 bBox3
                        |> CollisionDetection2d.collideWith (always True) targetBox
                        |> List.map .key
                        |> List.sort
            in
            equalLists desired actual

        testTripleInsertion bBox1 bBox2 bBox3 =
            let
                boundary =
                    genBoundary 0 0 2000 2000

                targetBox =
                    genBoundingBox 0 0 2000 2000

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 0 bBox1
                        |> CollisionDetection2d.insert 0 bBox2
                        |> CollisionDetection2d.insert 0 bBox3

                actual =
                    CollisionDetection2d.collideWith (always True) targetBox container
                        |> List.map .key
                        |> List.sort

                desired =
                    CollisionDetection2d.naive { extrema = extrema, intersects = intersects, getBoundingBox = identity }
                        |> CollisionDetection2d.insert 0 bBox1
                        |> CollisionDetection2d.insert 0 bBox2
                        |> CollisionDetection2d.insert 0 bBox3
                        |> CollisionDetection2d.collideWith (always True) targetBox
                        |> List.map .key
                        |> List.sort
            in
            equalLists desired actual
    in
    describe "fuzz-test CollisionDetection2d.insert"
        [ Test.fuzz2
            (boundingBoxFuzzer 2000 2000)
            (boundingBoxFuzzer 2000 2000)
            "test two-box for same key"
            testDuplicateInsertion
        , Test.fuzz3
            (boundingBoxFuzzer 2000 2000)
            (boundingBoxFuzzer 2000 2000)
            (boundingBoxFuzzer 2000 2000)
            "test two-box for same key with one other box"
            testDuplicateInsertionTwo
        , Test.fuzz3
            (boundingBoxFuzzer 2000 2000)
            (boundingBoxFuzzer 2000 2000)
            (boundingBoxFuzzer 2000 2000)
            "test three-box for same key"
            testTripleInsertion
        ]


testRemove : Test
testRemove =
    let
        testExampleOne () =
            let
                boundary =
                    genBoundary 0 0 100 100

                bBox1 =
                    genBoundingBox 10 10 40 40

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 1 bBox1

                removedContainer =
                    CollisionDetection2d.remove 1 container

                hasNoKeys =
                    equalLists [] (CollisionDetection2d.keys removedContainer)

                noItemsCollided =
                    equalLists []
                        (CollisionDetection2d.collideWith (always True) (genBoundingBox 0 0 100 100) removedContainer)
            in
            Expect.all [ always hasNoKeys, always noItemsCollided ] ()

        testExampleTwo () =
            let
                boundary =
                    genBoundary 0 0 100 100

                bBox1 =
                    genBoundingBox 10 10 40 40

                bBox2 =
                    genBoundingBox 15 15 40 40

                container =
                    CollisionDetection2d.quadTree
                        { extrema = extrema
                        , boundary = boundary
                        , intersects = intersects
                        , getBoundingBox = identity
                        }
                        |> CollisionDetection2d.insert 1 bBox1
                        |> CollisionDetection2d.insert 2 bBox2

                removedContainer =
                    CollisionDetection2d.remove 1 container

                hasNoKeys =
                    equalLists [ 2 ] (CollisionDetection2d.keys removedContainer)

                noItemsCollided =
                    equalLists [ { key = 2, object = bBox2 } ]
                        (CollisionDetection2d.collideWith (always True) (genBoundingBox 0 0 100 100) removedContainer)
            in
            Expect.all [ always hasNoKeys, always noItemsCollided ] ()
    in
    describe "test CollisionDetection2d.remove"
        [ test "test Example 1" testExampleOne
        , test "test Example 2" testExampleTwo
        ]



-- Helpers


boundingBoxFuzzer maxX maxY =
    Fuzz.map4
        genBoundingBox
        (Fuzz.floatRange 0 maxX)
        (Fuzz.floatRange 0 maxY)
        (Fuzz.floatRange 0 maxX)
        (Fuzz.floatRange 0 maxY)


genBoundary a b c d =
    { minX = a, minY = b, maxX = c, maxY = d }


genBoundingBox a b c d =
    BoundingBox2d.from (Point2d.pixels a b) (Point2d.pixels c d)


extrema =
    BoundingBox2d.extrema
        >> (\r ->
                { minX = Pixels.inPixels r.minX
                , minY = Pixels.inPixels r.minY
                , maxX = Pixels.inPixels r.maxX
                , maxY = Pixels.inPixels r.maxY
                }
           )


intersects =
    BoundingBox2d.intersects


noCheck _ _ =
    True


sortKey ( a, b ) =
    case compare a b of
        LT ->
            ( a, b )

        EQ ->
            ( a, b )

        GT ->
            ( b, a )
