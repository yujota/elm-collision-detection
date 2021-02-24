module Tests.CollisionDetection2d.Internal.FixedDepthTree2d exposing (..)

import BoundingBox2d
import CollisionDetection2d.Internal.FixedDepthTree2d exposing (..)
import Dict exposing (Dict)
import Expect exposing (Expectation, equal, equalLists)
import Pixels
import Point2d
import Test exposing (Test, describe, fuzz, test)


testCollidedPairs : Test
testCollidedPairs =
    let
        assert { boxes, desired } =
            equalCollidedPairs desired
                (empty defaultDepth constraint
                    |> (\ctn -> List.foldl (\( k, v ) ct -> insert k v ct) ctn boxes)
                    |> collidedPairs
                )

        testHelper params =
            test (Debug.toString params) (\() -> assert params)

        constraint =
            { range = { minX = 0, minY = 0, maxX = 100, maxY = 100 }
            , boundingBox = getBoundingBox
            , isCollided = BoundingBox2d.intersects
            }

        genBox a b c d =
            BoundingBox2d.from (Point2d.pixels a b) (Point2d.pixels c d)

        getBoundingBox =
            BoundingBox2d.extrema
                >> (\r ->
                        { minX = Pixels.inPixels r.minX
                        , minY = Pixels.inPixels r.minY
                        , maxX = Pixels.inPixels r.maxX
                        , maxY = Pixels.inPixels r.maxY
                        }
                   )

        box0 =
            genBox 10 10 40 40

        box1 =
            genBox 20 20 60 60

        box2 =
            genBox 50 50 80 80

        box3 =
            genBox 5 5 90 90
    in
    describe "test FixedDepthTree2d.collidedPairs with simple arguments"
        [ testHelper { boxes = [], desired = [] }
        , testHelper { boxes = [ ( 0, box0 ), ( 1, box1 ) ], desired = [ ( ( 0, box0 ), ( 1, box1 ) ) ] }
        , testHelper
            { boxes = [ ( 0, box0 ), ( 1, box1 ), ( 2, box2 ) ]
            , desired =
                [ ( ( 0, box0 ), ( 1, box1 ) )
                , ( ( 1, box1 ), ( 2, box2 ) )
                ]
            }
        , testHelper { boxes = [ ( 0, box0 ), ( 2, box2 ) ], desired = [] }
        , testHelper
            { boxes = [ ( 0, box0 ), ( 1, box1 ), ( 2, box2 ), ( 3, box3 ) ]
            , desired =
                [ ( ( 0, box0 ), ( 1, box1 ) )
                , ( ( 0, box0 ), ( 3, box3 ) )
                , ( ( 1, box1 ), ( 2, box2 ) )
                , ( ( 1, box1 ), ( 3, box3 ) )
                , ( ( 2, box2 ), ( 3, box3 ) )
                ]
            }
        ]


equalCollidedPairs :
    List ( ( comparable, a ), ( comparable, a ) )
    -> List ( ( comparable, a ), ( comparable, a ) )
    -> Expectation
equalCollidedPairs xs ys =
    Expect.equalDicts (pairsToDict xs) (pairsToDict ys)


pairsToDict : List ( ( comparable, a ), ( comparable, a ) ) -> Dict comparable ( a, Dict comparable a )
pairsToDict xs =
    pairsToDictHelper xs Dict.empty


pairsToDictHelper :
    List ( ( comparable, a ), ( comparable, a ) )
    -> Dict comparable ( a, Dict comparable a )
    -> Dict comparable ( a, Dict comparable a )
pairsToDictHelper xs acc =
    let
        f v kX kV maybeItem =
            case maybeItem of
                Just ( v_, d ) ->
                    Just ( v_, Dict.insert kX kV d )

                Nothing ->
                    Just ( v, Dict.singleton kX kV )
    in
    case xs of
        [] ->
            acc

        ( ( k0, v0 ), ( k1, v1 ) ) :: xs_ ->
            pairsToDictHelper xs_ (acc |> Dict.update k0 (f v0 k1 v1) |> Dict.update k1 (f v1 k0 v0))
