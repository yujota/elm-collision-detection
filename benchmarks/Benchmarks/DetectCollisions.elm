module Benchmarks.DetectCollisions exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BoundingBox2d
import CollisionDetection2d
import Pixels
import Point2d
import Random


main : BenchmarkProgram
main =
    program suite


initialSeed =
    202005011033
        |> Random.initialSeed


numBoxes =
    400


boundary =
    { minX = 0, minY = 0, maxX = 511, maxY = 511 }


boxes =
    List.range 0 (numBoxes - 1)
        |> List.foldr generateBoxes { seed = initialSeed, result = [] }
        |> .result


naive =
    CollisionDetection2d.naive { extrema = extrema, intersects = intersects, getBoundingBox = identity }
        |> (\c -> List.foldl insert c boxes)


quadTree =
    CollisionDetection2d.quadTree
        { extrema = extrema
        , intersects = intersects
        , boundary = boundary
        , getBoundingBox = identity
        }
        |> (\c -> List.foldl insert c boxes)


customQuadTree =
    CollisionDetection2d.customQuadTree
        { extrema = extrema
        , intersects = intersects
        , getBoundingBox = identity
        , boundary = boundary
        , unitWidth = 128
        , unitHeight = 128
        , depth = 3
        }
        |> (\c -> List.foldl insert c boxes)


insert ( i, bBox ) cnt =
    CollisionDetection2d.insert i bBox cnt


suite : Benchmark
suite =
    Benchmark.describe "QuadTree.quadKey"
        [ Benchmark.compare
            "compare detectCollisions"
            "naive"
            (\_ -> CollisionDetection2d.detectCollisions noCheck naive)
            "quadTree"
            (\_ -> CollisionDetection2d.detectCollisions noCheck quadTree)
        ]



-- Helpers


boundingBoxGenerator =
    Random.map4 (\a b c d -> BoundingBox2d.from (Point2d.pixels a b) (Point2d.pixels c d))
        (Random.float boundary.minX boundary.maxX)
        (Random.float boundary.minY boundary.maxY)
        (Random.float boundary.minX boundary.maxX)
        (Random.float boundary.minY boundary.maxY)


generateBoxes i { seed, result } =
    let
        ( bBox, newSeed ) =
            Random.step boundingBoxGenerator seed
    in
    { seed = newSeed, result = ( i, bBox ) :: result }


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
    let
        a =
            fib 100 0 0
    in
    a /= 0


fib n a b =
    if n > 0 then
        fib (n - 1) (a + b) a

    else
        a
