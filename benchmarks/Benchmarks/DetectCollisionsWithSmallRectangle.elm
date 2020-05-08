module Benchmarks.DetectCollisionsWithSmallRectangle exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BoundingBox2d
import CollisionDetection2d
import Pixels
import Point2d
import Random


params =
    { num = 200, boxSize = 20, areaSize = 512 }


main : BenchmarkProgram
main =
    program suite


initialSeed =
    202005011033
        |> Random.initialSeed


boundary =
    { minX = 0, minY = 0, maxX = params.areaSize - 1, maxY = params.areaSize - 1 }


boxes =
    List.range 0 (params.num - 1)
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
        , cellWidth = 128
        , cellHeight = 128
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


boundingSmallBoxGenerator =
    Random.map2
        (\a b ->
            BoundingBox2d.withDimensions
                ( Pixels.pixels params.boxSize, Pixels.pixels params.boxSize )
                (Point2d.pixels a b)
        )
        (Random.float (boundary.minX + params.boxSize / 2) (boundary.maxX + params.boxSize / 2))
        (Random.float (boundary.minX + params.boxSize / 2) (boundary.maxX + params.boxSize / 2))


generateBoxes i { seed, result } =
    let
        ( bBox, newSeed ) =
            Random.step boundingSmallBoxGenerator seed
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
