module Metrics exposing (Metrics, averageFps, init, update)

import Array exposing (Array)


type alias Metrics =
    { averageFps : Float
    , evalTimer : Float -- Evaluate fps when this value is less than equal to 0.
    , animationDeltas : Array Float
    }


init : () -> Metrics
init _ =
    { averageFps = 0
    , evalTimer = resetEvalTimer
    , animationDeltas = Array.empty
    }


resetEvalTimer : Float
resetEvalTimer =
    200


update : Float -> Metrics -> Metrics
update delta m =
    let
        deltas =
            if Array.length m.animationDeltas >= 20 then
                Array.slice 1 20 m.animationDeltas |> Array.push delta

            else
                m.animationDeltas |> Array.push delta
    in
    { m | animationDeltas = deltas }
        |> (\met ->
                if met.evalTimer < 0 then
                    { m
                        | averageFps =
                            1000 / (Array.foldr (+) 0 deltas / toFloat (Array.length deltas))
                        , evalTimer = 500
                    }

                else
                    { m | evalTimer = m.evalTimer - delta }
           )


averageFps : Metrics -> Float
averageFps =
    .averageFps
