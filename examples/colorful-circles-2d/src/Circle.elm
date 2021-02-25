module Circle exposing
    ( Circle
    , generator
    , getBoundingBox
    , isCollided
    , reactToCollisionAndWallBound
    , render
    , updatePosition
    )

import Axis2d
import BoundingBox2d
import Canvas
import Canvas.Settings as CSettings
import Color exposing (Color)
import Duration exposing (Duration, Seconds)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity
import Random
import Vector2d exposing (Vector2d)


type CanvasSystem
    = CanvasSystem


type alias PixelsPerSecond =
    Quantity.Rate Pixels Seconds


type alias Velocity =
    Vector2d PixelsPerSecond CanvasSystem


type alias Position =
    Point2d Pixels CanvasSystem


type alias Circle =
    { position : Position
    , velocity : Velocity
    , radius : Float
    , color : Color
    , collidedTime : Float
    }


getBoundingBox : Circle -> { minX : Float, maxX : Float, minY : Float, maxY : Float }
getBoundingBox { radius, position } =
    BoundingBox2d.withDimensions ( Pixels.pixels radius, Pixels.pixels radius ) position
        |> BoundingBox2d.extrema
        |> (\r ->
                { minX = Pixels.inPixels r.minX
                , minY = Pixels.inPixels r.minY
                , maxX = Pixels.inPixels r.maxX
                , maxY = Pixels.inPixels r.maxY
                }
           )


isCollided : Circle -> Circle -> Bool
isCollided c1 c2 =
    Point2d.equalWithin (c1.radius + c2.radius |> Pixels.pixels) c1.position c2.position


updatePosition : Float -> Circle -> Circle
updatePosition t ({ velocity, position } as c) =
    { c | position = Vector2d.for (Duration.milliseconds t) velocity |> (\v -> Point2d.translateBy v position) }


reactToCollisionAndWallBound :
    { canvasWidth : Float, canvasHeight : Float, timeStamp : Float }
    -> List ( Int, Circle )
    -> b
    -> Circle
    -> Circle
reactToCollisionAndWallBound { timeStamp, canvasWidth, canvasHeight } collided _ ({ velocity, position } as c) =
    boundOnWall { canvasWidth = canvasWidth, canvasHeight = canvasHeight } <|
        case collided of
            [] ->
                c

            ( _, targetCircle ) :: _ ->
                { c | collidedTime = timeStamp }


reactToCollision : Circle -> Circle -> Circle
reactToCollision otherC targetC =
    let
        e =
            0.97

        vecC : Velocity
        vecC =
            Vector2d.from targetC.position otherC.position
                |> Vector2d.per Duration.second

        vecCLength =
            Vector2d.length vecC |> Quantity.unwrap

        mTarget =
            targetC.radius ^ 2

        mOther =
            otherC.radius ^ 2

        dotResult =
            Vector2d.dot (Vector2d.minus targetC.velocity otherC.velocity) vecC
                |> Quantity.unwrap
                |> (\x -> x / vecCLength)
    in
    ((1 + e) * mOther)
        / ((mTarget + mOther) * vecCLength)
        * dotResult
        |> (\s -> Vector2d.scaleBy s vecC)
        |> Vector2d.plus targetC.velocity
        |> (\v -> { targetC | velocity = v })


boundOnWall : { canvasWidth : Float, canvasHeight : Float } -> Circle -> Circle
boundOnWall { canvasWidth, canvasHeight } ({ position, radius } as c) =
    let
        ( x, y ) =
            Point2d.toTuple Pixels.inPixels position

        flipX crl =
            if x < radius && (Vector2d.xComponent crl.velocity |> Quantity.lessThan Quantity.zero) then
                -- Circle is touched to the left wall
                { crl | velocity = Vector2d.mirrorAcross Axis2d.y crl.velocity }

            else if
                -- Circle is touched to the right wall
                (canvasWidth - radius)
                    < x
                    && (Vector2d.xComponent crl.velocity |> Quantity.greaterThan Quantity.zero)
            then
                { crl | velocity = Vector2d.mirrorAcross Axis2d.y crl.velocity }

            else
                crl

        flipY crl =
            if y < radius && (Vector2d.yComponent crl.velocity |> Quantity.lessThan Quantity.zero) then
                -- Circle is touched to the bottom wall
                { crl | velocity = Vector2d.mirrorAcross Axis2d.x crl.velocity }

            else if
                -- Circle is touched to the top wall
                (canvasHeight - radius)
                    < y
                    && (Vector2d.yComponent crl.velocity |> Quantity.greaterThan Quantity.zero)
            then
                { crl | velocity = Vector2d.mirrorAcross Axis2d.x crl.velocity }

            else
                crl
    in
    c |> flipX |> flipY


generator : Float -> Float -> Random.Generator Circle
generator canvasWidth canvasHeight =
    let
        maxRadius =
            20

        minRadius =
            10

        speedGenerator =
            Random.uniform ( 10, 100 ) [ ( -100, -10 ) ]
                |> Random.andThen (\( a, b ) -> Random.float a b)

        positionGenerator =
            Random.map2 Point2d.pixels
                (Random.float (2 * maxRadius) (canvasWidth - 2 * maxRadius))
                (Random.float (2 * maxRadius) (canvasHeight - 2 * maxRadius))

        velocityGenerator =
            Random.map2 toVelocity
                speedGenerator
                speedGenerator

        colorGenerator =
            Random.weighted ( 80, Color.blue ) [ ( 15, Color.darkBlue ), ( 5, Color.lightYellow ) ]
    in
    Random.map4
        (\p v r c ->
            { position = p
            , velocity = v
            , radius = r
            , color = c
            , collidedTime = 0
            }
        )
        positionGenerator
        velocityGenerator
        (Random.float minRadius maxRadius)
        colorGenerator


toVelocity : Float -> Float -> Velocity
toVelocity x y =
    Vector2d.xy
        (Quantity.per (Duration.seconds 1) (Pixels.pixels x))
        (Quantity.per (Duration.seconds 1) (Pixels.pixels y))


render : Float -> Circle -> Canvas.Renderable
render timeStamp c =
    Canvas.circle (Point2d.toTuple Pixels.inPixels c.position) c.radius
        |> List.singleton
        |> Canvas.shapes
            [ CSettings.fill
                (if timeStamp - c.collidedTime < 200 then
                    c.color

                 else
                    Color.lightBlue
                )
            ]
