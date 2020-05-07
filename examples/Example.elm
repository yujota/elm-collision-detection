module Example exposing (main)

import Array exposing (Array)
import Axis2d
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas
import Canvas.Settings as CSettings
import Canvas.Settings.Text as CText
import CollisionDetection2d
import Color exposing (Color)
import Duration exposing (Duration, Seconds)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity
import Random
import Task
import Vector2d exposing (Vector2d)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- Types


type Mode
    = QuadTreeMode
    | NaiveMode


type CanvasSystem
    = CanvasSystem


type alias PixelsPerSecond =
    Quantity.Rate Pixels Seconds


type alias Velocity =
    Vector2d PixelsPerSecond CanvasSystem


type alias Position =
    Point2d Pixels CanvasSystem


type alias Circle =
    { position : Position, velocity : Velocity, radius : Float, color : Color }


type alias Circles =
    CollisionDetection2d.Container Int Circle (BoundingBox2d Pixels CanvasSystem)


toVelocity : Float -> Float -> Velocity
toVelocity x y =
    Vector2d.xy
        (Quantity.per (Duration.seconds 1) (Pixels.pixels x))
        (Quantity.per (Duration.seconds 1) (Pixels.pixels y))



-- Model


type alias Model =
    { seed : Random.Seed
    , canvasWidth : Float
    , canvasHeight : Float
    , toDraw : List Canvas.Renderable
    , circles : CollisionDetection2d.Container Int Circle (BoundingBox2d Pixels CanvasSystem)
    , renderingIntervals : Array Float
    , averageFps : Float
    , fpsSleepTimer : Float
    , count : Float
    , maxCircles : Int
    , addCircleInterval : Float
    , mode : Mode
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seed = Random.initialSeed 127
      , canvasWidth = 0
      , canvasHeight = 0
      , toDraw = []
      , circles =
            CollisionDetection2d.quadTree
                { extrema = extrema
                , intersects = intersects
                , boundary = emptyBoundary
                , getBoundingBox = getBoundingBox
                }
      , renderingIntervals = Array.empty
      , averageFps = 0
      , fpsSleepTimer = 500
      , count = 0
      , maxCircles = 500
      , addCircleInterval = 100
      , mode = QuadTreeMode
      }
    , Task.perform GetViewPort getViewport
    )



-- Update


type Msg
    = RefreshScreen Float
    | GetViewPort Viewport
    | ChangeMode Mode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (\m -> ( m, Cmd.none )) <|
        case msg of
            RefreshScreen d ->
                (if model.count - d < 0 then
                    if CollisionDetection2d.size model.circles < model.maxCircles && model.averageFps > 30 then
                        addCircle model
                            |> (\m -> { m | count = model.addCircleInterval })

                    else
                        { model | count = model.addCircleInterval }

                 else
                    { model | count = model.count - d }
                )
                    |> refreshScreen d

            ChangeMode QuadTreeMode ->
                { model
                    | circles =
                        CollisionDetection2d.quadTree
                            { extrema = extrema
                            , intersects = intersects
                            , boundary = { emptyBoundary | maxX = model.canvasWidth, maxY = model.canvasHeight }
                            , getBoundingBox = getBoundingBox
                            }
                            |> transferCircles model.circles
                    , mode = QuadTreeMode
                    , renderingIntervals = Array.empty
                }

            ChangeMode NaiveMode ->
                { model
                    | circles =
                        CollisionDetection2d.naive
                            { extrema = extrema
                            , intersects = intersects
                            , getBoundingBox = getBoundingBox
                            }
                            |> transferCircles model.circles
                    , mode = NaiveMode
                    , renderingIntervals = Array.empty
                }

            GetViewPort data ->
                let
                    ( canvasWidth, windowHeight ) =
                        ( data.viewport.width, data.viewport.height )
                in
                { model
                    | canvasWidth = canvasWidth
                    , canvasHeight = windowHeight - bottomBarHeight
                    , count = model.addCircleInterval
                    , circles =
                        CollisionDetection2d.quadTree
                            { extrema = extrema
                            , intersects = intersects
                            , boundary = { emptyBoundary | maxX = canvasWidth, maxY = windowHeight - bottomBarHeight }
                            , getBoundingBox = getBoundingBox
                            }
                }
                    |> addCircle


transferCircles : Circles -> Circles -> Circles
transferCircles oldContainer newContainer =
    CollisionDetection2d.foldr (\k c cont -> CollisionDetection2d.insert k c cont) newContainer oldContainer


addCircle : Model -> Model
addCircle model =
    let
        ( newCircle, newSeed ) =
            Random.step (circleGenerator model.canvasWidth model.canvasHeight) model.seed
    in
    { model
        | seed = newSeed
        , circles = CollisionDetection2d.insert (CollisionDetection2d.size model.circles) newCircle model.circles
    }


refreshScreen : Float -> Model -> Model
refreshScreen d model =
    updateAverageFps d model
        |> updateCircles d
        |> render


updateAverageFps : Float -> Model -> Model
updateAverageFps d model =
    let
        intervals =
            if Array.length model.renderingIntervals >= 20 then
                Array.slice 1 20 model.renderingIntervals |> Array.push d

            else
                model.renderingIntervals |> Array.push d
    in
    { model | renderingIntervals = intervals }
        |> (\m ->
                if model.fpsSleepTimer < 0 then
                    { m
                        | averageFps =
                            1000 / (Array.foldr (+) 0 intervals / toFloat (Array.length intervals))
                        , fpsSleepTimer = 500
                    }

                else
                    { m | fpsSleepTimer = m.fpsSleepTimer - d }
           )


updateCircles : Float -> Model -> Model
updateCircles d model =
    { model
        | circles =
            CollisionDetection2d.map
                (\_ c -> flip { width = model.canvasWidth, height = model.canvasHeight } c |> move d)
                model.circles
    }


render : Model -> Model
render model =
    { model | toDraw = [ renderBackground model ] ++ renderCircles model ++ [ renderAverageFps model ] }


renderBackground : Model -> Canvas.Renderable
renderBackground model =
    Canvas.rect ( 0, 0 ) model.canvasWidth model.canvasHeight
        |> List.singleton
        |> Canvas.shapes [ CSettings.fill Color.white ]


renderAverageFps : Model -> Canvas.Renderable
renderAverageFps model =
    let
        center =
            ( model.canvasWidth / 2, model.canvasHeight / 2 )
    in
    Canvas.text
        [ CText.font { size = 36, family = "monospace" }
        , CText.align CText.Center
        , CSettings.fill Color.darkGray
        ]
        center
        (model.averageFps |> floor |> String.fromInt |> (\s -> "FPS: " ++ s))


renderCircles : Model -> List Canvas.Renderable
renderCircles model =
    let
        collidedKeys =
            CollisionDetection2d.detectCollisions isCollided model.circles
                |> List.concatMap (\( a, b ) -> [ a.key, b.key ])

        rdr k c =
            Canvas.circle (Point2d.toTuple Pixels.inPixels c.position) c.radius
                |> List.singleton
                |> Canvas.shapes
                    [ CSettings.fill
                        (if List.member k collidedKeys then
                            c.color

                         else
                            Color.lightBlue
                        )
                    ]
    in
    CollisionDetection2d.foldr (\k c l -> rdr k c :: l) [] model.circles



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta RefreshScreen



-- view


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "elm-div" ]
        [ Canvas.toHtml ( floor model.canvasWidth, floor model.canvasHeight )
            [ Attr.class "canvas-display" ]
            model.toDraw
        , buttons model
        ]


buttons : Model -> Html Msg
buttons model =
    (\l -> Html.div [ Attr.class "button-group" ] l) <|
        case model.mode of
            QuadTreeMode ->
                [ Html.button
                    [ onClick (ChangeMode NaiveMode), Attr.class "button-inactive" ]
                    [ Html.text "Naive" ]
                , Html.button
                    [ onClick (ChangeMode QuadTreeMode), Attr.class "button-active" ]
                    [ Html.text "QuadTree" ]
                ]

            NaiveMode ->
                [ Html.button
                    [ onClick (ChangeMode NaiveMode), Attr.class "button-active" ]
                    [ Html.text "Naive" ]
                , Html.button
                    [ onClick (ChangeMode QuadTreeMode), Attr.class "button-inactive" ]
                    [ Html.text "QuadTree" ]
                ]


bottomBarHeight =
    60



-- Helpers


isCollided : Circle -> Circle -> Bool
isCollided c1 c2 =
    Point2d.equalWithin (c1.radius + c2.radius |> Pixels.pixels) c1.position c2.position


circleGenerator : Float -> Float -> Random.Generator Circle
circleGenerator canvasWidth canvasHeight =
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
    Random.map4 (\p v r c -> { position = p, velocity = v, radius = r, color = c })
        positionGenerator
        velocityGenerator
        (Random.float minRadius maxRadius)
        colorGenerator


move : Float -> Circle -> Circle
move t ({ velocity, position } as c) =
    { c | position = Vector2d.for (Duration.milliseconds t) velocity |> (\v -> Point2d.translateBy v position) }


flip : { width : Float, height : Float } -> Circle -> Circle
flip { width, height } ({ position, radius } as c) =
    let
        ( x, y ) =
            Point2d.toTuple Pixels.inPixels position

        flipX crl =
            if x < radius || (width - radius) < x then
                { crl | velocity = Vector2d.mirrorAcross Axis2d.y crl.velocity }

            else
                crl

        flipY crl =
            if y < radius || (height - radius) < y then
                { crl | velocity = Vector2d.mirrorAcross Axis2d.x crl.velocity }

            else
                crl
    in
    c |> flipX |> flipY


getBoundingBox : Circle -> BoundingBox2d Pixels CanvasSystem
getBoundingBox c =
    BoundingBox2d.withDimensions ( Pixels.pixels c.radius, Pixels.pixels c.radius ) c.position


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


emptyBoundary =
    { minX = 0, minY = 0, maxX = 0, maxY = 0 }
