module Simple exposing (main)

import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Canvas
import Canvas.Settings exposing (fill)
import Circle2d exposing (Circle2d)
import CollisionDetection2d
import Color exposing (Color)
import Html exposing (Html)
import Html.Events.Extra.Pointer exposing (onMove)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)
import Triangle2d exposing (Triangle2d)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = always Sub.none }



-- Types


type CanvasSystem
    = CanvasSystem


type Object
    = Circle { shape : Circle2d Pixels CanvasSystem, color : Color }
    | Triangle { shape : Triangle2d Pixels CanvasSystem, color : Color }
    | Rectangle { shape : Rectangle2d Pixels CanvasSystem, color : Color }


isTouched : Point2d Pixels CanvasSystem -> Object -> Bool
isTouched point object =
    case object of
        Circle { shape } ->
            Circle2d.contains point shape

        Triangle { shape } ->
            Triangle2d.contains point shape

        Rectangle { shape } ->
            Rectangle2d.contains point shape


getBoundingBox : Object -> BoundingBox2d Pixels CanvasSystem
getBoundingBox object =
    case object of
        Circle { shape } ->
            Circle2d.boundingBox shape

        Triangle { shape } ->
            Triangle2d.boundingBox shape

        Rectangle { shape } ->
            Rectangle2d.boundingBox shape


extrema : BoundingBox2d Pixels CanvasSystem -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
extrema =
    BoundingBox2d.extrema
        >> (\r ->
                { minX = Pixels.inPixels r.minX
                , minY = Pixels.inPixels r.minY
                , maxX = Pixels.inPixels r.maxX
                , maxY = Pixels.inPixels r.maxY
                }
           )



-- Model


init : () -> ( Model, Cmd Msg )
init _ =
    let
        container =
            CollisionDetection2d.quadTree
                { extrema = extrema
                , getBoundingBox = getBoundingBox
                , intersects = BoundingBox2d.intersects
                , boundary = { minX = 0, minY = 0, maxX = 640, maxY = 480 }
                }
    in
    [ Circle { shape = Circle2d.atPoint (Point2d.pixels 120 140) (Pixels.pixels 80), color = Color.yellow }
    , Rectangle { shape = Rectangle2d.from (Point2d.pixels 240 80) (Point2d.pixels 400 200), color = Color.green }
    , Triangle
        { shape = Triangle2d.from (Point2d.pixels 520 70) (Point2d.pixels 442 205) (Point2d.pixels 598 205)
        , color = Color.blue
        }
    , Rectangle { shape = Rectangle2d.from (Point2d.pixels 60 260) (Point2d.pixels 180 420), color = Color.green }
    , Triangle
        { shape = Triangle2d.from (Point2d.pixels 250 340) (Point2d.pixels 385 262) (Point2d.pixels 385 418)
        , color = Color.blue
        }
    , Circle { shape = Circle2d.atPoint (Point2d.pixels 520 340) (Pixels.pixels 80), color = Color.yellow }
    ]
        |> (\objs ->
                { objects =
                    objs
                        |> List.indexedMap Tuple.pair
                        |> List.foldr (\( k, obj ) cont -> CollisionDetection2d.insert k obj cont) container
                , toDraw = List.map (renderObject False) objs
                }
           )
        |> (\m -> ( m, Cmd.none ))


type alias Model =
    { objects : CollisionDetection2d.Container Int Object (BoundingBox2d Pixels CanvasSystem)
    , toDraw : List Canvas.Renderable
    }



-- Update


type Msg
    = OnPointerMove (Point2d Pixels CanvasSystem)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPointerMove point ->
            let
                targetArea =
                    BoundingBox2d.withDimensions ( Pixels.pixels 1, Pixels.pixels 1 ) point

                touchedObjectKeys =
                    CollisionDetection2d.collideWith (isTouched point) targetArea model.objects
                        |> List.map .key
            in
            ( { model
                | toDraw =
                    CollisionDetection2d.foldr
                        (\k o l -> renderObject (List.member k touchedObjectKeys) o :: l)
                        []
                        model.objects
              }
            , Cmd.none
            )



-- view


view : Model -> Html Msg
view { toDraw } =
    Canvas.toHtml ( 640, 480 )
        [ onMove (\ev -> ev.pointer |> .offsetPos |> (\( x, y ) -> Point2d.pixels x y |> OnPointerMove)) ]
        toDraw


renderObject : Bool -> Object -> Canvas.Renderable
renderObject isTouchedFlag object =
    let
        toTuple =
            Point2d.toTuple Pixels.inPixels
    in
    (\( s, c ) ->
        Canvas.shapes
            [ fill
                (if isTouchedFlag then
                    c

                 else
                    Color.gray
                )
            ]
            [ s ]
    )
    <|
        case object of
            Circle { shape, color } ->
                let
                    ( x, y ) =
                        Circle2d.centerPoint shape |> toTuple

                    radius =
                        Circle2d.radius shape |> Pixels.inPixels
                in
                ( Canvas.circle ( x, y ) radius, color )

            Triangle { shape, color } ->
                let
                    ( p0, p1, p2 ) =
                        Triangle2d.vertices shape
                            |> (\( a, b, c ) -> ( toTuple a, toTuple b, toTuple c ))
                in
                ( Canvas.path p0 [ Canvas.lineTo p1, Canvas.lineTo p2, Canvas.lineTo p0 ], color )

            Rectangle { shape, color } ->
                let
                    ( width, height ) =
                        Rectangle2d.dimensions shape |> Tuple.mapBoth Pixels.inPixels Pixels.inPixels

                    ( x, y ) =
                        Rectangle2d.centerPoint shape
                            |> toTuple
                            |> (\( cX, cY ) -> ( cX - (width / 2), cY - (height / 2) ))
                in
                ( Canvas.rect ( x, y ) width height, color )
