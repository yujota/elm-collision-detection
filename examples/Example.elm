module Example exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Canvas
import Color
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Pixels exposing (Pixels)
import QuadTree exposing (QuadTree)
import Random
import Task


main =
    Browser.element { init = init, update = update, view = view, subscriptions = always Sub.none }


init : Int -> ( Model, Cmd Msg )
init i =
    ( NotStarted { seed = Random.initialSeed i }
    , Task.perform GetViewPort getViewport
    )


type Model
    = NotStarted { seed : Random.Seed }
    | QuadTreeMode
        { canvasWidth : Int
        , canvasHeight : Int
        , numPoint : Int
        , circles : QuadTree Int Circle Pixels CanvasSystem
        , seed : Random.Seed
        , renderingDuration : Array Float
        , count : Float
        }


type alias Circle =
    {}



-- Update


type Msg
    = Tick
    | GetViewPort Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( model, Cmd.none )

        GetViewPort data ->
            let
                ( canvasWidth, canvasHeight ) =
                    ( data.viewport.width, data.viewport.height )
            in
            ( QuadTreeMode {}
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div [] []


bottomBarHeight =
    20


type CanvasSystem
    = CanvasSystem


updateRenderingDuration : Float -> Array Float -> ( Array Float, Float )
updateRenderingDuration c stamps =
    let
        newStamps =
            Array.slice 1 9 stamps |> Array.push c
    in
    ( newStamps, Array.foldr (+) 0 newStamps / toFloat (Array.length newStamps) )
