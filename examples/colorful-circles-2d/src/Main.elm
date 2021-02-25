module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas
import Canvas.Settings as CSettings
import Canvas.Settings.Text as CText
import Circle exposing (Circle)
import CollisionDetection2d exposing (Container)
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Metrics exposing (Metrics)
import Random
import Task


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- Model


type alias Model =
    { seed : Random.Seed
    , canvasWidth : Float
    , canvasHeight : Float
    , toDraw : List Canvas.Renderable
    , circles : Container Int Circle
    , metrics : Metrics
    , timeStamp : Float
    , dataStructure : DataStructure
    }


type DataStructure
    = MerelyDict
    | QuadTree


mapStructure : DataStructure -> CollisionDetection2d.DataStructure
mapStructure st =
    case st of
        MerelyDict ->
            CollisionDetection2d.merelyDict

        QuadTree ->
            CollisionDetection2d.fixedDepthTree { depth = 4 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seed = Random.initialSeed 127
      , canvasWidth = 0
      , canvasHeight = 0
      , toDraw = []
      , circles =
            CollisionDetection2d.empty
                CollisionDetection2d.default
                { range = { minX = 0, maxX = 0, minY = 0, maxY = 0 }
                , boundingBox = Circle.getBoundingBox
                , isCollided = Circle.isCollided
                }
      , metrics = Metrics.init ()
      , timeStamp = 0
      , dataStructure = QuadTree
      }
    , Task.perform GetViewPort getViewport
    )



-- Update


type Msg
    = RefreshScreen Float
    | GetViewPort Viewport
    | ChangeDataStructure DataStructure


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (\m -> ( m, Cmd.none )) <|
        case msg of
            RefreshScreen d ->
                let
                    canvasSpec =
                        { canvasWidth = model.canvasWidth, canvasHeight = model.canvasHeight }

                    newTimeStamp =
                        model.timeStamp + d

                    newCircles =
                        model.circles
                            |> CollisionDetection2d.map (\_ v -> Circle.updatePosition d v)
                            |> CollisionDetection2d.mapWithDetection
                                (Circle.reactToCollisionAndWallBound
                                    { canvasWidth = model.canvasWidth
                                    , canvasHeight = model.canvasHeight
                                    , timeStamp = newTimeStamp
                                    }
                                )
                in
                { model
                    | circles = newCircles
                    , timeStamp = newTimeStamp
                    , metrics = Metrics.update d model.metrics
                    , toDraw =
                        renderBackground canvasSpec
                            :: renderFpsText canvasSpec (Metrics.averageFps model.metrics)
                            :: CollisionDetection2d.foldl (\_ c acc -> Circle.render newTimeStamp c :: acc) [] newCircles
                }
                    |> (\m ->
                            if Metrics.averageFps model.metrics > 55 then
                                addCircle m

                            else
                                m
                       )

            GetViewPort data ->
                let
                    ( canvasWidth, canvasHeight ) =
                        ( data.viewport.width, data.viewport.height - bottomBarHeight )
                in
                { model
                    | canvasWidth = canvasWidth
                    , canvasHeight = canvasHeight
                    , circles =
                        CollisionDetection2d.empty
                            (mapStructure model.dataStructure)
                            { range = { minX = 0, maxX = canvasWidth, minY = 0, maxY = canvasHeight }
                            , boundingBox = Circle.getBoundingBox
                            , isCollided = Circle.isCollided
                            }
                }

            ChangeDataStructure structure ->
                { model
                    | dataStructure = structure
                    , circles =
                        CollisionDetection2d.fromDict
                            (mapStructure structure)
                            { range = { minX = 0, maxX = model.canvasWidth, minY = 0, maxY = model.canvasHeight }
                            , boundingBox = Circle.getBoundingBox
                            , isCollided = Circle.isCollided
                            }
                            (CollisionDetection2d.toDict model.circles)
                }


addCircle : Model -> Model
addCircle model =
    let
        ( newCircle, newSeed ) =
            Random.step (Circle.generator model.canvasWidth model.canvasHeight) model.seed
    in
    { model
        | seed = newSeed
        , circles = CollisionDetection2d.insert (CollisionDetection2d.size model.circles) newCircle model.circles
    }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta RefreshScreen



-- View


bottomBarHeight =
    60


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
        case model.dataStructure of
            QuadTree ->
                [ Html.button
                    [ onClick (ChangeDataStructure MerelyDict), Attr.class "button-inactive" ]
                    [ Html.text "Dict" ]
                , Html.button
                    [ Attr.class "button-active" ]
                    [ Html.text "QuadTree" ]
                ]

            MerelyDict ->
                [ Html.button
                    [ Attr.class "button-active" ]
                    [ Html.text "Dict" ]
                , Html.button
                    [ onClick (ChangeDataStructure QuadTree), Attr.class "button-inactive" ]
                    [ Html.text "QuadTree" ]
                ]


renderBackground : { canvasWidth : Float, canvasHeight : Float } -> Canvas.Renderable
renderBackground model =
    Canvas.rect ( 0, 0 ) model.canvasWidth model.canvasHeight
        |> List.singleton
        |> Canvas.shapes [ CSettings.fill Color.white ]


renderFpsText : { canvasWidth : Float, canvasHeight : Float } -> Float -> Canvas.Renderable
renderFpsText model fps =
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
        (fps |> floor |> String.fromInt |> (\s -> "FPS: " ++ s))
