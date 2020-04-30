module Naive2d exposing (Naive2d)

import BoundingBox2d exposing (BoundingBox2d)
import Dict exposing (Dict)


type Naive2d comparable shape units coordinates
    = Naive2d
        { shapes : Dict comparable { shape : shape, boundingBox : BoundingBox2d units coordinates }
        }


naive2d : Naive2d comparable shape units coordinates
naive2d =
    Naive2d { shapes = Dict.empty }


detectCollisions :
    (shape -> shape -> Bool)
    -> Naive2d comparable shape units coordinates
    -> List ( comparable, comparable )
detectCollisions checkCollision (Naive2d container) =
    detectCollisionsLoop checkCollision (Dict.toList container.shapes) []


collideWith :
    (shape -> Bool)
    -> BoundingBox2d units coordinates
    -> Naive2d comparable shape units coordinates
    -> List ( comparable, shape )
collideWith checkCollision boundingBox (Naive2d container) =
    Dict.toList container.shapes
        |> List.filterMap (collideWithFilter checkCollision boundingBox)


insert :
    comparable
    -> BoundingBox2d units coordinates
    -> shape
    -> Naive2d comparable shape units coordinates
    -> Naive2d comparable shape units coordinates
insert key boundingBox shape (Naive2d container) =
    Naive2d { container | shapes = Dict.insert key { boundingBox = boundingBox, shape = shape } container.shapes }


remove :
    comparable
    -> Naive2d comparable shape units coordinates
    -> Naive2d comparable shape units coordinates
remove key (Naive2d container) =
    Naive2d { container | shapes = Dict.remove key container.shapes }


get :
    comparable
    -> Naive2d comparable shape units coordinates
    -> Maybe shape
get key (Naive2d container) =
    Dict.get key container.shapes |> Maybe.map .shape


toDict :
    Naive2d comparable shape units coordinates
    -> Dict comparable { shape : shape, boundingBox : BoundingBox2d units coordinates }
toDict (Naive2d container) =
    container.shapes



-- Helper functions


detectCollisionsLoop :
    (shape -> shape -> Bool)
    -> List ( key, { shape : shape, boundingBox : BoundingBox2d units coordinates } )
    -> List ( key, key )
    -> List ( key, key )
detectCollisionsLoop check l result =
    case l of
        ( k, { shape, boundingBox } ) :: remaining ->
            List.filterMap (detectCollisionsLoopFilter check k shape boundingBox) remaining
                |> detectCollisionsLoop check remaining

        [] ->
            result


detectCollisionsLoopFilter :
    (shape -> shape -> Bool)
    -> key
    -> shape
    -> BoundingBox2d units coordinates
    -> ( key, { shape : shape, boundingBox : BoundingBox2d units coordinates } )
    -> Maybe ( key, key )
detectCollisionsLoopFilter check k s bBox ( k2, obj ) =
    case BoundingBox2d.intersects bBox obj.boundingBox of
        True ->
            case check s obj.shape of
                True ->
                    Just ( k, k2 )

                False ->
                    Nothing

        False ->
            Nothing


collideWithFilter :
    (shape -> Bool)
    -> BoundingBox2d units coordinates
    -> ( key, { shape : shape, boundingBox : BoundingBox2d units coordinates } )
    -> Maybe ( key, shape )
collideWithFilter check bBox ( k, obj ) =
    case BoundingBox2d.intersects bBox obj.boundingBox of
        True ->
            case check obj.shape of
                True ->
                    Just ( k, obj.shape )

                False ->
                    Nothing

        False ->
            Nothing
