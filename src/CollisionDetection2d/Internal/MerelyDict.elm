module CollisionDetection2d.Internal.MerelyDict exposing
    ( Container
    , detect
    , empty
    , fromDict
    , get
    , insert
    , map
    , mapWithDetection
    , remove
    , size
    , toDict
    )

import Dict exposing (Dict)


type alias Container comparable a =
    { items : Dict comparable a, isCollided : a -> a -> Bool }


type alias Constraint a =
    { isCollided : a -> a -> Bool
    }


empty : Constraint a -> Container comparable a
empty { isCollided } =
    { items = Dict.empty, isCollided = isCollided }


fromDict : Constraint a -> Dict comparable a -> Container comparable a
fromDict { isCollided } d =
    { items = d, isCollided = isCollided }


detect : Container comparable a -> Dict comparable { object : a, collidedWith : List ( comparable, a ) }
detect container =
    collidedPairs container |> (\cnt -> detectHelper cnt Dict.empty)


detectHelper :
    List ( ( comparable, a ), ( comparable, a ) )
    -> Dict comparable { object : a, collidedWith : List ( comparable, a ) }
    -> Dict comparable { object : a, collidedWith : List ( comparable, a ) }
detectHelper xs acc =
    let
        f v kX kV maybeItem =
            case maybeItem of
                Just p ->
                    Just { object = p.object, collidedWith = ( kX, kV ) :: p.collidedWith }

                Nothing ->
                    Just { object = v, collidedWith = [ ( kX, kV ) ] }
    in
    case xs of
        [] ->
            acc

        ( ( k0, v0 ), ( k1, v1 ) ) :: xs_ ->
            detectHelper xs_ (acc |> Dict.update k0 (f v0 k1 v1) |> Dict.update k1 (f v1 k0 v0))


collidedPairs : Container comparable a -> List ( ( comparable, a ), ( comparable, a ) )
collidedPairs { isCollided, items } =
    collidedPairsHelper isCollided (Dict.toList items)


collidedPairsHelper :
    (a -> a -> Bool)
    -> List ( comparable, a )
    -> List ( ( comparable, a ), ( comparable, a ) )
collidedPairsHelper isCollided l =
    case l of
        [] ->
            []

        _ :: [] ->
            []

        x :: xs ->
            List.filterMap
                (\y ->
                    if isCollided (Tuple.second x) (Tuple.second y) then
                        Just ( x, y )

                    else
                        Nothing
                )
                xs
                ++ collidedPairsHelper isCollided xs


mapWithDetection : (List ( comparable, a ) -> comparable -> a -> a) -> Container comparable a -> Container comparable a
mapWithDetection f cont =
    let
        detectedCollisions =
            detect cont

        f_ k v =
            f (Dict.get k detectedCollisions |> Maybe.map .collidedWith |> Maybe.withDefault []) k v
    in
    map f_ cont


map : (comparable -> a -> a) -> Container comparable a -> Container comparable a
map f cont =
    { cont | items = Dict.map f cont.items }


insert : comparable -> a -> Container comparable a -> Container comparable a
insert key object cont =
    { cont | items = Dict.insert key object cont.items }


remove : comparable -> Container comparable a -> Container comparable a
remove key cont =
    { cont | items = Dict.remove key cont.items }


get : comparable -> Container comparable a -> Maybe a
get key cont =
    Dict.get key cont.items


size : Container comparable a -> Int
size cont =
    Dict.size cont.items


toDict : Container comparable a -> Dict comparable a
toDict cont =
    cont.items
