module CollisionDetection2d.Internal.FixedDepthTree2d exposing
    ( Constraint
    , Container
    , collidedPairs
    , defaultDepth
    , detect
    , empty
    , foldl
    , fromDict
    , get
    , insert
    , map
    , mapWithDetection
    , remove
    , size
    , toDict
    )

import CollisionDetection2d.Internal.Index2d as Index2d exposing (Direction2d(..), Index2d)
import Dict exposing (Dict)



-- Data structure


type Tree comparable a
    = Leaf (Dict comparable a)
    | Node (Dict comparable a) (Tree comparable a) (Tree comparable a) (Tree comparable a) (Tree comparable a)


build : Int -> Tree comparable a
build depth =
    if depth <= 1 then
        Leaf Dict.empty

    else
        Node
            Dict.empty
            (build (depth - 1))
            (build (depth - 1))
            (build (depth - 1))
            (build (depth - 1))


lookup : Index2d -> comparable -> Tree comparable a -> Maybe a
lookup i key tree =
    let
        path =
            Index2d.toPath i
    in
    treeLookupHelper path key tree


treeLookupHelper : List Direction2d -> comparable -> Tree comparable a -> Maybe a
treeLookupHelper path key tree =
    case path of
        [] ->
            case tree of
                Leaf x ->
                    Dict.get key x

                Node x _ _ _ _ ->
                    Dict.get key x

        d :: path_ ->
            case tree of
                Node x t0 t1 t2 t3 ->
                    case d of
                        D0 ->
                            treeLookupHelper path_ key t0

                        D1 ->
                            treeLookupHelper path_ key t1

                        D2 ->
                            treeLookupHelper path_ key t2

                        D3 ->
                            treeLookupHelper path_ key t3

                _ ->
                    Nothing


add : Index2d -> comparable -> a -> Tree comparable a -> Tree comparable a
add i key item tree =
    let
        path =
            Index2d.toPath i
    in
    treeMapHelper (Dict.insert key item) path tree


drop : Index2d -> comparable -> Tree comparable a -> Tree comparable a
drop i key tree =
    let
        path =
            Index2d.toPath i
    in
    treeMapHelper (Dict.remove key) path tree


treeMapHelper : (Dict comparable a -> Dict comparable a) -> List Direction2d -> Tree comparable a -> Tree comparable a
treeMapHelper f path tree =
    case path of
        [] ->
            case tree of
                Leaf x ->
                    Leaf (f x)

                Node x t0 t1 t2 t3 ->
                    Node (f x) t0 t1 t2 t3

        d :: path_ ->
            case tree of
                Node x t0 t1 t2 t3 ->
                    case d of
                        D0 ->
                            Node x (treeMapHelper f path_ t0) t1 t2 t3

                        D1 ->
                            Node x t0 (treeMapHelper f path_ t1) t2 t3

                        D2 ->
                            Node x t0 t1 (treeMapHelper f path_ t2) t3

                        D3 ->
                            Node x t0 t1 t2 (treeMapHelper f path_ t3)

                _ ->
                    tree



-- APIs


type alias Container comparable a =
    { tree : Tree comparable a
    , toIndex : Dict comparable Index2d
    , constraint : Constraint a
    , parameters : Parameters
    }


type alias Constraint a =
    { range : { minX : Float, maxX : Float, minY : Float, maxY : Float }
    , boundingBox : a -> { minX : Float, maxX : Float, minY : Float, maxY : Float }
    , isCollided : a -> a -> Bool
    }


type alias Parameters =
    { depth : Int, unitWidth : Int, unitHeight : Int }



-- Constructor


defaultDepth : Int
defaultDepth =
    4


empty : Int -> Constraint a -> Container comparable a
empty depth ({ range } as constraint) =
    let
        tree =
            build depth

        ( width, height ) =
            ( range.maxX - range.minX |> floor, range.maxY - range.minY |> floor )

        ( unitWidth, unitHeight ) =
            ( width // 2 ^ (depth - 1) + 1, height // 2 ^ (depth - 1) + 1 )

        parameters =
            { depth = depth
            , unitWidth = unitWidth
            , unitHeight = unitHeight
            }
    in
    { tree = tree, toIndex = Dict.empty, constraint = constraint, parameters = parameters }



-- Manipulation


get : comparable -> Container comparable a -> Maybe a
get key { tree, toIndex } =
    case Dict.get key toIndex of
        Just i ->
            lookup i key tree

        Nothing ->
            Nothing


insert : comparable -> a -> Container comparable a -> Container comparable a
insert key item ({ tree, constraint, toIndex, parameters } as cont) =
    let
        newIndex =
            constraint.boundingBox item
                |> (\r -> { x0 = r.minX, y0 = r.minY, x1 = r.maxX, y1 = r.maxY })
                |> Index2d.coordinateToIndex parameters
    in
    (\t -> { cont | tree = t, toIndex = Dict.insert key newIndex toIndex }) <|
        case Dict.get key toIndex of
            Just oldIndex ->
                if oldIndex == newIndex then
                    add oldIndex key item tree

                else
                    drop oldIndex key tree |> add newIndex key item

            Nothing ->
                add newIndex key item tree


remove : comparable -> Container comparable a -> Container comparable a
remove key ({ tree, constraint, toIndex, parameters } as cont) =
    case Dict.get key toIndex of
        Just oldIndex ->
            drop oldIndex key tree
                |> (\t -> { cont | tree = t, toIndex = Dict.remove key toIndex })

        Nothing ->
            cont


map : (comparable -> a -> a) -> Container comparable a -> Container comparable a
map f ({ tree, constraint, parameters } as cont) =
    let
        getIndex : a -> Index2d
        getIndex v =
            constraint.boundingBox v
                |> (\r -> { x0 = r.minX, x1 = r.maxX, y0 = r.minY, y1 = r.maxY })
                |> Index2d.coordinateToIndex parameters

        newKeyValues : Dict Index2d (List ( comparable, a ))
        newKeyValues =
            mapApplyHelper f getIndex tree

        newKeyToIndex : Dict comparable Index2d
        newKeyToIndex =
            Dict.foldl (\i vs acc -> List.foldl (\( k, _ ) ac -> Dict.insert k i ac) acc vs) Dict.empty newKeyValues
    in
    { cont
        | tree = mapBuildTreeHelper newKeyValues tree []
        , toIndex = newKeyToIndex
    }


mapApplyHelper :
    (comparable -> a -> a)
    -> (a -> Index2d)
    -> Tree comparable a
    -> Dict Index2d (List ( comparable, a ))
mapApplyHelper f getIndex tree =
    let
        collect d =
            Dict.toList d
                |> List.foldl collectHelper Dict.empty

        merge d0 d1 =
            Dict.merge mergeSingle mergeTwo mergeSingle d0 d1 Dict.empty

        mergeSingle k v acc =
            Dict.insert k v acc

        mergeTwo k v0 v1 acc =
            Dict.insert k (v0 ++ v1) acc

        collectHelper ( k, v ) d =
            let
                newV =
                    f k v

                newIndex =
                    getIndex newV
            in
            case Dict.get newIndex d of
                Just xs ->
                    Dict.insert newIndex (( k, newV ) :: xs) d

                Nothing ->
                    Dict.insert newIndex [ ( k, newV ) ] d
    in
    case tree of
        Leaf d ->
            collect d

        Node d t0 t1 t2 t3 ->
            collect d
                |> merge (mapApplyHelper f getIndex t0)
                |> merge (mapApplyHelper f getIndex t1)
                |> merge (mapApplyHelper f getIndex t2)
                |> merge (mapApplyHelper f getIndex t3)


mapBuildTreeHelper :
    Dict Index2d (List ( comparable, a ))
    -> Tree comparable a
    -> List Direction2d
    -> Tree comparable a
mapBuildTreeHelper ref tree directions =
    let
        newItems =
            Dict.get (Index2d.fromPath directions) ref
                |> Maybe.withDefault []
                |> Dict.fromList
    in
    case tree of
        Leaf _ ->
            Leaf newItems

        Node _ t0 t1 t2 t3 ->
            Node
                newItems
                (mapBuildTreeHelper ref t0 (directions ++ [ D0 ]))
                (mapBuildTreeHelper ref t1 (directions ++ [ D1 ]))
                (mapBuildTreeHelper ref t2 (directions ++ [ D2 ]))
                (mapBuildTreeHelper ref t3 (directions ++ [ D3 ]))


mapWithDetection : (List ( comparable, a ) -> comparable -> a -> a) -> Container comparable a -> Container comparable a
mapWithDetection f cont =
    let
        detectedCollisions =
            detect cont

        f_ k v =
            f (Dict.get k detectedCollisions |> Maybe.map .collidedWith |> Maybe.withDefault []) k v
    in
    map f_ cont



-- Collision detection


collidedPairs : Container comparable a -> List ( ( comparable, a ), ( comparable, a ) )
collidedPairs { constraint, tree } =
    let
        f ( _, x ) ( _, y ) =
            constraint.isCollided x y
    in
    collidedPairsHelper f tree []


collidedPairsHelper :
    (( comparable, a ) -> ( comparable, a ) -> Bool)
    -> Tree comparable a
    -> List ( comparable, a )
    -> List ( ( comparable, a ), ( comparable, a ) )
collidedPairsHelper f tree superSets =
    case tree of
        Leaf x ->
            let
                items =
                    Dict.toList x
            in
            comb2 f items [] ++ combProd f items superSets []

        Node x t0 t1 t2 t3 ->
            let
                items =
                    Dict.toList x

                superSets_ =
                    items ++ superSets
            in
            comb2 f items []
                ++ combProd f items superSets []
                ++ collidedPairsHelper f t0 superSets_
                ++ collidedPairsHelper f t1 superSets_
                ++ collidedPairsHelper f t2 superSets_
                ++ collidedPairsHelper f t3 superSets_


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



-- Useful APIs


toDict : Container comparable a -> Dict comparable a
toDict cont =
    toDictHelper cont.tree


toDictHelper : Tree comparable a -> Dict comparable a
toDictHelper tree =
    case tree of
        Leaf d ->
            d

        Node d t0 t1 t2 t3 ->
            d
                |> Dict.union (toDictHelper t0)
                |> Dict.union (toDictHelper t1)
                |> Dict.union (toDictHelper t2)
                |> Dict.union (toDictHelper t3)


fromDict : Int -> Constraint a -> Dict comparable a -> Container comparable a
fromDict depth constraint d =
    Dict.foldl (\k v cnt -> insert k v cnt) (empty depth constraint) d


size : Container comparable a -> Int
size { toIndex } =
    Dict.size toIndex


foldl : (comparable -> a -> b -> b) -> b -> Container comparable a -> b
foldl f acc cont =
    toDict cont |> Dict.foldl f acc


foldr : (comparable -> a -> b -> b) -> b -> Container comparable a -> b
foldr f acc cont =
    toDict cont |> Dict.foldr f acc



-- Helper functions


{-| Make pairs without duplication

    comb2 [ 'a', 'b', 'c' ] [] == [ ( 'a', 'b' ), ( 'a', 'c' ), ( 'b', 'c' ) ]

-}
comb2 : (a -> a -> Bool) -> List a -> List ( a, a ) -> List ( a, a )
comb2 f l acc =
    case l of
        [] ->
            acc

        _ :: [] ->
            acc

        x :: xs ->
            comb2 f
                xs
                (acc
                    ++ List.filterMap
                        (\y ->
                            if f x y then
                                Just ( x, y )

                            else
                                Nothing
                        )
                        xs
                )


{-| Make combinations from two list

    combProd [ 'a', 'b' ] [ 'c', 'd' ] [] == [ ( 'a', 'c' ), ( 'a', 'd' ), ( 'b', 'c' ), ( 'b', 'd' ) ]

-}
combProd : (a -> a -> Bool) -> List a -> List a -> List ( a, a ) -> List ( a, a )
combProd f l target acc =
    case l of
        [] ->
            acc

        x :: [] ->
            acc
                ++ List.filterMap
                    (\y ->
                        if f x y then
                            Just ( x, y )

                        else
                            Nothing
                    )
                    target

        x :: xs ->
            combProd f
                xs
                target
                (acc
                    ++ List.filterMap
                        (\y ->
                            if f x y then
                                Just ( x, y )

                            else
                                Nothing
                        )
                        target
                )
