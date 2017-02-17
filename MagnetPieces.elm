module MagnetPieces exposing (..)

import String
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Point =
    { x : Float, y : Float }


to : List Point -> Int -> Point
to ps pointNumber =
    ps
        |> List.drop (pointNumber - 1)
        |> List.head
        |> Maybe.withDefault { x = 0, y = 0 }


snap : Int -> Point -> List Point -> List Point
snap pointNumber snapTarget ps =
    case
        ps
            |> List.drop (pointNumber - 1)
            |> List.head
    of
        Just pivot ->
            ps
                |> sub pivot
                |> add snapTarget

        Nothing ->
            ps


sub : Point -> List Point -> List Point
sub { x, y } =
    add { x = -x, y = -y }


add : Point -> List Point -> List Point
add delta ps =
    List.map (\{ x, y } -> { x = x + delta.x, y = y + delta.y }) ps


pointDiff : Point -> Point -> Point
pointDiff origin current =
    Point (current.x - origin.x) (current.y - origin.y)


flip : List Point -> List Point
flip ps =
    List.map (\{ x, y } -> { x = -x, y = y }) ps


rotate : Float -> List Point -> List Point
rotate angle ps =
    let
        rad =
            degrees angle

        rotate_ { x, y } =
            Point
                (cos rad * x + sin rad * y)
                (sin rad * -x + cos rad * y)
    in
        List.map rotate_ ps


triangle : Float -> List Point
triangle size =
    [ Point 0 0
    , Point size 0
    , Point 0 size
    ]


parallelogram : List Point
parallelogram =
    [ Point 0 0
    , Point 1 0
    , Point 2 -1
    , Point 1 -1
    ]


square : List Point
square =
    [ Point 0 0
    , Point 1 0
    , Point 1 1
    , Point 0 1
    ]


draw : List (Attribute msg) -> String -> List Point -> Svg msg
draw givenAttributes color ps =
    let
        allAttributes =
            List.append
                givenAttributes
                [ ps
                    |> List.concatMap (\p -> [ p.x, p.y ])
                    |> List.map toString
                    |> String.join ","
                    |> points
                , fill color
                , stroke "white"
                , strokeWidth "0.05"
                ]
    in
        polygon
            allAttributes
            []
