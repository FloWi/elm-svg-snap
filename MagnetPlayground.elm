module MagnetPlayground exposing (..)

import MagnetPieces exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (viewBox)
import Html exposing (div)
import Html.Attributes
import Mouse
import Html.Events exposing (on)
import Json.Decode as Decode


colors =
    { gray = "#5a6378"
    , green = "#83c833"
    , orange = "#efa500"
    , blue = "#5fb4ca"
    }


type alias Shape =
    { points : List Point
    , color : String
    , id : String
    }



-- MODEL


type alias Model =
    { shapes : List Shape
    , drag : Maybe Drag
    , mouseOver : Maybe Point
    }


type alias Drag =
    { start : Point
    , current : Point
    }


type Msg
    = DragStart Mouse.Position
    | DragAt Mouse.Position
    | DragEnd Mouse.Position
    | MouseOver Mouse.Position


initialShapes : List { points : List Point, color : String }
initialShapes =
    [ { points = triangle 2 |> rotate (45 + 180)
      , color = colors.gray
      }
    , { points = triangle 2 |> rotate -45
      , color = colors.blue
      }
    ]


init : ( Model, Cmd Msg )
init =
    let
        addIdToShape : Int -> { points : List Point, color : String } -> Shape
        addIdToShape id x =
            { points = x.points, color = x.color, id = toString id }

        shapesWithId : List Shape
        shapesWithId =
            List.indexedMap addIdToShape initialShapes
    in
        ( { shapes = shapesWithId
          , drag = Nothing
          , mouseOver = Nothing
          }
        , Cmd.none
        )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
        ( model
        , Cmd.none
        )



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        svgShapes =
            List.map (\shape -> draw [ onMouseDown ] shape.color shape.points) model.shapes
    in
        div []
            [ svg
                [ Svg.Attributes.width "640"
                , Svg.Attributes.height "480"
                , viewBox "-3 -3 10 10"
                ]
                svgShapes
            , div
                []
                [ Html.text <| toString model ]
            ]


onMouseDown : Svg.Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map DragStart Mouse.position)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Mouse.moves DragAt
                , Mouse.ups DragEnd
                ]


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
