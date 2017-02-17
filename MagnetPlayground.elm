module MagnetPlayground exposing (..)

import MagnetPieces exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (viewBox)
import Html exposing (div)
import Html.Attributes
import Mouse
import Html.Events exposing (on)
import Json.Decode as Decode exposing (int, field)


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
    }


type alias ShapePosition =
    { shapeId : String
    , pointOnCanvas : Point
    }


type alias Drag =
    { start : ShapePosition
    , current : Point
    }


type Msg
    = DragStart ShapePosition
    | DragAt Point
    | DragEnd Point
    | MouseOver ShapePosition


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
          }
        , Cmd.none
        )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- let
    --     _ =
    --         Debug.log "msg" msg
    -- in
    case msg of
        DragStart shapePosition ->
            ( { model | drag = Just { start = shapePosition, current = shapePosition.pointOnCanvas } }
            , Cmd.none
            )

        DragAt position ->
            case model.drag of
                Nothing ->
                    ( model, Cmd.none )

                Just drag ->
                    let
                        diff =
                            pointDiff drag.current position

                        _ =
                            Debug.log "dragged " diff
                    in
                        ( model, Cmd.none )

        _ ->
            ( model
            , Cmd.none
            )



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        eventHanderAttributes shape =
            [ onMouseDown shape.id
            , onMouseHover shape.id
            ]

        svgShapes =
            List.map (\shape -> draw (eventHanderAttributes shape) shape.color shape.points) model.shapes
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


pointDecoder =
    (Decode.map2
        Point
        (field "x" Decode.float)
        (field "y" Decode.float)
    )


shapePositionDecoder : String -> Decode.Decoder ShapePosition
shapePositionDecoder identifier =
    (Decode.map2 ShapePosition
        (Decode.succeed
            identifier
        )
        pointDecoder
    )


onMouseDown : String -> Svg.Attribute Msg
onMouseDown identifier =
    on "mousedown" (Decode.map DragStart (shapePositionDecoder identifier))


onMouseHover : String -> Svg.Attribute Msg
onMouseHover identifier =
    on "mousemove" (Decode.map MouseOver (shapePositionDecoder identifier))



-- SUBSCRIPTIONS


mousePositionToPoint : Mouse.Position -> Point
mousePositionToPoint pos =
    Point (toFloat pos.x) (toFloat pos.y)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just drag ->
            Sub.batch
                [ Sub.map (\p -> DragAt p) (Mouse.moves mousePositionToPoint)
                , Sub.map (\p -> DragAt p) (Mouse.ups mousePositionToPoint)
                ]



--Sub.none


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
