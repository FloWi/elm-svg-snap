module MagnetPlayground exposing (..)

import MagnetPieces exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (viewBox)
import Html exposing (div, hr)
import Html.Attributes
import Mouse
import Html.Events exposing (on, onClick)
import Json.Decode as Decode exposing (int, field)
import Dict


colors : { gray : String, green : String, orange : String, blue : String }
colors =
    { gray = "#5a6378"
    , green = "#83c833"
    , orange = "#efa500"
    , blue = "#5fb4ca"
    }


type alias Sizes =
    { canvasSize : Size, viewBoxSize : Size, viewBoxOffset : Point }


type alias Size =
    { width : Float, height : Float }


type alias Shape =
    { points : List Point
    , color : String
    , id : String
    , zIndex : Int
    }



-- MODEL


type alias Model =
    { shapes : Dict.Dict String Shape
    , drag : Maybe Drag
    , sizes : Sizes
    , showDebug : Bool
    }


type alias Drag =
    { start : ShapePosition
    , current : Point
    }


type alias ShapePosition =
    { shapeId : String
    , pointOnCanvas : Point
    }


bigTriangleGray =
    { points = triangle 2 |> rotate (45 + 180)
    , color = colors.gray
    }


bigTriangleBlue =
    { points = triangle 2 |> rotate -45
    , color = colors.blue
    }


parallelogramGreen =
    { points = parallelogram |> rotate -45 |> snap 1 (to bigTriangleGray.points 3)
    , color = colors.green
    }


smallTriangleOrange =
    { points = triangle 1 |> rotate (45 + 90)
    , color = colors.orange
    }


mediumTriangleBlue =
    { points = triangle (sqrt 2) |> rotate -90 |> snap 3 (to parallelogramGreen.points 4)
    , color = colors.blue
    }


smallTriangleGreen =
    { points = square |> rotate 45
    , color = colors.green
    }


smallTriangleOrangeUpright =
    { points = triangle 1 |> rotate 45 |> snap 3 (to bigTriangleBlue.points 2)
    , color = colors.orange
    }


initialShapes : List { points : List Point, color : String }
initialShapes =
    [ bigTriangleGray
    , bigTriangleBlue
    , parallelogramGreen
    , smallTriangleGreen
    , smallTriangleOrangeUpright
    , smallTriangleOrange
    , mediumTriangleBlue
    ]


solvedTangram =
    initialShapes
        |> List.indexedMap addIdToShape
        |> List.map (\shape -> ( shape.id, shape ))
        |> Dict.fromList


addIdToShape : Int -> { points : List Point, color : String } -> Shape
addIdToShape id x =
    { points = x.points, color = x.color, id = toString id, zIndex = 0 }


init : ( Model, Cmd Msg )
init =
    let
        addOffsetToShape : Int -> Shape -> Shape
        addOffsetToShape i shape =
            let
                yOffset =
                    i // 3

                xOffset =
                    i % 3
            in
                { shape | points = (add (Point (toFloat xOffset * 2) (toFloat yOffset * 3)) shape.points) }

        shapes : List Shape
        shapes =
            List.indexedMap addIdToShape initialShapes
                |> List.indexedMap (\i shape -> addOffsetToShape i shape)
    in
        ( { shapes = Dict.fromList (List.map (\shape -> ( shape.id, shape )) shapes)
          , drag = Nothing
          , showDebug = False
          , sizes =
                { canvasSize = (Size 640 480)
                , viewBoxSize = (Size 8 8)
                , viewBoxOffset = (Point -2 -2)
                }
          }
        , Cmd.none
        )



-- UPDATE


type Msg
    = DragStart ShapePosition
    | DragAt Point
    | DragEnd Point
    | MouseOver ShapePosition
    | PutIntoCorrectPositions
    | ToggleDebugOutput


scaleWith : Size -> Point -> Point
scaleWith scaleFactor pt =
    Point (pt.x / scaleFactor.width) (pt.y / scaleFactor.height)


bringShapeToFront =
    applyZIndexToShape 10


bringShapeToBackToNormal =
    applyZIndexToShape 0


applyZIndexToShape : Int -> String -> Dict.Dict String Shape -> Dict.Dict String Shape
applyZIndexToShape zIndex id shapes =
    case Dict.get id shapes of
        Nothing ->
            shapes

        Just shape ->
            shapes |> Dict.insert id { shape | zIndex = zIndex }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        scaleFactor =
            { width = model.sizes.canvasSize.width / model.sizes.viewBoxSize.width
            , height = model.sizes.canvasSize.height / model.sizes.viewBoxSize.height
            }
    in
        -- let
        --     _ =
        --         Debug.log "msg" msg
        -- in
        case msg of
            DragStart shapePosition ->
                ( { model
                    | drag = Just { start = shapePosition, current = shapePosition.pointOnCanvas }
                    , shapes = bringShapeToFront shapePosition.shapeId model.shapes
                  }
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
                                    |> scaleWith scaleFactor

                            shapeId =
                                drag.start.shapeId
                        in
                            case Dict.get shapeId model.shapes of
                                Just draggedShape ->
                                    let
                                        newShapePoints =
                                            (add diff draggedShape.points)

                                        updatedShape =
                                            { draggedShape | points = newShapePoints }

                                        updatedShapesDict =
                                            Dict.insert shapeId updatedShape model.shapes
                                    in
                                        ( { model
                                            | shapes = updatedShapesDict
                                            , drag = Just { drag | current = position }
                                          }
                                        , Cmd.none
                                        )

                                Nothing ->
                                    ( model, Cmd.none )

            DragEnd position ->
                case model.drag of
                    Nothing ->
                        ( model, Cmd.none )

                    Just drag ->
                        ( { model
                            | drag = Nothing
                            , shapes = bringShapeToBackToNormal drag.start.shapeId model.shapes
                          }
                        , Cmd.none
                        )

            MouseOver position ->
                ( model, Cmd.none )

            PutIntoCorrectPositions ->
                ( { model | shapes = solvedTangram }, Cmd.none )

            ToggleDebugOutput ->
                ( { model | showDebug = not model.showDebug }, Cmd.none )



-- VIEW


(=>) =
    (,)


checkbox : msg -> String -> Html.Html msg
checkbox msg name =
    Html.label
        [ Html.Attributes.style [ ( "padding", "20px" ) ]
        ]
        [ Html.input [ Html.Attributes.type_ "checkbox", onClick msg ] []
        , Html.text name
        ]


view : Model -> Html.Html Msg
view model =
    let
        svgShapeAttributes shape =
            [ onMouseDown shape.id
            , onMouseHover shape.id
            , Svg.Attributes.cursor "move"
            ]

        svgShapes =
            model.shapes
                |> Dict.values
                |> List.sortBy (\shape -> shape.zIndex)
                |> List.map (\shape -> draw (svgShapeAttributes shape) shape.color shape.points)

        viewBoxOffsetString =
            [ model.sizes.viewBoxOffset.x
            , model.sizes.viewBoxOffset.y
            , model.sizes.viewBoxSize.width
            , model.sizes.viewBoxSize.height
            ]
                |> List.map (\n -> floor n)
                |> List.map toString
                |> String.join " "
    in
        div []
            [ svg
                [ Svg.Attributes.width (toString model.sizes.canvasSize.width)
                , Svg.Attributes.height (toString model.sizes.canvasSize.height)
                , viewBox viewBoxOffsetString
                ]
                svgShapes
            , hr [] []
            , div
                []
                [ Html.button [ onClick PutIntoCorrectPositions ] [ Html.text "elm-ify svg-polygons" ]
                , checkbox ToggleDebugOutput "Show Debug Output"
                ]
            , hr [] []
            , Html.text <|
                if (model.showDebug) then
                    toString model
                else
                    ""
            ]


pointDecoder : Decode.Decoder Point
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
                , Sub.map (\p -> DragEnd p) (Mouse.ups mousePositionToPoint)
                ]



--Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
