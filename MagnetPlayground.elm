module MagnetPlayground exposing (..)

import MagnetPieces exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (viewBox)
import Html


colors =
    { gray = "#5a6378"
    , green = "#83c833"
    , orange = "#efa500"
    , blue = "#5fb4ca"
    }


type alias Shape =
    { points : List Point
    , color : String
    }



-- MODEL


type alias Model =
    { shapes : List Shape
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Point
    , current : Point
    }


type Msg
    = NothingYet



-- | DragStart Position
-- | DragAt Position
-- | DragEnd Position


init : ( Model, Cmd Msg )
init =
    ( { shapes =
            [ { points = triangle 2 |> rotate (45 + 180)
              , color = colors.gray
              }
            , { points = triangle 2 |> rotate -45
              , color = colors.blue
              }
            ]
      , drag = Nothing
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
    , Cmd.none
    )



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        foo =
            List.map (\shape -> draw shape.color shape.points) model.shapes
    in
        svg [ viewBox "-3 -3 10 10" ] foo


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
