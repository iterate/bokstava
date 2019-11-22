module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Browser.Events as Events
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { t : Float, delta : Float }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Model 0 0, Cmd.none )


type Msg
    = TimeDelta Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeDelta delta ->
            ( Model (delta + model.t) delta, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onAnimationFrameDelta TimeDelta


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "Share Tech Mono"
        , style "letter-spacing" "-1 px"
        , style "line-height" "10px"
        , style "font-size" "13px"
        ]
        [ div [] [ text (String.fromFloat model.delta) ]
        , div []
            [ text
                ((List.range 0 70
                    |> List.map
                        (\y ->
                            List.range 0 280 |> List.map (\x -> oneCell model.t x y) |> String.join ""
                        )
                 )
                    |> String.join " "
                )
            ]
        ]


oneCell : Float -> Int -> Int -> String
oneCell t x y =
    xyToF t x y |> floatToCell


xyToF : Float -> Int -> Int -> Int
xyToF t x y =
    let
        tt =
            floor t

        yy =
            y
    in
    (tt // 99) * x * yy // 7


floatToCell : Int -> String
floatToCell f =
    case modBy 10 f of
        0 ->
            "®"

        1 ->
            "."

        2 ->
            ":"

        3 ->
            "+"

        4 ->
            "o"

        5 ->
            "x"

        6 ->
            "O"

        7 ->
            "X"

        8 ->
            "N"

        9 ->
            "M"

        _ ->
            " "
