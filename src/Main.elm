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
    { t : Float
    , delta : Float
    }


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
        , style "color" "darkslategray"
        ]
        [ div [] [ text ("BOKSTAVA DENTATA " ++ String.fromFloat model.delta ++ " " ++ String.fromFloat model.t) ]
        , Html.pre [ style "font-family" "Share Tech Mono", style "font-size" "13px" ]
            [ text
                ((List.range 0 70
                    |> List.map
                        (\y ->
                            List.range 0 150 |> List.map (\x -> oneCell model.t x y) |> String.join ""
                        )
                 )
                    |> String.join "\n"
                )
            ]
        , Html.pre [ style "font-family" "Share Tech Mono" ] [ text (sampleCell model.t ++ " " ++ sampleNum model.t) ]
        ]


sampleNum t =
    String.padRight 19 ' ' (String.fromFloat (calc2 t 0 0))


sampleCell : Float -> String
sampleCell t =
    oneCell t 0 0


oneCell : Float -> Int -> Int -> String
oneCell t x y =
    let
        calc =
            calc2

        render =
            render3
    in
    calc t x y |> render


calc2 : Float -> Int -> Int -> Float
calc2 t x y =
    let
        xf =
            toFloat x

        yf =
            toFloat y

        xx =
            cos (fMod 360 (xf / (8 + fMod 1000 (t / 1000)) + t / 1000 + (yf / 100) + 1))

        yy =
            cos (fMod 360 (yf / (8 + fMod 100 (t / 100)) + t / 1000))
    in
    xx + yy


calc1 : Float -> Int -> Int -> Float
calc1 t x y =
    let
        xf =
            toFloat x

        yf =
            toFloat y

        xx =
            sin (fMod 360 (xf / 8 + t / 1000))

        yy =
            sin (fMod 360 (yf / 8 + t / 1000))
    in
    xx + yy


fMod : Int -> Float -> Float
fMod i f =
    let
        deltpå =
            f / toFloat i

        gange =
            f * deltpå
    in
    f - gange


calc0 : Float -> Int -> Int -> Float
calc0 t x y =
    let
        yy =
            toFloat y

        xx =
            toFloat x
    in
    ((t / 99) * xx * yy / 7) / 360 |> fMod 1


render3 : Float -> String
render3 f =
    let
        a =
            f / 2 |> abs

        b =
            a * 6

        n =
            round b
    in
    case n of
        0 ->
            "."

        1 ->
            ","

        2 ->
            "v"

        3 ->
            "V"

        4 ->
            "N"

        5 ->
            "M"

        6 ->
            "#"

        _ ->
            " "


render2 : Float -> String
render2 f =
    let
        a =
            f * 8

        n =
            round a
    in
    case n of
        0 ->
            "▁"

        1 ->
            "▂"

        2 ->
            "▃"

        3 ->
            "▄"

        4 ->
            "▅"

        5 ->
            "▆"

        6 ->
            "▇"

        7 ->
            "█"

        _ ->
            " "


render1 : Int -> String
render1 f =
    case modBy 10 f of
        0 ->
            "▁"

        1 ->
            "▂"

        2 ->
            "▃"

        3 ->
            "▄"

        4 ->
            "▅"

        5 ->
            "▆"

        6 ->
            "▇"

        7 ->
            "█"

        8 ->
            "░"

        9 ->
            "▒"

        _ ->
            " "


render0 : Int -> String
render0 f =
    case modBy 10 f of
        0 ->
            ">>"

        1 ->
            "-."

        2 ->
            "-:"

        3 ->
            ".-"

        4 ->
            ".."

        5 ->
            ".:"

        6 ->
            ":-"

        7 ->
            ":."

        8 ->
            "::"

        9 ->
            "--"

        _ ->
            " "
