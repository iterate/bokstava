
module Main exposing (Model, Msg, update, view, subscriptions, init, main)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Browser
import Browser.Events as Events
import Maybe

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }


type alias Model = Float


init : flags -> (Model, Cmd Msg)
init _ =
    ( 0, Cmd.none)


type Msg
    = TimeDelta Float


update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
    case msg of
        TimeDelta delta -> ( delta, Cmd.none)
            
    


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onAnimationFrameDelta TimeDelta


view : Model -> Html Msg
view t =
    div [ style "font-family" "Share Tech Mono", 
            style "letter-spacing" "-1 px",
            style "line-height" "12px"] 
    (  List.range 1 70
    |> List.map (\y -> 
        div [] (List.range 1 300
        |> List.map(\x ->        oneCell t x y        ))))


oneCell : Float -> Int -> Int -> Html msg
oneCell t x y =
    xyToF t x y |> floatToCell |> text

xyToF : Float -> Int -> Int -> Float
xyToF t x y =
    (t/99) *   toFloat x * (toFloat y/7)

floatToCell : Float -> String
floatToCell f =
    let 
        (i, _) = f |> String.fromFloat |> String.uncons |> Maybe.withDefault ('0', "")
    in
        case i of
            '0' ->  " "
            '1' ->  " "
            '2' ->  "I"
            '3' ->  "O"    
            '4' ->  "0"    
            '5' ->  "H"    
            '6' ->  "#"    
            '7' ->  "M"    
            '8' ->  "N"    
            '9' ->  "V"    
            _ -> " "
        