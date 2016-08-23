-- Pomodoro Clock


module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Time exposing (Time, second)
import Date
import String


-- Model


type alias Model =
    { min : Int
    , secs : Int
    , mode : Mode
    }


type Mode
    = Start
    | Stop


type Msg
    = StartClock
    | StopClock
    | Tick Time
    | Min String
    | Sec String


initModel : ( Model, Cmd Msg )
initModel =
    ( { min = 1, secs = 5, mode = Stop }, Cmd.none )



-- UPDATE


myStrToInt : String -> Int
myStrToInt =
    Result.withDefault 0 << String.toInt


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Min newMin ->
            ( { model | min = myStrToInt newMin }, Cmd.none )

        Sec newSec ->
            ( { model | secs = myStrToInt newSec }, Cmd.none )

        Tick newTime ->
            if model.mode == Start then
                ( decrementClock model, Cmd.none )
            else
                ( model, Cmd.none )

        StartClock ->
            ( { model | mode = Start }, Cmd.none )

        StopClock ->
            ( { model | mode = Stop }, Cmd.none )


decrementClock : Model -> Model
decrementClock =
    decrementMin << decrementSec


decrementSec : Model -> Model
decrementSec model =
    if model.secs == 0 && model.min > 0 then
        { model | secs = 59 }
    else if model.secs > 0 then
        { model | secs = model.secs - 1 }
    else
        model


decrementMin : Model -> Model
decrementMin model =
    if model.secs == 59 && model.min > 0 then
        { model | min = model.min - 1 }
    else
        model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ style [ ( "display", "flex" ) ] ]
            [ input
                [ style [ ( "font-size", "50px"), ("width", "80px") ]
                , type' "number"
                , onInput Min
                , value <| padSingleDigit model.min
                ]
                []
            , div [ style [ ("font-size", "50px") ] ]
                [ text ":" ]
            , input
                [ style [ ("font-size", "50px"), ("width","80px") ]
                , type' "number"
                , onInput Sec
                , value <| padSingleDigit model.secs
                ]
                []
            ]
        , button
            [ style
                [ ( "width", "100px" )
                , ( "height", "60px" )
                ]
            , onClick StartClock
            ]
            [ text "Start" ]
        , button
            [ style
                [ ( "width", "100px" )
                , ( "height", "60px" )
                ]
            , onClick StopClock
            ]
            [ text "Stop" ]
        , div [] [ text <| toString model ]
        ]


padSingleDigit : Int -> String
padSingleDigit i =
    if i < 10 then
        String.padLeft 2 '0' <| toString i
    else
        toString i



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


main : Program Never
main =
    App.program { init = initModel, update = update, view = view, subscriptions = subscriptions }



-- Erin's Tutorial Part
-- keyButton label =
--     button [ style [ ( "width", "50px"  ] ]
--         [ text label ]
--
--
-- main =
--     div [ style [ ( "background-color", "red" ) ] ]
--         [ text "Hello World"
--         , b [] [ text "World" ]
--         , keyButton "Something"
--         ]
