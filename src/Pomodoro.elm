-- Pomodoro Clock


port module Pomodoro exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Time exposing (Time, second)
import Date
import String


main : Program Never
main =
    App.program { init = initModel, update = update, view = view, subscriptions = subscriptions }



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
    ( Model 0 5 Stop, Cmd.none )



-- UPDATE
--For Elm 0.17.1, port needs AT LEAST 1 parameter???
--Reference: https://groups.google.com/forum/#!forum/elm-dev -> topic: "JS interop type confusion"


port ring : String -> Cmd msg


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
            if model.mode == Start && (model.min > 0 || model.secs > 0) then
                ( decrementClock model, Cmd.none )
            else if model.mode == Start && model.min == 0 && model.secs == 0 then
                ( { model | mode = Stop }, ring "" )
            else
                ( model, Cmd.none )

        StartClock ->
            ( checkSec { model | mode = Start }, Cmd.none )

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


checkSec : Model -> Model
checkSec model =
    if model.secs > 59 then
        { model | secs = 59 }
    else if model.secs < 0 then
        { model | secs = 0 }
    else
        model



-- VIEW


centerCSS : Attribute Msg
centerCSS =
    style [ ( "display", "flex" ), ( "justify-content", "center" ), ( "align-items", "center" ) ]


numberCSS : Model -> (String -> Msg) -> List (Attribute Msg)
numberCSS model num =
    [ style [ ( "font-size", "50px" ), ( "width", "90px" ) ]
    , type' "number"
    , readOnlyIfRunning model
    , onInput num
    ]


type Start_Stop_btn
    = StartButton
    | StopButton


hideButtonCSS : Model -> Start_Stop_btn -> Attribute Msg
hideButtonCSS model btn =
    case btn of
        StartButton ->
            if model.mode == Start then
                hidden True
            else
                hidden False

        StopButton ->
            if model.mode == Stop then
                hidden True
            else
                hidden False


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "background-color", "red" )
            , ( "width", "250px" )
            , ( "height", "300px" )
            , ( "position", "relative" )
            , ( "top", "50px" )
            , ( "left", "500px" )
            ]
        ]
        [ div
            [ centerCSS
            , style
                [ ( "position", "relative" )
                , ( "top", "50px" )
                ]
            ]
            [ input
                ([ value <| padSingleDigit model.min ]
                    ++ numberCSS model Min
                )
                []
            , div [ style [ ( "font-size", "50px" ) ] ]
                [ text ":" ]
            , input
                ([ Html.Attributes.max "60"
                 , Html.Attributes.min "0"
                 , value <| padSingleDigit model.secs
                 ]
                    ++ numberCSS model Sec
                )
                []
            ]
        , div
            [ centerCSS
            , style
                [ ( "position", "relative" )
                , ( "top", "50px" )
                ]
            ]
            [ button
                [ style
                    [ ( "width", "200px" )
                    , ( "height", "60px" )
                    , ( "position", "relative" )
                    , ( "top", "30px" )
                    , ( "font-size", "large" )
                    ]
                , hideButtonCSS model StartButton
                , onClick StartClock
                ]
                [ text "Start" ]
            , button
                [ style
                    [ ( "width", "200px" )
                    , ( "height", "60px" )
                    , ( "position", "relative" )
                    , ( "top", "30px" )
                    , ( "font-size", "large" )
                    ]
                , hideButtonCSS model StopButton
                , onClick StopClock
                ]
                [ text "Stop" ]
            ]
        , div
            [ centerCSS
            , style
                [ ( "position", "relative" )
                , ( "top", "100px" )
                ]
            ]
            [ text <| toString model ]
        ]


padSingleDigit : Int -> String
padSingleDigit i =
    if i < 10 then
        String.padLeft 2 '0' <| toString i
    else
        toString i


readOnlyIfRunning : Model -> Attribute Msg
readOnlyIfRunning model =
    if (model.mode == Start) then
        readonly True
    else
        readonly False



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick
