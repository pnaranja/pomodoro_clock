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
    { time : Time
    , mode : Mode
    }


type Mode
    = Start
    | Stop


type Msg
    = StartClock
    | StopClock
    | Tick Time


initModel : ( Model, Cmd Msg )
initModel =
    ( { time = 30, mode = Stop }, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            if model.mode == Start
            then
                ( { model | time = newTime }, Cmd.none )
            else
                (model,Cmd.none)

        StartClock ->
            ( { model | mode = Start }, Cmd.none )

        StopClock ->
            ( { model | mode = Stop }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ style [ ( "font-size", "60px" ) ] ]
            [ text <| toString <| 60 - (Date.second <| Date.fromTime <| model.time) ]
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


main : Program Never
main =
    App.program { init = initModel, update = update, view = view, subscriptions = subscriptions }



-- Erin's Tutorial Part
-- keyButton label =
--     button [ style [ ( "width", "50px" ) ] ]
--         [ text label ]
--
--
-- main =
--     div [ style [ ( "background-color", "red" ) ] ]
--         [ text "Hello World"
--         , b [] [ text "World" ]
--         , keyButton "Something"
--         ]
