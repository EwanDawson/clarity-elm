module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Debug exposing (log)
import Clarity exposing (..)


type alias Model =
    { title : String }


type Msg
    = ChangeTitle


alertItem : AlertItem Msg
alertItem =
    { text = "Welcome to Clarity Design System!"
    , actions = List.map (\s -> { name = s, message = ChangeTitle }) [ "Action 1", "Action 2" ]
    }


alert : Alert Msg
alert =
    { severity = Success
    , items = [ alertItem ]
    , size = Regular
    , closeable = True
    }


application : ApplicationLayout Model Msg
application =
    { appAlert = Just alert
    , header = (\m -> div [] [])
    , subnav = (\m -> div [] [])
    , contentContainer = (\m -> div [] [])
    }


update : Msg -> Model -> { layout : ApplicationLayout Model Msg, model : Model, command : Cmd Msg }
update message model =
    { layout = application, model = model, command = Cmd.none }


main : Program Never (Clarity.Model Model Msg) (Clarity.Msg Msg)
main =
    Clarity.program
        { init = { layout = application, model = { title = "Hello" }, command = Cmd.none }
        , update = update
        , subscriptions = (\m -> Sub.none)
        }
