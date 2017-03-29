module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Debug exposing (log)


main : Program Never Model Msg
main =
    Html.program
        { init = initModel |> noCommand
        , update = (\msg -> \model -> log "Result" (update (log "Message" msg) (log "Model" model)))
        , subscriptions = (\m -> Sub.none)
        , view = view
        }
