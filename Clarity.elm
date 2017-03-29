module Clarity exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- containers


type alias MainContainer model msg =
    { appAlert : model -> Html msg
    , header : model -> Html msg
    , subnav : model -> Html msg
    , contentContainer : model -> Html msg
    }


type alias ContentContainer model msg =
    { contentArea : model -> Html msg
    , sidenav : model -> Html msg
    }


mainContainer : MainContainer model msg -> model -> Html msg
mainContainer layout model =
    div [ class "main-container" ]
        [ div [ class "alert alert-app-level" ] [ layout.appAlert model ]
        , header [ class "header header-6" ] [ layout.header model ]
        , nav [ class "subnav" ] [ layout.subnav model ]
        , div [ class "content-container" ] [ layout.contentContainer model ]
        ]


contentContainer : ContentContainer model msg -> model -> Html msg
contentContainer layout model =
    div [ class "content-container" ]
        [ div [ class "content-area" ] [ layout.contentArea model ]
        , nav [ class "sidenav" ] [ layout.sidenav model ]
        ]



-- alerts


type alias Alert msg =
    { severity : AlertSeverity
    , items : List (AlertItem msg)
    , size : AlertSize
    , onClose : msg
    }


type alias AlertItem msg =
    { text : String
    , actions : List (AlertAction msg)
    }


type alias AlertAction msg =
    { name : String
    , message : msg
    }


type AlertSize
    = Regular
    | Small


type AlertSeverity
    = Danger
    | Info
    | Warning
    | Success


alertClass : Alert msg -> String
alertClass alert =
    (case alert.severity of
        Danger ->
            "alert alert-danger"

        Info ->
            "alert alert-ingo"

        Warning ->
            "alert alert-warning"

        Success ->
            "alert alert-success"
    )
        ++ " "
        ++ (case alert.size of
                Small ->
                    " alert-sm"

                _ ->
                    ""
           )


appAlert : Alert msg -> Html msg
appAlert alert =
    div [ class ((alertClass alert) ++ " alert-app-level") ]
        ((closeButton alert.onClose) :: (List.map alertItem alert.items))


alert : Alert msg -> Html msg
alert alert =
    div [ class (alertClass alert) ]
        ((closeButton alert.onClose) :: (List.map alertItem alert.items))


alertItem : AlertItem msg -> Html msg
alertItem item =
    div [ class "alert-item" ]
        [ span [ class "alert-text" ] [ text item.text ]
        , div [ class "alert-actions" ]
            [ div [ class "alert-action dropdown botton-right" ]
                [ Html.button [ class "dropdown-toggle" ]
                    [ text "Actions"
                    , icon "caret down" []
                    ]
                , div [ class "dropdown-menu" ]
                    (List.map alertAction item.actions)
                ]
            ]
        ]


alertAction : AlertAction msg -> Html msg
alertAction action =
    a [ class "dropdown-item", onClick action.message ] [ text action.name ]



-- icons


icon : String -> List (Attribute msg) -> Html msg
icon shape attrs =
    node "clr-icon" ((attribute "shape" shape) :: attrs) []



-- content


p : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
p n attrs content =
    Html.p ((class ("p" ++ (toString n))) :: attrs) content



-- buttons


type ButtonType
    = PrimaryButton
    | SuccessButton
    | WarningButton
    | DangerButton
    | DisabledButton
    | OutlineButton
    | SuccessOutlineButton
    | InfoOutlineButton
    | WarningOutlineButton
    | DangerOutlineButton
    | DisabledOutlineButton
    | FlatButton
    | FlatDisabledButton
    | InverseButton


type ButtonSize
    = NormalButton
    | SmallButton
    | BlockButton


button : ButtonType -> ButtonSize -> msg -> String -> Html msg
button buttonType buttonSize message buttonText =
    let
        buttonClass =
            "btn"
                ++ " "
                ++ (buttonTypeToClass buttonType)
                ++ " "
                ++ (buttonSizeToClass buttonSize)
    in
        Html.button [ type_ "submit", class buttonClass, onClick message ] [ text buttonText ]


buttonTypeToClass : ButtonType -> String
buttonTypeToClass buttonType =
    case buttonType of
        PrimaryButton ->
            "btn-primary"

        SuccessButton ->
            "btn-success"

        WarningButton ->
            "btn-warning"

        DangerButton ->
            "btn-danger"

        DisabledButton ->
            "btn-primary"

        OutlineButton ->
            "btn-outline"

        SuccessOutlineButton ->
            "btn-success-outline"

        InfoOutlineButton ->
            "btn-info-outline"

        WarningOutlineButton ->
            "btn-warning-outline"

        DangerOutlineButton ->
            "btn-danger-outline"

        DisabledOutlineButton ->
            "btn-outline"

        FlatButton ->
            "btn-link"

        FlatDisabledButton ->
            "btn-link"

        InverseButton ->
            "btn-inverse"


buttonSizeToClass : ButtonSize -> String
buttonSizeToClass buttonSize =
    case buttonSize of
        NormalButton ->
            ""

        SmallButton ->
            "btn-sm"

        BlockButton ->
            "btn-block"


closeButton : msg -> Html msg
closeButton message =
    Html.button [ type_ "button", class "close", onClick message ] [ icon "close" [] ]



-- badges


type BadgeType
    = GreyBadge
    | PurpleBadge
    | OrangeBadge
    | LightBlueBadge
    | InfoBadge
    | SuccessBadge
    | WarningBadge
    | DangerBadge


badge : BadgeType -> String -> Html msg
badge badgeType badgeText =
    let
        badgeClass =
            "badge"
                ++ case badgeType of
                    GreyBadge ->
                        ""

                    PurpleBadge ->
                        " badge-purple"

                    OrangeBadge ->
                        " badge-blue"

                    LightBlueBadge ->
                        " badge-light-blue"

                    InfoBadge ->
                        " badge-info"

                    SuccessBadge ->
                        " badge-success"

                    WarningBadge ->
                        " badge-warning"

                    DangerBadge ->
                        " badge-danger"
    in
        span [ class badgeClass ] [ text badgeText ]
