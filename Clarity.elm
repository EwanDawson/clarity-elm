module Clarity exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias UpdateResult model msg =
    { layout : ApplicationLayout model msg, model : model, command : Cmd msg }


type alias Config model msg =
    { init : UpdateResult model msg
    , update : msg -> model -> UpdateResult model msg
    , subscriptions : model -> Sub msg
    }


type alias Model model msg =
    { layout : ApplicationLayout model msg
    , model : model
    }


type Msg msg
    = Message msg
    | CloseAppAlert


update :
    (msg -> model -> UpdateResult model msg)
    -> Msg msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg msg) )
update updater message application =
    case message of
        Message msg ->
            let
                result =
                    updater msg application.model
            in
                ( { application | layout = result.layout, model = result.model }
                , Cmd.map Message result.command
                )

        CloseAppAlert ->
            let
                layout =
                    application.layout

                updatedLayout =
                    case layout.appAlert of
                        Just alert ->
                            { layout | appAlert = Nothing }

                        Nothing ->
                            layout
            in
                ( { application | layout = updatedLayout }, Cmd.none )


program : Config model msg -> Program Never (Model model msg) (Msg msg)
program config =
    let
        model =
            { layout = config.init.layout
            , model = config.init.model
            }

        init =
            ( model, Cmd.map Message config.init.command )
    in
        Html.program
            { init = init
            , update = update config.update
            , subscriptions = (\m -> Sub.none)
            , view = view
            }


type alias ApplicationLayout model msg =
    { appAlert : Maybe (Alert msg)
    , header : model -> Html (Msg msg)
    , subnav : model -> Html (Msg msg)
    , contentContainer : model -> Html (Msg msg)
    }


view : Model model msg -> Html (Msg msg)
view application =
    let
        layout =
            application.layout

        model =
            application.model
    in
        div [ class "main-container" ]
            ((Maybe.withDefault [] (Maybe.map (\a -> [ appAlert a ]) layout.appAlert))
                ++ [ header [ class "header header-6" ] [ layout.header model ]
                   , nav [ class "subnav" ] [ layout.subnav model ]
                   , div [ class "content-container" ] [ layout.contentContainer model ]
                   ]
            )



-- containers


type alias ContentContainer model msg =
    { contentArea : model -> Html (Msg msg)
    , sidenav : model -> Html (Msg msg)
    }


contentContainer : ContentContainer model msg -> model -> Html (Msg msg)
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
    , closeable : Bool
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


type AlertLevel
    = AppLevelAlert
    | ContainerLevelAlert


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


appAlert : Alert msg -> Html (Msg msg)
appAlert alert =
    viewAlert AppLevelAlert alert


alert : Alert msg -> Html (Msg msg)
alert alert =
    viewAlert ContainerLevelAlert alert


viewAlert : AlertLevel -> Alert msg -> Html (Msg msg)
viewAlert alertLevel alert =
    let
        alertItems =
            List.map (alertItem alertLevel) alert.items

        classString =
            case alertLevel of
                AppLevelAlert ->
                    (alertClass alert) ++ " alert-app-level"

                ContainerLevelAlert ->
                    alertClass alert
    in
        div [ class classString ]
            (if alert.closeable then
                ((closeButton CloseAppAlert) :: (alertItems))
             else
                alertItems
            )


alertItem : AlertLevel -> AlertItem msg -> Html (Msg msg)
alertItem level item =
    div [ class "alert-item" ]
        ((span [ class "alert-text" ] [ text item.text ])
            :: if List.isEmpty item.actions then
                []
               else
                [ alertActions level item.actions ]
        )


alertActions : AlertLevel -> List (AlertAction msg) -> Html (Msg msg)
alertActions level actions =
    case level of
        AppLevelAlert ->
            div [ class "alert-actions" ]
                (List.map buttonAlertAction actions)

        ContainerLevelAlert ->
            case actions of
                [ action1, action2 ] ->
                    div [ class "alert-actions" ]
                        (List.map buttonAlertAction [ action1, action2 ])

                _ ->
                    div [ class "alert-actions" ]
                        [ div [ class "alert-action dropdown botton-right" ]
                            [ Html.button [ class "dropdown-toggle" ]
                                [ text "Actions"
                                , icon "caret down" []
                                ]
                            , div [ class "dropdown-menu" ]
                                (List.map dropdownAlertAction actions)
                            ]
                        ]


buttonAlertAction : AlertAction msg -> Html (Msg msg)
buttonAlertAction action =
    Html.button [ class "btn alert-action", onClick (Message action.message) ] [ text action.name ]


dropdownAlertAction : AlertAction msg -> Html (Msg msg)
dropdownAlertAction action =
    a [ class "dropdown-item", onClick (Message action.message) ] [ text action.name ]



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



-- card


type CardContent msg
    = CardHeader { text : String }
    | CardBlock { title : Maybe String, text : Html msg }
    | CardFooter { contents : Html msg }
    | CardImage (Html.Attribute msg)


type CardBlockContent msg
    = CardBlockTitle String


type alias Card msg =
    { contents : List CardContent
    , onClick : Maybe msg
    }
