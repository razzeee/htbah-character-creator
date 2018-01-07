module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Input.Number as Number
import List.Extra exposing (..)

main =
    Html.program { init = init, view = view, update = update, subscriptions = always Sub.none }

-- MODEL


type alias Model =
    { character : Character
    , page : Page
    }


type Page
    = PageBaseProperties
    | PageSkillpoints
    | CharacterSheet

type alias ListItem =
    { id : String,
    name : String,
    value : Maybe Int,
    masterItem : Bool}

type alias Character =
    { name : String
    , firstName : String
    , stature : String
    , religion : String
    , sex : String
    , age : String
    , job : String
    , familyStatus : String
    , lifePoints : Maybe Int
    , knowledgeList : List  ListItem
    , interactList : List  ListItem
    , actsList : List ListItem
    , unassignedPoints : String
    , picture : String
    }

init : (Model, Cmd Msg)
init =
    (Model (Character "" "" "" "" "" "" "" "" (Just 100) [(ListItem "knowledge" "Wissen" (Just 0) True)] [(ListItem"interact" "Interagieren" (Just 0) True)] [(ListItem "acts" "Handeln" (Just 0) True)] "500" "img/character.jpg") PageBaseProperties, Cmd.none)



-- UPDATE


type Msg
    = ChangeName String
    | ChangeFirstname String
    | ChangeStature String
    | ChangeReligion String
    | ChangeSex String
    | ChangeAge String
    | ChangeJob String
    | ChangeFamilystatus String
    | ChangeLifepoints (Maybe Int)
    | ChangePage Page
    | FocusChanged Bool
    | ChangeListItem (Maybe Int)

update msg model =
    case msg of
        ChangeName name ->
            let
                character =
                    model.character
            in
                ({ model | character = { character | name = name } }, Cmd.none)

        ChangeFirstname firstName ->
            let
                character =
                    model.character
            in
                ({ model | character = { character | firstName = firstName } }, Cmd.none)

        ChangeStature stature ->
            let
                character =
                    model.character
            in
                ({ model | character = { character | stature = stature } }, Cmd.none)

        ChangeReligion religion ->
            let
                character =
                    model.character
            in
                ({ model | character = { character | religion = religion } }, Cmd.none)

        ChangeSex sex ->
            let
                character =
                    model.character
            in
                ({ model | character = { character | sex = sex } }, Cmd.none)

        ChangeAge age ->
            let
                character =
                    model.character
            in
                ({ model | character = { character | age = age } }, Cmd.none)

        ChangeJob job ->
            let
                character =
                    model.character
            in
                ({ model | character = { character | job = job } }, Cmd.none)

        ChangeFamilystatus familyStatus ->
            let
                character =
                    model.character
            in
                ({ model | character = { character | familyStatus = familyStatus } }, Cmd.none)

        ChangeLifepoints lifePoints ->
            let
                character =
                    model.character
            in
                ({ model | character = { character | lifePoints = lifePoints } }, Cmd.none)

        ChangeListItem listitem ->
            (model, Cmd.none)

        ChangePage page ->
            ({ model | page = page }, Cmd.none)

        FocusChanged bool ->
            (model, Cmd.none)




-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        PageBaseProperties ->
            renderContent pageBaseproperties model

        PageSkillpoints ->
            renderContent pageSkillpoints model

        CharacterSheet ->
            renderContent characterSheet model


setTabActive : { b | page : a } -> a -> Attribute msg
setTabActive model page =
    if model.page == page then
        class "is-active"
    else
        class ""


drawTabs : Model -> Html Msg
drawTabs model =
    div [ class "hero-foot" ]
    [  nav [ class "tabs is-boxed is-fullwidth" ]
        [ div [ class "container" ]
            [  ul []
                [ li
                [ setTabActive model PageBaseProperties
                ]
                [ a []
                    [ text "Basiseigenschaften" ]
                ]
            , li [ setTabActive model PageSkillpoints ]
                [ a []
                    [ text "Skillpunkte" ]
                ]
            , li [ setTabActive model CharacterSheet ]
                [ a []
                    [ text "Character-Blatt" ]
                ]
            ] 
            ] 
        ] 
    ]


nextButton : Model -> Page -> Page -> Html Msg
nextButton model previousPage nextPage =
    div [ class "field is-grouped is-centered" ]
        [ div [ class "control" ]
            [ button [ class "button is-link", onClick (ChangePage previousPage) ]
                [ text "Vorherige" ]
            ],
            div [ class "control" ]
            [ button [ class "button is-link", onClick (ChangePage nextPage) ]
                [ text "Nächste" ]
            ]
        ]


addInput model title placeholderText inputMessage =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text title ]
        , div [ class "control" ]
            [ input [ class "input", type_ "text", placeholder placeholderText, onInput inputMessage ]
                []
            ]
        ]

addInputNumerical value title inputMessage =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text title ]
        , div [ class "control" ]
            [ Number.input
                { onInput = inputMessage
                , maxLength = Nothing
                , maxValue = Just 100
                , minValue = Just 0,
                hasFocus = Just FocusChanged
                }
                [ class "input"
                ]
                value
            ]
        ]


drawNumericalList list =
    div [ class "column"]
         (List.map createNumbers list)


createNumbers: ListItem -> Html Msg
createNumbers list =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text list.name ]
        , div [ class "control" ]
        [
        Number.input
                { onInput = ChangeListItem
                , maxLength = Nothing
                , maxValue = Just 100
                , minValue = Just 0,
                hasFocus = Just FocusChanged
                }
                [ class "input"
                ]
                list.value
            ]
        ]

renderContent page model =
    div [] 
    [section [ class "hero is-primary" ]
    [  div [ class "hero-body" ]
        [  div [ class "container" ]
            [  h1 [ class "title" ]
                [ text "How to be a hero" ] 
            , h2 [ class "subtitle" ]
                [ text "Characterblatt-Ersteller" ] 
            ] 
        ],
        drawTabs model
    ],
        page model,
        footer [ class "footer" ]
    [ div [ class "container" ]
        [ div [ class "content has-text-centered" ]
            [ p []
                [ text "How to be a hero character sheet creator by Kolja Lampe. The source code is licensed MIT."
                ]
            ]
        ]
    ]
    ]

pageBaseproperties : Model -> Html Msg
pageBaseproperties model =
    div [ class "container" ]
        [ 
        addInput model "Vorname" "Der Vorname deines Characters" ChangeFirstname
        , addInput model "Name" "Der Nachname deines Characters" ChangeName
        , addInput model "Statur" "Die Statur deines Characters" ChangeStature
        , addInput model "Religion" "Deine Religion" ChangeReligion
        , addInput model "Geschlecht" "Mit welchem Geschlecht identifizierst du dich?" ChangeSex
        , addInput model "Alter" "Wie alt bist du?" ChangeAge
        , addInputNumerical model.character.lifePoints "Lebenspunkte" ChangeLifepoints
        , addInput model "Beruf" "Dein Beruf" ChangeJob
        , addInput model "Familienstand" "Wie ist dein Familienstand?" ChangeFamilystatus
        , (nextButton model PageBaseProperties PageSkillpoints)
        ]

pageSkillpoints : Model -> Html Msg
pageSkillpoints model =
    div [ class "container" ]
        [ 
             div [ class "field" ]
        [ label [ class "label has-text-centered" ]
            [ text "Verbleibende Skillpunkte" ]
        , div [ class "control" ]
            [ input [ class "input is-large has-text-centered is-static", type_ "text", value model.character.unassignedPoints, readonly True, disabled True ]
                []
            ]
        ]
        , div [ class "columns"] [
        drawNumericalList model.character.actsList
        , drawNumericalList model.character.knowledgeList
        , drawNumericalList model.character.interactList
        ]
        , (nextButton model PageBaseProperties CharacterSheet)
        ]


characterSheet : Model -> Html Msg
characterSheet model =
    div [ class "container" ]
        [ 
             div [ class "field" ]
        [ label [ class "label has-text-centered" ]
            [ text "Charakterbogen" ]
        ]
        , div [ class "columns"] [
            div [ class "column" ] [
                showReadonlyValue model.character.firstName "Vorname"
                ,showReadonlyValue model.character.sex "Geschlecht"
            ]
            , div [ class "column" ] [
                showReadonlyValue model.character.name "Name"
                ,showReadonlyValue model.character.age "Alter"
            ]
            , div [ class "column" ] [
                img [src model.character.picture] []
            ]
            , div [ class "column" ] [
                showReadonlyValue model.character.stature "Statur"
                , showReadonlyValue model.character.job "Beruf"
            ]
            , div [ class "column" ] [
                showReadonlyValue model.character.religion "Religion"
                , showReadonlyValue model.character.familyStatus "Familienstand"
            ]
        ]
        , div [ class "columns"] [
            div [ class "column" ] [
            ]
            , div [ class "column" ] [
            ]
            , div [ class "column" ] [
                showReadonlyLifepoints model.character.lifePoints "Lebenspunkte"
            ]
            , div [ class "column" ] [
            ]
            , div [ class "column" ] [
            ]
        ]
        , div [ class "columns"] [
            div [ class "column" ] [
                showReadonlyListItemValue (List.Extra.find (\value -> value.masterItem) model.character.actsList) "Handeln"
            ]
            , div [ class "column" ] [
                showReadonlyListItemValue (List.Extra.find (\value -> value.masterItem) model.character.knowledgeList) "Wissen"
            ]
            , div [ class "column" ] [
                showReadonlyListItemValue (List.Extra.find (\value -> value.masterItem) model.character.interactList) "Interagieren"
            ]
        ]
        , div [ class "columns"] [
            div [ class "column" ] []
            , div [ class "column" ] []
            , div [ class "column" ] []
        ]
        , (nextButton model PageSkillpoints CharacterSheet)
        ]

showReadonlyValue: String -> String -> Html Msg
showReadonlyValue value title =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text title ]
        , div [ class "control" ]
            [ input [ class "input", type_ "text", disabled True, readonly True]
                [ text value ]
            ]
        ]


showReadonlyListItemValue listItem title =
    case listItem of
        Nothing ->
            div [ class "field" ]
                [ label [ class "label" ]
                    [ text title ]
                , div [ class "control" ]
                    [ input [ class "input", type_ "text", disabled True, readonly True]
                        [ text "" ]
                    ]
                ]
        Just value ->
            div [ class "field" ]
                [ label [ class "label" ]
                    [ text title ]
                , div [ class "control" ]
                    [ input [ class "input", type_ "text", disabled True, readonly True]
                        [ text (convertMaybeIntToString value.value) ]
                    ]
                ]

showReadonlyLifepoints: Maybe Int -> String -> Html Msg
showReadonlyLifepoints value title =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text title ]
        , div [ class "control" ]
            [ Number.input
                { onInput = ChangeLifepoints
                , maxLength = Nothing
                , maxValue = Just 100
                , minValue = Just 0,
                hasFocus = Just FocusChanged
                }
                [ class "input", disabled True, readonly True
                ]
                value
            ]
        ]
    
convertMaybeIntToString input =
    case input of
        Nothing ->
        "0"
        Just value ->
        toString value