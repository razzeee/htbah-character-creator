module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Input.Number as Number

main =
    Html.program { init = init, view = view, update = update, subscriptions = always Sub.none }

-- MODEL


type alias Model =
    { character : Character
    , page : Page
    }


type Page
    = Page1
    | Page2
    | Page3
    | CharacterSheet

type alias ListItem =
    { id : String,
    name : String,
    value : Maybe Int}

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
    }

init : (Model, Cmd Msg)
init =
    (Model (Character "" "" "" "" "" "" "" "" (Just 100) [(ListItem "knowledge" "Wissen" (Just 0))] [(ListItem"interact" "Interagieren" (Just 0))] [(ListItem "acts" "Handeln" (Just 0))] "500") Page1, Cmd.none)



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
        Page1 ->
            headerAndSection page1 model

        Page2 ->
            headerAndSection page2 model

        Page3 ->
            headerAndSection page3 model

        CharacterSheet ->
            headerAndSection characterSheet model


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
                [ setTabActive model Page1
                ]
                [ a []
                    [ text "Seite 1" ]
                ]
            , li [ setTabActive model Page2 ]
                [ a []
                    [ text "Seite 2" ]
                ]
            , li [ setTabActive model Page3 ]
                [ a []
                    [ text "Seite 3" ]
                ]
            , li [ setTabActive model CharacterSheet ]
                [ a []
                    [ text "Character Sheet" ]
                ]
            ] 
            ] 
        ] 
    ]


nextButton : Model -> Page -> Html Msg
nextButton model page =
    div [ class "field is-grouped" ]
        [ div [ class "control" ]
            [ button [ class "button is-link", onClick (ChangePage page) ]
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
                [ class "numberInput"
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
                [ class "numberInput"
                ]
                list.value
            ]
        ]

headerAndSection page model =
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
        page model
    ]

page1 : Model -> Html Msg
page1 model =
    div [ class "container" ]
        [ 
        addInput model "Vorname" "Der Vorname deines Characters" ChangeFirstname
        , addInput model "Name" "Der Nachname deines Characters" ChangeName
        , addInput model "Statur" "Die Statur deines Characters" ChangeStature
        , addInput model "Religion" "Deine Religion" ChangeReligion
        , (nextButton model Page2)
        ]

page2 : Model -> Html Msg
page2 model =
    div [ class "container" ]
        [  addInput model "Geschlecht" "Mit welchem Geschlecht identifizierst du dich?" ChangeSex
        , addInput model "Alter" "Wie alt bist du?" ChangeAge
        , addInput model "Beruf" "Dein Beruf" ChangeJob
        , addInput model "Familienstand" "Wie ist dein Familienstand?" ChangeFamilystatus
        , (nextButton model Page3)
        ]


page3 : Model -> Html Msg
page3 model =
    div [ class "container" ]
        [  div [ class "field" ]
        [ label [ class "label" ]
            [ text "Verbleibende Skillpunkte" ]
        , div [ class "control" ]
            [ input [ class "input is-large", type_ "text", value model.character.unassignedPoints, readonly True, disabled True ]
                []
            ]
        ]
        , addInputNumerical model.character.lifePoints "Lebenspunkte" ChangeLifepoints
        , div [ class "columns"] [
        drawNumericalList model.character.actsList
        , drawNumericalList model.character.knowledgeList
        , drawNumericalList model.character.interactList
        ]
        , (nextButton model CharacterSheet)
        ]


characterSheet : Model -> Html Msg
characterSheet model =
    div [ class "container" ]
        []
