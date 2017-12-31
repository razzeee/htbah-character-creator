module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Slider exposing (..)

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


type alias Character =
    { name : String
    , firstName : String
    , stature : String
    , religion : String
    , sex : String
    , age : String
    , job : String
    , familyStatus : String
    , lifePoints : Int
    , actsSlider : Slider.Model
    , knowledge : Int
    , interact : Int
    , unassignedPoints : Int
    }

initSlider =
     { val = 1 , minVal = 0 , maxVal = 100 , step = 1 }

init : (Model, Cmd Msg)
init =
    (Model (Character "" "" "" "" "" "" "" "" 0 initSlider 0 0 500) Page1, Cmd.none)



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
    | ChangeLifepoints Int
    | ChangeKnowledge Int
    | ChangeInteract Int
    | ChangePage Page
    | ActsSlider Slider.Msg


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

        ActsSlider acts ->
            let
                updatedModel =
                    Slider.update acts model.character.actsSlider
                character =
                    model.character
            in
                ( { model | character = { character | actsSlider = Tuple.first(updatedModel) } }, Cmd.none )

        ChangeKnowledge knowledge ->
            let
                character =
                    model.character
            in
                ({ model | character = { character | knowledge = knowledge } }, Cmd.none)

        ChangeInteract interact ->
            let
                character =
                    model.character
            in
                ({ model | character = { character | interact = interact } }, Cmd.none)

        ChangePage page ->
            ({ model | page = page }, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Page1 ->
            page1 model

        Page2 ->
            page2 model

        Page3 ->
            page3 model

        CharacterSheet ->
            characterSheet model


setTabActive : { b | page : a } -> a -> Attribute msg
setTabActive model page =
    if model.page == page then
        class "is-active"
    else
        class ""


drawTabs : Model -> Html Msg
drawTabs model =
    div [ class "tabs is-boxed" ]
        [ ul []
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

addInputSlider model title inputMessage =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text title ]
        , div [ class "control" ]
            [ 
            ]
        ]


page1 : Model -> Html Msg
page1 model =
    div [ class "container" ]
        [ drawTabs model
        , addInput model "Vorname" "Der Vorname deines Characters" ChangeFirstname
        , addInput model "Name" "Der Nachname deines Characters" ChangeName
        , addInput model "Statur" "Die Statur deines Characters" ChangeStature
        , addInput model "Religion" "Deine Religion" ChangeReligion
        , (nextButton model Page2)
        ]


page2 : Model -> Html Msg
page2 model =
    div [ class "container" ]
        [ drawTabs model
        , addInput model "Geschlecht" "Mit welchem Geschlecht identifizierst du dich?" ChangeSex
        , addInput model "Alter" "Wie alt bist du?" ChangeAge
        , addInput model "Beruf" "Dein Beruf" ChangeJob
        , addInput model "Familienstand" "Wie ist dein Familienstand?" ChangeFamilystatus
        , (nextButton model Page3)
        ]


page3 : Model -> Html Msg
page3 model =
    div [ class "container" ]
        [ drawTabs model
       , Html.map ActsSlider <| Slider.view model.character.actsSlider

        {--, addInput model "Lebenspunkte" "Bis 100" ChangeLifepoints
        , addInput model "Handeln" "Bis 100" ChangeActs
        , addInput model "Wissen" "Bis 100" ChangeKnowledge
        , addInput model "Interagieren" "Bis 100" ChangeInteract--}
        , (nextButton model CharacterSheet)
        ]


characterSheet : Model -> Html Msg
characterSheet model =
    div [ class "container" ]
        []
