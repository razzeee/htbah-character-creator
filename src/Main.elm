module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Input.Number as Number
import Input.Text as Text
import List.Extra exposing (..)

main =
    Html.program { init = init, view = view, update = update, subscriptions = always Sub.none }

-- MODEL


type alias Model =
    { character : Character
    , page : Page
    , inputNewActsItem : String
    , inputNewKnowledgeItem : String
    , inputNewInteractItem : String
    }


type Page
    = PageBaseProperties
    | PageSkillpoints
    | PageCharacterSheet

type alias ListItem =
    {
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
    (Model (Character "" "" "" "" "" "" "" "" (Just 100) 
    [(ListItem "Wissen" (Just 0) True)] [(ListItem "Interagieren" (Just 0) True)] [(ListItem "Handeln" (Just 0) True)] 
    "500" "img/character.jpg") PageBaseProperties "" "" "", Cmd.none)


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
    | ChangeActsList String (Maybe Int)
    | ChangeKnowledgeList String (Maybe Int)
    | ChangeInteractList String (Maybe Int)
    | ChangeActsListRemoveItem String
    | ChangeKnowledgeListRemoveItem String
    | ChangeInteractListRemoveItem String
    | InputNewItemActs String
    | ChangeActsAddNewItem
    | InputNewItemKnowledge String
    | ChangeKnowledgeAddNewItem
    | InputNewItemInteract String
    | ChangeInteractAddNewItem

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

        ChangeActsList name value ->
            let
                character =
                    model.character

                actsListNew = List.Extra.updateIf (\value -> value.name == name) (\input -> {input | value = value}) model.character.actsList
            in
                ({ model | character = { character | actsList = actsListNew } }, Cmd.none)


        ChangeKnowledgeList name value ->
            let
                character =
                    model.character

                knowledgeListNew = List.Extra.updateIf (\value -> value.name == name) (\input -> {input | value = value}) model.character.knowledgeList

            in
                ({ model | character = { character | knowledgeList = knowledgeListNew } }, Cmd.none)


        ChangeInteractList name value ->
            let
                character =
                    model.character

                interactListNew = List.Extra.updateIf (\value -> value.name == name) (\input -> {input | value = value}) model.character.interactList
            in
                ({ model | character = { character | interactList = interactListNew } }, Cmd.none)

        ChangeActsListRemoveItem name ->
            let
                character =
                    model.character

                actsListNew = List.filter (\value -> value.name /= name) model.character.actsList
            in
                ({ model | character = { character | actsList = actsListNew } }, Cmd.none)


        ChangeKnowledgeListRemoveItem name ->
            let
                character =
                    model.character

                knowledgeListNew = List.filter (\value -> value.name /= name) model.character.knowledgeList
            in
                ({ model | character = { character | knowledgeList = knowledgeListNew } }, Cmd.none)

        ChangeInteractListRemoveItem name ->
            let
                character =
                    model.character

                interactListNew = List.filter (\value -> value.name /= name) model.character.interactList
            in
                ({ model | character = { character | interactList = interactListNew } }, Cmd.none)

        ChangeActsAddNewItem ->
                let
                character =
                    model.character

                newList = List.append model.character.actsList [(ListItem model.inputNewActsItem (Just 0) False)]
            in
                ({ model | character = { character | actsList = newList } }, Cmd.none)
        
        InputNewItemActs value ->
            ({ model | inputNewActsItem = value }, Cmd.none)

        ChangeKnowledgeAddNewItem ->
                let
                character =
                    model.character

                newList = List.append model.character.knowledgeList [(ListItem model.inputNewKnowledgeItem (Just 0) False)]
            in
                ({ model | character = { character | knowledgeList = newList } }, Cmd.none)
        
        InputNewItemKnowledge value ->
            ({ model | inputNewKnowledgeItem = value }, Cmd.none)

        ChangeInteractAddNewItem ->
                let
                character =
                    model.character

                newList = List.append model.character.interactList [(ListItem model.inputNewInteractItem (Just 0) False)]
            in
                ({ model | character = { character | interactList = newList } }, Cmd.none)
        
        InputNewItemInteract value ->
            ({ model | inputNewInteractItem = value }, Cmd.none)

        ChangePage newPage ->
            ({ model | page = newPage }, Cmd.none)

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

        PageCharacterSheet ->
            renderContent pagecharacterSheet model


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
            , li [ setTabActive model PageCharacterSheet ]
                [ a []
                    [ text "Character-Blatt" ]
                ]
            ] 
            ] 
        ] 
    ]


nextButton : Model -> Maybe Page -> Maybe Page -> Html Msg
nextButton model previousPage nextPage =
    div [ class "field  is-grouped is-grouped-centered" ]
        [ div [ class "control" ]
            [ case previousPage of
                Nothing ->
                    (div [] [])

                Just value ->
                    (button [ class "button is-link", onClick (ChangePage value) ]
                    [ text "Vorherige" ])

            ],
            div [ class "control" ]
            [ case nextPage of
                Nothing ->
                    (div [] [])

                Just value ->
                    (button [ class "button is-link", onClick (ChangePage value) ]
                    [ text "Nächste" ])
            ]
        ]


addInput title placeholderText inputMessage value =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text title ]
        , div [ class "control" ]
            [ Text.input
                (Text.defaultOptions inputMessage)
                [ class "input", type_ "text", placeholder placeholderText ]
                value
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


drawActsList list =
    div [ class "column"]
         (List.map createActsNumbers list)


drawKnowledgeList list =
    div [ class "column"]
         (List.map createKnowledgeNumbers list)


drawInteractList list =
    div [ class "column"]
         (List.map createInteractNumbers list)

createActsNumbers listItem =
    div [] [
        label [ class "label" ]
            [ text listItem.name ]
        , 
    div [ class "field has-addons" ]
        [ div [ class "control" ]
        [
        Number.input
                { onInput = ChangeActsList listItem.name
                , maxLength = Nothing
                , maxValue = Just 100
                , minValue = Just 0,
                hasFocus = Just FocusChanged
                }
                [ class "input"
                ]
                listItem.value
            ]
        , if not listItem.masterItem then div [class "control"] [a [class "button is-danger", onClick (ChangeActsListRemoveItem listItem.name)] [text "X"]]
        else  div [class "control"] []
        ]
    ]

createKnowledgeNumbers listItem =
    div [] [
        label [ class "label" ]
            [ text listItem.name ]
        , 
    div [ class "field has-addons" ]
        [ div [ class "control" ]
        [
        Number.input
                { onInput = ChangeKnowledgeList listItem.name
                , maxLength = Nothing
                , maxValue = Just 100
                , minValue = Just 0,
                hasFocus = Just FocusChanged
                }
                [ class "input"
                ]
                listItem.value
            ]
        , if not listItem.masterItem then div [class "control"] [a [class "button is-danger", onClick (ChangeKnowledgeListRemoveItem listItem.name)] [text "X"]]
        else  div [class "control"] []
        ]
    ]

createInteractNumbers listItem =
    div [] [
        label [ class "label" ]
            [ text listItem.name ]
        , 
    div [ class "field has-addons" ]
        [ div [ class "control" ]
        [
        Number.input
                { onInput = ChangeInteractList listItem.name
                , maxLength = Nothing
                , maxValue = Just 100
                , minValue = Just 0,
                hasFocus = Just FocusChanged
                }
                [ class "input"
                ]
                listItem.value
            ]
        , if not listItem.masterItem then div [class "control"] [a [class "button is-danger", onClick (ChangeInteractListRemoveItem listItem.name)] [text "X"]]
        else  div [class "control"] []
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
        addInput "Vorname" "Der Vorname deines Characters" ChangeFirstname model.character.firstName
        , addInput "Name" "Der Nachname deines Characters" ChangeName model.character.name
        , addInput "Statur" "Die Statur deines Characters" ChangeStature model.character.stature
        , addInput "Religion" "Deine Religion" ChangeReligion model.character.religion
        , addInput "Geschlecht" "Mit welchem Geschlecht identifizierst du dich?" ChangeSex model.character.sex
        , addInput "Alter" "Wie alt bist du?" ChangeAge model.character.age
        , addInputNumerical model.character.lifePoints "Lebenspunkte" ChangeLifepoints
        , addInput "Beruf" "Dein Beruf" ChangeJob model.character.job
        , addInput "Familienstand" "Wie ist dein Familienstand?" ChangeFamilystatus model.character.familyStatus
        , (nextButton model Nothing (Just PageSkillpoints))
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
        drawActsList model.character.actsList
        , drawKnowledgeList model.character.knowledgeList
        , drawInteractList model.character.interactList
        ]
        , div [ class "columns"] [
        renderInputForNewItem model.inputNewActsItem InputNewItemActs ChangeActsAddNewItem
        , renderInputForNewItem model.inputNewKnowledgeItem InputNewItemKnowledge ChangeKnowledgeAddNewItem
        , renderInputForNewItem model.inputNewInteractItem InputNewItemInteract ChangeInteractAddNewItem
        ]
        , (nextButton model (Just PageBaseProperties) (Just PageCharacterSheet))
        ]


pagecharacterSheet : Model -> Html Msg
pagecharacterSheet model =
    div [ class "container" ]
        [ 
             div [ class "field" ]
        [ label [ class "label has-text-centered" ]
            [ text "Charakterbogen" ]
        ]
        , div [ class "columns"] [
            div [ class "column" ] [
                addInput "Vorname" "" ChangeFirstname model.character.firstName
                ,addInput "Geschlecht" "" ChangeSex model.character.sex
            ]
            , div [ class "column" ] [
                addInput "Name" "" ChangeSex model.character.name
                ,addInput "Alter" "" ChangeSex model.character.age
            ]
            , div [ class "column" ] [
                img [src model.character.picture] []
            ]
            , div [ class "column" ] [
                addInput "Statur" "" ChangeSex model.character.stature
                , addInput "Beruf" "" ChangeSex model.character.job
            ]
            , div [ class "column" ] [
                addInput "Religion" "" ChangeSex model.character.religion
                , addInput "Familienstand" "" ChangeSex model.character.familyStatus
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
        , (nextButton model (Just PageSkillpoints) Nothing)
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

renderInputForNewItem value inputEvent onClickEvent =
        div [class "column"] [
            div [ class "field has-addons" ]
                [ div [ class "control" ]
                    [ Text.input
                        (Text.defaultOptions inputEvent)
                        [ class "input", type_ "text", placeholder "Neuer Wert" ]
                        value
                    ]
                , div [ class "control" ] 
                [
                    button [ class "button is-info", onClick onClickEvent ] 
                    [i [ class "fa fa-plus" ] []]
                ]
            ]
        ]