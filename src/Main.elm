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


type ListItemType
    = Acts
    | Knowledge
    | Interact


type alias ListItem =
    { name : String
    , value : Int
    , itemType : ListItemType
    }


type alias Character =
    { name : String
    , firstName : String
    , stature : String
    , primaryLanguage : String
    , religion : String
    , sex : String
    , age : String
    , job : String
    , familyStatus : String
    , lifePoints : String
    , skillList : List ListItem
    , knowledge : Int
    , interact : Int
    , acts : Int
    , assignedSkillpoints : Int
    , spendableSkillpoints : Int
    , picture : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (Character ""
            ""
            ""
            ""
            ""
            ""
            ""
            ""
            ""
            "100"
            []
            0
            0
            0
            0
            500
            "img/character.jpg"
        )
        PageBaseProperties
        ""
        ""
        ""
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeName String
    | ChangeFirstname String
    | ChangeStature String
    | ChangePrimaryLanguage String
    | ChangeReligion String
    | ChangeSex String
    | ChangeAge String
    | ChangeJob String
    | ChangeFamilystatus String
    | ChangePage Page
    | ChangeSkillList String (Maybe Int)
    | ChangeSkillListRemoveItem String
    | InputNewItemActs String
    | ChangeActsAddNewItem
    | InputNewItemKnowledge String
    | ChangeKnowledgeAddNewItem
    | InputNewItemInteract String
    | ChangeInteractAddNewItem
    | NoOp String


update msg model =
    case msg of
        ChangeName name ->
            let
                character =
                    model.character
            in
                ( { model | character = { character | name = name } }, Cmd.none )

        ChangeFirstname firstName ->
            let
                character =
                    model.character
            in
                ( { model | character = { character | firstName = firstName } }, Cmd.none )

        ChangeStature stature ->
            let
                character =
                    model.character
            in
                ( { model | character = { character | stature = stature } }, Cmd.none )

        ChangePrimaryLanguage primaryLanguage ->
            let
                character =
                    model.character
            in
                ( { model | character = { character | primaryLanguage = primaryLanguage } }, Cmd.none )

        ChangeReligion religion ->
            let
                character =
                    model.character
            in
                ( { model | character = { character | religion = religion } }, Cmd.none )

        ChangeSex sex ->
            let
                character =
                    model.character
            in
                ( { model | character = { character | sex = sex } }, Cmd.none )

        ChangeAge age ->
            let
                character =
                    model.character
            in
                ( { model | character = { character | age = age } }, Cmd.none )

        ChangeJob job ->
            let
                character =
                    model.character
            in
                ( { model | character = { character | job = job } }, Cmd.none )

        ChangeFamilystatus familyStatus ->
            let
                character =
                    model.character
            in
                ( { model | character = { character | familyStatus = familyStatus } }, Cmd.none )

        ChangeSkillList name value ->
            let
                character =
                    model.character

                {--Total --}
                summedValues =
                    character.skillList
                        |> List.filter (\value -> value.name /= name)
                        |> List.map .value
                        |> List.sum

                sumAndCurrentValue =
                    summedValues + convertMaybeIntToInt value

                {--Acts --}
                actsTotal =
                    calculateTotal character Acts

                {--Knowledge --}
                knowledgeTotal =
                    calculateTotal character Knowledge

                {--Interact --}
                interactTotal =
                    calculateTotal character Interact

                skillListNew =
                    if sumAndCurrentValue <= 500 then
                        List.Extra.updateIf (\value -> value.name == name) (\input -> { input | value = convertMaybeIntToInt value }) model.character.skillList
                    else
                        model.character.skillList
            in
                ( { model | character = { character | skillList = skillListNew, assignedSkillpoints = sumAndCurrentValue, acts = actsTotal, knowledge = knowledgeTotal, interact = interactTotal } }, Cmd.none )

        ChangeSkillListRemoveItem name ->
            let
                character =
                    model.character

                skillListNew =
                    List.filter (\value -> value.name /= name) model.character.skillList
            in
                ( { model | character = { character | skillList = skillListNew } }, Cmd.none )

        ChangeActsAddNewItem ->
            let
                character =
                    model.character

                newList =
                    if String.trim model.inputNewActsItem /= "" then
                        List.append model.character.skillList [ (ListItem model.inputNewActsItem 0 Acts) ]
                    else
                        model.character.skillList

                newInputItem =
                    if String.trim model.inputNewActsItem /= "" then
                        ""
                    else
                        model.inputNewActsItem
            in
                ( { model | character = { character | skillList = newList }, inputNewActsItem = newInputItem }, Cmd.none )

        InputNewItemActs value ->
            ( { model | inputNewActsItem = value }, Cmd.none )

        ChangeKnowledgeAddNewItem ->
            let
                character =
                    model.character

                newList =
                    if String.trim model.inputNewKnowledgeItem /= "" then
                        List.append model.character.skillList [ (ListItem model.inputNewKnowledgeItem 0 Knowledge) ]
                    else
                        model.character.skillList

                newInputItem =
                    if String.trim model.inputNewKnowledgeItem /= "" then
                        ""
                    else
                        model.inputNewKnowledgeItem
            in
                ( { model | character = { character | skillList = newList }, inputNewKnowledgeItem = newInputItem }, Cmd.none )

        InputNewItemKnowledge value ->
            ( { model | inputNewKnowledgeItem = value }, Cmd.none )

        ChangeInteractAddNewItem ->
            let
                character =
                    model.character

                newList =
                    if String.trim model.inputNewInteractItem /= "" then
                        List.append model.character.skillList [ (ListItem model.inputNewInteractItem 0 Interact) ]
                    else
                        model.character.skillList

                newInputItem =
                    if String.trim model.inputNewInteractItem /= "" then
                        ""
                    else
                        model.inputNewInteractItem
            in
                ( { model | character = { character | skillList = newList }, inputNewInteractItem = newInputItem }, Cmd.none )

        InputNewItemInteract value ->
            ( { model | inputNewInteractItem = value }, Cmd.none )

        ChangePage newPage ->
            ( { model | page = newPage }, Cmd.none )

        NoOp _ ->
            ( model, Cmd.none )


calculateTotal : Character -> ListItemType -> Int
calculateTotal character listItemType =
    (calculateSkillDivided character listItemType) + (calculateSkillOutstanding character listItemType)


calculateSkillDivided : Character -> ListItemType -> Int
calculateSkillDivided character listItemType =
    (character.skillList
        |> List.filter (\value -> value.itemType == listItemType)
        |> List.map .value
        |> List.sum
    )
        // 10


calculateSkillOutstanding : Character -> ListItemType -> Int
calculateSkillOutstanding character listItemType =
    (character.skillList
        |> List.filter (\value -> value.itemType == listItemType && value.value >= 80)
        |> List.length
    )
        * 10



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        PageBaseProperties ->
            renderHeaderAndFooter pageBaseproperties model

        PageSkillpoints ->
            renderHeaderAndFooter pageSkillpoints model

        PageCharacterSheet ->
            renderHeaderAndFooter pageCharacterSheet model


pageBaseproperties : Model -> Html Msg
pageBaseproperties model =
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "column" ]
                [ addInput "Vorname" "Der Vorname deines Characters" ChangeFirstname model.character.firstName False ]
            , div [ class "column" ]
                [ addInput "Name" "Der Nachname deines Characters" ChangeName model.character.name False
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ addInput "Statur" "Die Statur deines Characters" ChangeStature model.character.stature False
                ]
            , div [ class "column" ]
                [ addInput "Muttersprache" "Die Muttersprache deines Characters" ChangePrimaryLanguage model.character.primaryLanguage False
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ addInput "Religion" "Deine Religion" ChangeReligion model.character.religion False
                ]
            , div [ class "column" ]
                [ addInput "Geschlecht" "Mit welchem Geschlecht identifizierst du dich?" ChangeSex model.character.sex False
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ addInput "Alter" "Wie alt bist du?" ChangeAge model.character.age False
                ]
            , div [ class "column" ]
                [ addInput "Beruf" "Dein Beruf" ChangeJob model.character.job False
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ addInput "Familienstand" "Wie ist dein Familienstand?" ChangeFamilystatus model.character.familyStatus False
                ]
            ]
        , (renderNextAndPreviousButtons model Nothing (Just PageSkillpoints))
        ]


remainingSkillpoints : Model -> String
remainingSkillpoints model =
    toString (model.character.spendableSkillpoints - model.character.assignedSkillpoints)


pageSkillpoints : Model -> Html Msg
pageSkillpoints model =
    div [ class "container" ]
        [ div [ class "field" ]
            [ label [ class "label has-text-centered" ]
                [ text "Verbleibende Skillpunkte" ]
            , div [ class "control" ]
                [ input [ class "input is-large has-text-centered", type_ "text", value (remainingSkillpoints model), readonly True, disabled True ]
                    []
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ showReadonlyInput model.character.acts "Handeln"
                , renderList Acts model.character.skillList
            , renderInputWithPlusButton model.inputNewActsItem InputNewItemActs ChangeActsAddNewItem "Neue Handeln Begabung"
                ]
            , div [ class "column" ]
                [ showReadonlyInput model.character.knowledge "Wissen"
                ,
             renderList Knowledge model.character.skillList
            , renderInputWithPlusButton model.inputNewKnowledgeItem InputNewItemKnowledge ChangeKnowledgeAddNewItem "Neue Wissens Begabung"
                ]
            , div [ class "column" ]
                [ showReadonlyInput model.character.interact "Interagieren"
                , 
             renderList Interact model.character.skillList
            , renderInputWithPlusButton model.inputNewInteractItem InputNewItemInteract ChangeInteractAddNewItem "Neue Interaktions Begabung"
                ]
            ]
        , (renderNextAndPreviousButtons model (Just PageBaseProperties) (Just PageCharacterSheet))
        ]


pageCharacterSheet : Model -> Html Msg
pageCharacterSheet model =
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "column" ]
                [ addInput "Vorname" "" ChangeFirstname model.character.firstName True
                , addInput "Geschlecht" "" ChangeSex model.character.sex True
                , addInput "Name" "" ChangeName model.character.name True
                , addInput "Alter" "" ChangeAge model.character.age True
                , addInput "Muttersprache" "" ChangePrimaryLanguage model.character.primaryLanguage True
                ]
            , div [ class "column" ]
                [ img [ src model.character.picture ] []
                ]
            , div [ class "column" ]
                [ addInput "Statur" "" ChangeStature model.character.stature True
                , addInput "Beruf" "" ChangeJob model.character.job True
                , addInput "Religion" "" ChangeReligion model.character.religion True
                , addInput "Familienstand" "" ChangeFamilystatus model.character.familyStatus True
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                []
            , div [ class "column" ]
                []
            , div [ class "column" ]
                [ addInput "Lebenspunkte" "" NoOp model.character.lifePoints True
                ]
            , div [ class "column" ]
                []
            , div [ class "column" ]
                []
            ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ showReadonlyInput model.character.acts "Handeln"
                , renderList Acts model.character.skillList
                ]
            , div [ class "column" ]
                [ showReadonlyInput model.character.knowledge "Wissen"
                , renderList Knowledge model.character.skillList
                ]
            , div [ class "column" ]
                [ showReadonlyInput model.character.interact "Interagieren"
                , renderList Interact model.character.skillList
                ]
            ]
        , (renderNextAndPreviousButtons model (Just PageSkillpoints) Nothing)
        ]


setTabActive : Model -> Page -> Attribute msg
setTabActive model page =
    if model.page == page then
        class "is-active"
    else
        class ""


renderTabs : Model -> Html Msg
renderTabs model =
    div [ class "hero-foot" ]
        [ nav [ class "tabs is-boxed is-fullwidth" ]
            [ div [ class "container" ]
                [ ul []
                    [ li
                        [ setTabActive model PageBaseProperties
                        ]
                        [ a []
                            [ text "Charakter Eigenschaften" ]
                        ]
                    , li [ setTabActive model PageSkillpoints ]
                        [ a []
                            [ text "Begabungspunkte" ]
                        ]
                    , li [ setTabActive model PageCharacterSheet ]
                        [ a []
                            [ text "Charakterbogen" ]
                        ]
                    ]
                ]
            ]
        ]


renderNextAndPreviousButtons : Model -> Maybe Page -> Maybe Page -> Html Msg
renderNextAndPreviousButtons model previousPage nextPage =
    div [ class "field  is-grouped is-grouped-centered" ]
        [ div [ class "control" ]
            [ case previousPage of
                Nothing ->
                    (div [] [])

                Just value ->
                    (button [ class "button is-link", onClick (ChangePage value) ]
                        [ text "Zurück" ]
                    )
            ]
        , div [ class "control" ]
            [ case nextPage of
                Nothing ->
                    (div [] [])

                Just value ->
                    (button [ class "button is-link", onClick (ChangePage value) ]
                        [ text "Weiter" ]
                    )
            ]
        ]


addInput title placeholderText inputMessage value readonlyInput =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text title ]
        , div [ class "control" ]
            [ (Text.input
                (Text.defaultOptions inputMessage)
                [ class
                    (if readonlyInput then
                        "input is-static"
                     else
                        "input"
                    )
                , type_ "text"
                , placeholder placeholderText
                , readonly readonlyInput
                ]
                value
              )
            ]
        ]


renderOrderedStaticList itemType list =
    div [ ]
        (list
            |> List.filter (\item -> item.itemType == itemType) 
            |> (List.map createListNumbers)
        )


renderList itemType list =
    div [ ]
        (list
            |> List.filter (\item -> item.itemType == itemType)
            |> (List.map createListNumbers)
        )


createListNumbers listItem =
    div []
        [ label [ class "label" ]
            [ text listItem.name ]
        , div [ class "field has-addons" ]
            [ div [ class "control is-expanded" ]
                [ Number.input
                    { onInput = ChangeSkillList listItem.name
                    , maxLength = Nothing
                    , maxValue = Just 100
                    , minValue = Just 0
                    , hasFocus = Nothing
                    }
                    [ class "input"
                    ]
                    (Just listItem.value)
                ]
            , div [ class "control" ] [ a [ class "button is-danger", onClick (ChangeSkillListRemoveItem listItem.name) ] [ text "X" ] ]
            ]
        ]


renderHeaderAndFooter page model =
    div []
        [ section [ class "hero is-primary" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ h1 [ class "title" ]
                        [ text "How to be a hero" ]
                    , h2 [ class "subtitle" ]
                        [ text "Characterblatt-Ersteller" ]
                    ]
                ]
            , renderTabs model
            ]
        , page model
        , footer [ class "footer" ]
            [ div [ class "container" ]
                [ div [ class "content has-text-centered" ]
                    [ p []
                        [ text "How to be a hero character sheet creator by Kolja Lampe. The source code is licensed MIT."
                        ]
                    ]
                ]
            ]
        ]


showReadonlyInput : Int -> String -> Html Msg
showReadonlyInput value title =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text title ]
        , div [ class "control" ]
            [ Text.input
                (Text.defaultOptions NoOp)
                [ class "input", type_ "text", readonly True, disabled True ]
                (toString value)
            ]
        ]


convertMaybeIntToInt : Maybe number -> number
convertMaybeIntToInt input =
    case input of
        Nothing ->
            0

        Just value ->
            value


renderInputWithPlusButton : String -> (String -> msg) -> msg -> String -> Html msg
renderInputWithPlusButton value inputEvent onClickEvent placeholderString =
    div [ ]
        [ div [ class "field has-addons" ]
            [ div [ class "control is-expanded" ]
                [ Text.input
                    (Text.defaultOptions inputEvent)
                    [ class "input", type_ "text", placeholder placeholderString ]
                    value
                ]
            , div [ class "control" ]
                [ button [ class "button is-info", onClick onClickEvent ]
                    [ i [ class "fa fa-plus" ] [] ]
                ]
            ]
        ]
