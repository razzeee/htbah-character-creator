module Main exposing (Character, ListItem, ListItemType(..), Model, Msg(..), Page(..), StepColors(..), StepMark(..), StringFieldType(..), addInput, calculateTotal, checkForDuplicate, checkIfValidNewItem, checkInputForErrorAndGenerateString, colorSteps, convertMaybeIntToInt, createListNumbers, getStepMarkHtml, init, leadUpToCurrentPageColors, leadsUpToCurrentPage, main, pageAdditionalProperties, pageBaseProperties, pageCharacterSheet, pageSkillpoints, remainingSkillpoints, renderHeaderAndFooter, renderHeaderAndPoints, renderInputWithPlusButton, renderList, renderNextAndPreviousButtons, renderOrderedStaticList, renderStepsOverview, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Input.Number as Number
import Input.Text as Text
import List.Extra exposing (..)


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = always Sub.none }



-- MODEL


type alias Model =
    { character : Character
    , page : Page
    , inputNewActsItem : String
    , inputNewActsItemError : String
    , inputNewKnowledgeItem : String
    , inputNewKnowledgeItemError : String
    , inputNewSocialItem : String
    , inputNewSocialItemError : String
    }


type Page
    = PageBaseProperties
    | PageAdditionalProperties
    | PageSkillpoints
    | PageCharacterSheet


type StepMark
    = Checkmark
    | Final
    | Blank


type StepColors
    = Active
    | Completed
    | None


type ListItemType
    = Acts
    | Knowledge
    | Social


type alias ListItem =
    { name : String
    , value : Int
    , calculatedValue : Int
    , itemType : ListItemType
    }


type StringFieldType
    = Name
    | FirstName
    | Stature
    | Religion
    | Sex
    | Age
    | Job
    | FamilyStatus
    | NoOp


type alias Character =
    { name : String
    , firstName : String
    , stature : String
    , religion : String
    , sex : String
    , age : String
    , job : String
    , familyStatus : String
    , lifePoints : String
    , skillList : List ListItem
    , knowledge : Int
    , knowledgeGBP : Int
    , social : Int
    , socialGBP : Int
    , acts : Int
    , actsGBP : Int
    , assignedSkillpoints : Int
    , spendableSkillpoints : Int
    , picture : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Character ""
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
            0
            0
            0
            400
            "img/character.jpg"
        )
        PageBaseProperties
        ""
        ""
        ""
        ""
        ""
        ""
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeStringField StringFieldType String
    | ChangePage Page
    | ChangeValueInList String (Maybe Int)
    | RemoveItemFromList String
    | ChangeInputOnNewItem ListItemType String
    | AddNewItemToList String ListItemType


update msg model =
    case msg of
        ChangeStringField fieldType value ->
            let
                character =
                    model.character
            in
            case fieldType of
                Name ->
                    ( { model | character = { character | name = value } }, Cmd.none )

                FirstName ->
                    ( { model | character = { character | firstName = value } }, Cmd.none )

                Stature ->
                    ( { model | character = { character | stature = value } }, Cmd.none )

                Religion ->
                    ( { model | character = { character | religion = value } }, Cmd.none )

                Sex ->
                    ( { model | character = { character | sex = value } }, Cmd.none )

                Age ->
                    ( { model | character = { character | age = value } }, Cmd.none )

                Job ->
                    ( { model | character = { character | job = value } }, Cmd.none )

                FamilyStatus ->
                    ( { model | character = { character | familyStatus = value } }, Cmd.none )

                NoOp ->
                    ( model, Cmd.none )

        ChangePage newPage ->
            ( { model | page = newPage }, Cmd.none )

        ChangeValueInList name value ->
            let
                character =
                    model.character

                {--Total --}
                summedValues =
                    character.skillList
                        |> List.filter (\value2 -> value2.name /= name)
                        |> List.map .value
                        |> List.sum

                sumAndCurrentValue =
                    summedValues + convertMaybeIntToInt value

                {--Acts --}
                actsTotal =
                    calculateTotal character Acts

                actsGBP =
                    round (toFloat actsTotal / 10)

                {--Knowledge --}
                knowledgeTotal =
                    calculateTotal character Knowledge

                knowledgeGBP =
                    round (toFloat knowledgeTotal / 10)

                {--Social --}
                socialTotal =
                    calculateTotal character Social

                socialGBP =
                    round (toFloat socialTotal / 10)

                calcTotal value4 =
                    if value4.value == 0 then
                        0
                    else 
                        case value4.itemType of
                            Acts ->
                                actsTotal

                            Knowledge ->
                                knowledgeTotal

                            Social ->
                                socialTotal

                skillListNew =
                    if sumAndCurrentValue <= 400 then
                        List.Extra.updateIf (\value3 -> value3.name == name) (\input -> { input | value = convertMaybeIntToInt value }) model.character.skillList

                    else
                        model.character.skillList

                updatedSkillList =
                    List.Extra.updateIf (\value3 -> True) (\input -> { input | calculatedValue = input.value + calcTotal input }) skillListNew
            in
            ( { model
                | character =
                    { character
                        | skillList = updatedSkillList
                        , assignedSkillpoints = sumAndCurrentValue
                        , acts = actsTotal
                        , actsGBP = actsGBP
                        , knowledge = knowledgeTotal
                        , knowledgeGBP = knowledgeGBP
                        , social = socialTotal
                        , socialGBP = socialGBP
                    }
              }
            , Cmd.none
            )

        RemoveItemFromList name ->
            let
                character =
                    model.character

                skillListNew =
                    List.filter (\value -> value.name /= name) model.character.skillList
            in
            ( { model | character = { character | skillList = skillListNew } }, Cmd.none )

        ChangeInputOnNewItem itemType value ->
            case itemType of
                Acts ->
                    ( { model | inputNewActsItem = value }, Cmd.none )

                Knowledge ->
                    ( { model | inputNewKnowledgeItem = value }, Cmd.none )

                Social ->
                    ( { model | inputNewSocialItem = value }, Cmd.none )

        AddNewItemToList newValue itemType ->
            let
                character =
                    model.character

                newList =
                    if checkIfValidNewItem newValue model.character.skillList then
                        List.append model.character.skillList [ ListItem newValue 0 0 itemType ]

                    else
                        model.character.skillList

                newInputItem =
                    if checkIfValidNewItem newValue model.character.skillList then
                        ""

                    else
                        newValue

                newInputItemError =
                    checkInputForErrorAndGenerateString newValue model.character.skillList
            in
            case itemType of
                Acts ->
                    ( { model | character = { character | skillList = newList }, inputNewActsItem = newInputItem, inputNewActsItemError = newInputItemError }, Cmd.none )

                Knowledge ->
                    ( { model | character = { character | skillList = newList }, inputNewKnowledgeItem = newInputItem, inputNewKnowledgeItemError = newInputItemError }, Cmd.none )

                Social ->
                    ( { model | character = { character | skillList = newList }, inputNewSocialItem = newInputItem, inputNewSocialItemError = newInputItemError }, Cmd.none )


calculateTotal : Character -> ListItemType -> Int
calculateTotal character listItemType =
    round
        ((character.skillList
            |> List.filter (\value -> value.itemType == listItemType)
            |> List.map .value
            |> List.sum
            |> toFloat
         )
            / 10
        )


checkForDuplicate : List ListItem -> String -> Bool
checkForDuplicate list name =
    list
        |> List.map .name
        |> List.member name


checkIfValidNewItem : String -> List ListItem -> Bool
checkIfValidNewItem value list =
    String.trim value /= "" && not (checkForDuplicate list value)


checkInputForErrorAndGenerateString : String -> List ListItem -> String
checkInputForErrorAndGenerateString value list =
    if String.trim value == "" then
        "Bitte geb einen Namen für die Begabung ein."

    else if checkForDuplicate list value then
        "Begabung mit dem Namen bereits vorhanden."

    else
        ""



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        PageBaseProperties ->
            renderHeaderAndFooter pageBaseProperties model

        PageAdditionalProperties ->
            renderHeaderAndFooter pageAdditionalProperties model

        PageSkillpoints ->
            renderHeaderAndFooter pageSkillpoints model

        PageCharacterSheet ->
            renderHeaderAndFooter pageCharacterSheet model


pageBaseProperties : Model -> Html Msg
pageBaseProperties model =
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "column" ]
                [ addInput "Vorname" "Der Vorname deines Characters." FirstName model.character.firstName False
                , addInput "Name" "Der Nachname deines Characters." Name model.character.name False
                , addInput "Alter" "Bist du schon ein Greiß oder eher ein junger Hüpfer?" Age model.character.age False
                , addInput "Geschlecht" "Welchem Geschlecht gehört dein Charakter an?" Sex model.character.sex False
                ]
            ]
        , renderNextAndPreviousButtons Nothing (Just PageAdditionalProperties)
        ]


pageAdditionalProperties : Model -> Html Msg
pageAdditionalProperties model =
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "column" ]
                [ addInput "Beruf" "Dein Beruf, z.B. Busfahrer oder Drachenjäger." Job model.character.job False
                , addInput "Statur" "Die Statur deines Characters, sollte zum Beruf passen." Stature model.character.stature False
                , addInput "Religion" "Deine Religion." Religion model.character.religion False
                , addInput "Familienstand" "Wie ist dein Familienstand?" FamilyStatus model.character.familyStatus False
                ]
            ]
        , renderNextAndPreviousButtons (Just PageBaseProperties) (Just PageSkillpoints)
        ]


remainingSkillpoints : Model -> String
remainingSkillpoints model =
    String.fromInt (model.character.spendableSkillpoints - model.character.assignedSkillpoints)


pageSkillpoints : Model -> Html Msg
pageSkillpoints model =
    div [ class "container" ]
        [ div [ class "field" ]
            [ label [ class "label has-text-centered" ]
                [ text "Verbleibende Skillpunkte" ]
            , div [ class "control" ]
                [ input [ class "input is-large has-text-centered is-primary", type_ "text", value (remainingSkillpoints model), readonly True, disabled True ]
                    []
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ div [ class "card" ]
                    [ renderHeaderAndPoints model.character.acts model.character.actsGBP "Handeln"
                    , div [ class "card-content" ]
                        [ renderList Acts model.character.skillList
                        , renderInputWithPlusButton model.inputNewActsItem model.inputNewActsItemError Acts (AddNewItemToList model.inputNewActsItem Acts) "Neue Handeln-Begabung hinzufügen"
                        ]
                    ]
                ]
            , div [ class "column" ]
                [ div [ class "card" ]
                    [ renderHeaderAndPoints model.character.knowledge model.character.knowledgeGBP "Wissen"
                    , div [ class "card-content" ]
                        [ renderList Knowledge model.character.skillList
                        , renderInputWithPlusButton model.inputNewKnowledgeItem model.inputNewKnowledgeItemError Knowledge (AddNewItemToList model.inputNewKnowledgeItem Knowledge) "Neue Wissens-Begabung hinzufügen"
                        ]
                    ]
                ]
            , div [ class "column" ]
                [ div [ class "card" ]
                    [ renderHeaderAndPoints model.character.social model.character.socialGBP "Soziales"
                    , div [ class "card-content" ]
                        [ renderList Social model.character.skillList
                        , renderInputWithPlusButton model.inputNewSocialItem model.inputNewSocialItemError Social (AddNewItemToList model.inputNewSocialItem Social) "Neue Soziales-Begabung hinzufügen"
                        ]
                    ]
                ]
            ]
        , renderNextAndPreviousButtons (Just PageAdditionalProperties) (Just PageCharacterSheet)
        ]


pageCharacterSheet : Model -> Html Msg
pageCharacterSheet model =
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "column" ]
                [ addInput "Vorname" "" FirstName model.character.firstName True
                , addInput "Geschlecht" "" Sex model.character.sex True
                , addInput "Alter" "" Age model.character.age True
                , addInput "Statur" "" Stature model.character.stature True
                ]
            , div [ class "column" ]
                [ figure [ class "image is-square" ]
                    [ img [ src model.character.picture ] []
                    ]
                , addInput "Lebenspunkte" "" NoOp model.character.lifePoints True
                ]
            , div [ class "column" ]
                [ addInput "Name" "" Name model.character.name True
                , addInput "Beruf" "" Job model.character.job True
                , addInput "Religion" "" Religion model.character.religion True
                , addInput "Familienstand" "" FamilyStatus model.character.familyStatus True
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ div [ class "card" ]
                    [ renderHeaderAndPoints model.character.acts model.character.actsGBP "Handeln"
                    , div [ class "card-content" ]
                        [ renderOrderedStaticList Acts model.character
                        ]
                    ]
                ]
            , div [ class "column" ]
                [ div [ class "card" ]
                    [ renderHeaderAndPoints model.character.knowledge model.character.knowledgeGBP "Wissen"
                    , div [ class "card-content" ]
                        [ renderOrderedStaticList Knowledge model.character
                        ]
                    ]
                ]
            , div [ class "column" ]
                [ div [ class "card" ]
                    [ renderHeaderAndPoints model.character.social model.character.socialGBP "Soziales"
                    , div [ class "card-content" ]
                        [ renderOrderedStaticList Social model.character
                        ]
                    ]
                ]
            ]
        , renderNextAndPreviousButtons (Just PageSkillpoints) Nothing
        ]


leadsUpToCurrentPage : Page -> Page -> StepMark
leadsUpToCurrentPage page currentPage =
    case currentPage of
        PageAdditionalProperties ->
            if page == PageBaseProperties then
                Checkmark

            else if page == PageCharacterSheet then
                Final

            else
                Blank

        PageSkillpoints ->
            if page == PageBaseProperties || page == PageAdditionalProperties then
                Checkmark

            else if page == PageCharacterSheet then
                Final

            else
                Blank

        PageCharacterSheet ->
            if page == PageBaseProperties || page == PageAdditionalProperties || page == PageSkillpoints then
                Checkmark

            else if page == PageCharacterSheet then
                Final

            else
                Blank

        _ ->
            Blank


getStepMarkHtml : StepMark -> Html msg
getStepMarkHtml stepmark =
    case stepmark of
        Checkmark ->
            Html.span [ class "icon" ]
                [ i [ class "fa fa-check" ]
                    []
                ]

        Final ->
            Html.span [ class "icon" ]
                [ i [ class "fa fa-flag" ]
                    []
                ]

        Blank ->
            text ""


leadUpToCurrentPageColors : Page -> Page -> StepColors
leadUpToCurrentPageColors page currentPage =
    case currentPage of
        PageBaseProperties ->
            if page == PageBaseProperties then
                Active

            else
                None

        PageAdditionalProperties ->
            if page == PageAdditionalProperties then
                Active

            else if page == PageBaseProperties then
                Completed

            else
                None

        PageSkillpoints ->
            if page == PageSkillpoints then
                Active

            else if page == PageBaseProperties || page == PageAdditionalProperties then
                Completed

            else
                None

        PageCharacterSheet ->
            if page == PageCharacterSheet then
                Active

            else if page == PageBaseProperties || page == PageAdditionalProperties || page == PageSkillpoints then
                Completed

            else
                None


colorSteps : StepColors -> String
colorSteps stepcolor =
    if stepcolor == Completed then
        "step-item is-completed is-primary"

    else if stepcolor == Active then
        "step-item is-active"

    else
        "step-item"


renderStepsOverview : Model -> Html Msg
renderStepsOverview model =
    div [ class "steps no-print hide-mobile" ]
        [ div [ class (leadUpToCurrentPageColors PageBaseProperties model.page |> colorSteps) ]
            [ div [ class "step-marker" ]
                [ leadsUpToCurrentPage PageBaseProperties model.page |> getStepMarkHtml ]
            , div [ class "step-details" ]
                [ p [ class "step-title" ]
                    [ text "Wer bist du?" ]
                , p []
                    [ text "Ein paar grundlegende Infos über deinen Charakter." ]
                ]
            ]
        , div [ class (leadUpToCurrentPageColors PageAdditionalProperties model.page |> colorSteps) ]
            [ div [ class "step-marker" ]
                [ leadsUpToCurrentPage PageAdditionalProperties model.page |> getStepMarkHtml ]
            , div [ class "step-details" ]
                [ p [ class "step-title" ]
                    [ text "Was machst du?" ]
                , p []
                    [ text "Was ist dein Beruf, wie ist deine Religion? Erstelle deine Hintergrundgeschichte." ]
                ]
            ]
        , div [ class (leadUpToCurrentPageColors PageSkillpoints model.page |> colorSteps) ]
            [ div [ class "step-marker" ]
                [ leadsUpToCurrentPage PageSkillpoints model.page |> getStepMarkHtml ]
            , div [ class "step-details" ]
                [ p [ class "step-title" ]
                    [ text "Begabungspunkte" ]
                , p []
                    [ text "Es ist soweit, hier kannst du deine Begabungspunkte verteilen!" ]
                ]
            ]
        , div [ class (leadUpToCurrentPageColors PageCharacterSheet model.page |> colorSteps) ]
            [ div [ class "step-marker" ]
                [ leadsUpToCurrentPage PageCharacterSheet model.page |> getStepMarkHtml ]
            , div [ class "step-details" ]
                [ p [ class "step-title" ]
                    [ text "Charakterbogen" ]
                , p []
                    [ text "Dein fertiger Charakterbogen, zum drucken." ]
                ]
            ]
        ]


renderNextAndPreviousButtons : Maybe Page -> Maybe Page -> Html Msg
renderNextAndPreviousButtons previousPage nextPage =
    div [ class "field  is-grouped is-grouped-centered no-print" ]
        [ div [ class "control" ]
            [ case previousPage of
                Nothing ->
                    div [] []

                Just value ->
                    button [ class "button is-primary", onClick (ChangePage value) ]
                        [ text "Zurück" ]
            ]
        , div [ class "control" ]
            [ case nextPage of
                Nothing ->
                    div [] []

                Just value ->
                    button [ class "button is-primary", onClick (ChangePage value) ]
                        [ text "Weiter" ]
            ]
        ]


addInput : String -> String -> StringFieldType -> String -> Bool -> Html Msg
addInput title placeholderText fieldType value readonlyInput =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text title ]
        , div [ class "control" ]
            [ Text.input
                (Text.defaultOptions (ChangeStringField fieldType))
                [ class "input"
                , type_ "text"
                , placeholder placeholderText
                , readonly readonlyInput
                , disabled readonlyInput
                ]
                value
            ]
        ]


renderOrderedStaticList : ListItemType -> Character -> Html Msg
renderOrderedStaticList itemType character =
    let
        typeBonus =
            case itemType of
                Acts ->
                    character.acts

                Knowledge ->
                    character.knowledge

                Social ->
                    character.social
    in
    div []
        (character.skillList
            |> List.filter (\item -> item.itemType == itemType)
            |> List.sortBy .value
            |> List.reverse
            |> List.map (\input -> addInput input.name "" NoOp (String.fromInt (clamp 0 100 (input.value + typeBonus))) True)
        )


renderList : ListItemType -> List ListItem -> Html Msg
renderList itemType list =
    div []
        (list
            |> List.filter (\item -> item.itemType == itemType)
            |> List.map createListNumbers
        )


createListNumbers : ListItem -> Html Msg
createListNumbers listItem =
    div []
        [ label [ class "label" ]
            [ text listItem.name ]
        , div [ class "field has-addons" ]
            [ div [ class "control is-expanded" ]
                [ Number.input
                    { onInput = ChangeValueInList listItem.name
                    , maxLength = Nothing
                    , maxValue = Just 100
                    , minValue = Just 0
                    , hasFocus = Nothing
                    }
                    [ class "input"
                    ]
                    (Just listItem.value)
                ]
            , div [ class "control" ]
                [ Number.input
                    { onInput = ChangeValueInList listItem.name
                    , maxLength = Nothing
                    , maxValue = Just 100
                    , minValue = Just 0
                    , hasFocus = Nothing
                    }
                    [ class "input"
                    , readonly True
                    , disabled True
                    ]
                    (Just listItem.calculatedValue)
                ]
            , div [ class "control" ] [ button [ class "button is-danger", onClick (RemoveItemFromList listItem.name) ] [ i [ class "fa fa-trash" ] [] ] ]
            ]
        ]


renderHeaderAndFooter : (Model -> Html Msg) -> Model -> Html Msg
renderHeaderAndFooter page model =
    div []
        [ section [ class "hero is-primary no-print" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ figure [ class "image is-128x128" ]
                        [ img [ src "img/htbah.png" ] []
                        ]
                    , h2 [ class "subtitle" ]
                        [ text "Characterbogen-Ersteller" ]
                    ]
                ]
            ]
        , section [ class "section" ] [ renderStepsOverview model, page model ]
        , footer [ class "footer no-print" ]
            [ div [ class "container" ]
                [ div [ class "content has-text-centered" ]
                    [ p []
                        [ text "How to be a hero character sheet creator done with "
                        , i [ class "fa fa-heart" ] []
                        , text " by Kolja Lampe. The source code is licensed MIT."
                        ]
                    , p []
                        [ text "You can find it on "
                        , a [ href "https://github.com/Razzeee/htbah-character-creator/" ] [ text "github" ]
                        , text "."
                        ]
                    ]
                ]
            ]
        ]


renderHeaderAndPoints : Int -> Int -> String -> Html Msg
renderHeaderAndPoints value gbpValue title =
    header [ class "card-header" ]
        [ p [ class "card-header-title is-centered" ]
            [ div [ class "control" ]
                [ div [ class "media-content" ]
                    [ p
                        [ class "title is-4" ]
                        [ div
                            [ class "tags has-addons" ]
                            [ Html.span [ class "tag is-large" ]
                                [ text title ]
                            , Html.span [ class "tag is-primary is-large" ]
                                [ text (String.fromInt value)
                                ]
                            ]
                        ]
                    , p [ class "subtitle is-6" ]
                        [ div [ class "tags has-addons" ]
                            [ Html.span [ class "tag" ]
                                [ text "Geistesblitzpunkte" ]
                            , Html.span [ class "tag is-primary" ]
                                [ text (String.fromInt gbpValue)
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , p [ class "card-header-icon is-centered no-print" ]
            [ Html.span [ class "icon tooltip is-tooltip-multiline no-print", attribute "data-tooltip" "Für alle 10 ausgegebenen Punkte, erhält die Kategorie in der du diese ausgibst einen Punkt." ]
                [ i [ class "fa fa-question-circle  no-print" ]
                    []
                ]
            ]
        ]


convertMaybeIntToInt : Maybe number -> number
convertMaybeIntToInt input =
    case input of
        Nothing ->
            0

        Just value ->
            value


renderInputWithPlusButton : String -> String -> ListItemType -> Msg -> String -> Html Msg
renderInputWithPlusButton value error itemType onClickEvent placeholderString =
    div []
        [ label [ class "label" ]
            []
        , div [ class "field has-addons" ]
            [ div [ class "control is-expanded" ]
                [ Text.input
                    (Text.defaultOptions (ChangeInputOnNewItem itemType))
                    [ class "input", type_ "text", placeholder placeholderString ]
                    value
                ]
            , div [ class "control" ]
                [ button [ class "button is-primary", onClick onClickEvent ]
                    [ i [ class "fa fa-plus" ] [] ]
                ]
            ]
        , if error /= "" then
            div [ class "notification is-danger" ]
                [ text error
                ]

          else
            div [] []
        ]
