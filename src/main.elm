module Main exposing (Meaning, Model, Msg(..), Translation, decodeMeaning, decodeTranslation, decodeTranslations, init, main, renderTranslation, renderTranslations, request, subscriptions, translate, update, view)

import Browser
import Browser.Dom as Dom
import Debug exposing (log)
import Html exposing (Attribute, Html, br, button, div, form, input, label, li, p, section, span, text, ul)
import Html.Attributes exposing (autocomplete, autofocus, class, for, id, placeholder, property, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Http
import Json.Decode as Decode exposing (decodeString, index)
import Json.Encode exposing (string)
import List exposing (isEmpty, map)
import Maybe exposing (withDefault)
import Task


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" NotEntered
    , Cmd.none
    )



-- MODEL


type alias Model =
    { text : String
    , data : RemoteData
    }


type RemoteData
    = NotEntered
    | Loading
    | Empty
    | Data (List Translation)
    | Error String


type alias Translation =
    { phrase : Maybe String
    , meanings : Maybe (List Meaning)
    , isExtended : Bool
    }


type alias Meaning =
    { language : Maybe Language
    , text : String
    }


type Language
    = NL
    | EN



-- UPDATE


type Msg
    = Change String
    | Translate
    | Translated (Result Http.Error (List Translation))
    | Reset
    | Extend Translation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Translate ->
            ( { model | data = Loading }, translate model.text )

        Translated (Ok newContent) ->
            case newContent of
                [] ->
                    ( { model | data = Empty }, Cmd.none )

                _ ->
                    ( { model | data = Data newContent }, Cmd.none )

        Translated (Err e) ->
            ( { model | data = Error "" }, Cmd.none )

        Reset ->
            ( { model | text = "", data = NotEntered }, Cmd.none )

        Change newContent ->
            ( { model | text = newContent }, Cmd.none )

        Extend translation ->
            case model.data of
                Data translations ->
                    let
                        mapper tr =
                            if tr == translation then
                                { tr | isExtended = True }

                            else
                                tr
                    in
                    ( { model | data = Data (List.map mapper translations) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


translate : String -> Cmd Msg
translate text =
    let
        lowercaseText =
            String.toLower text

        url =
            "https://cors-anywhere.herokuapp.com/https://glosbe.com/gapi/translate?from=nl&dest=eng&format=json&pretty=true&phrase=" ++ lowercaseText
    in
    Http.send Translated (request url)


decodeTranslations : Decode.Decoder (List Translation)
decodeTranslations =
    Decode.field "tuc" (Decode.list decodeTranslation)


decodeTranslation : Decode.Decoder Translation
decodeTranslation =
    Decode.map3 Translation
        (Decode.maybe
            (Decode.at [ "phrase", "text" ] Decode.string)
        )
        (Decode.maybe
            (Decode.field "meanings" (Decode.list decodeMeaning))
        )
        (Decode.succeed False)


decodeMeaning : Decode.Decoder Meaning
decodeMeaning =
    Decode.map2 Meaning
        (Decode.field "language" decodeLanguage)
        (Decode.field "text" Decode.string)


decodeLanguage : Decode.Decoder (Maybe Language)
decodeLanguage =
    Decode.string
        |> Decode.andThen stringToLanguage


stringToLanguage : String -> Decode.Decoder (Maybe Language)
stringToLanguage str =
    case str of
        "nl" ->
            Decode.succeed (Just NL)

        "en" ->
            Decode.succeed (Just EN)

        _ ->
            Decode.succeed Nothing


request : String -> Http.Request (List Translation)
request url =
    Http.get url decodeTranslations



-- VIEWs


view : Model -> Html Msg
view model =
    div
        [ class "section"
        ]
        [ div
            [ class "container level" ]
            [ div
                [ class "container" ]
                [ div
                    [ class "field" ]
                    [ label
                        [ class "label" ]
                        [ text "Dutch" ]
                    , div
                        [ class "control" ]
                        [ input
                            [ class "input is-medium"
                            , placeholder "Please enter text to translate"
                            , onInput Change
                            , onEnter Translate
                            , value model.text
                            , type_ "text"
                            , autofocus True
                            , autocomplete True
                            ]
                            []
                        ]
                    ]
                , div
                    [ class "buttons" ]
                    [ button
                        [ onClick Translate
                        , type_ "submit"
                        , class "button is-primary"
                        ]
                        [ text "Translate" ]
                    , button
                        [ onClick Reset
                        , type_ "reset"
                        , class "button is-outlined is-danger"
                        ]
                        [ text "Reset" ]
                    ]
                ]
            ]
        , renderTranslations model.data
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "not ENTER"
    in
    on "keydown" (Decode.andThen isEnter keyCode)


renderTranslations : RemoteData -> Html Msg
renderTranslations translations =
    case log "state" translations of
        NotEntered ->
            div [ class "container notification is-fluid" ] [ text "Please, enter something" ]

        Loading ->
            renderLoading

        Empty ->
            div [ class "notification is-warning" ] [ text "No results found :(" ]

        Data translated ->
            div [ class "container" ] (List.map renderTranslation translated)

        Error error ->
            div [ class "notification is-warning" ] [ text ("Some Error! " ++ error) ]


renderLoading : Html Msg
renderLoading =
    div
        [ class "container level-item Columns" ]
        [ div [ class "control is-large is-loading is-grey-darker" ] [] ]


renderTranslation : Translation -> Html Msg
renderTranslation translation =
    div
        [ class "container  box" ]
        [ div [ class "" ]
            [ div
                [ class "" ]
                [ text (withDefault "" translation.phrase) ]
            ]
        , renderMeanings translation
        ]


renderMeanings : Translation -> Html Msg
renderMeanings translation =
    case translation.meanings of
        Just m ->
            if translation.isExtended then
                div [ class "" ] (List.map renderMeaning m)

            else
                div [ class "level-right" ]
                    [ button
                        [ onClick <| Extend translation
                        , type_ "submit"
                        , class "button is-primary level-item"
                        ]
                        [ text "Meanings" ]
                    ]

        Nothing ->
            div [] []


renderMeaning : Meaning -> Html Msg
renderMeaning meaning =
    div
        [ class "tile is-child box" ]
        [ p
            [ class ("notification " ++ meaningStyle meaning.language) ]
            [ text <| "Language: " ++ languageToString meaning.language
            , br [] []
            , postBody meaning.text
            ]
        ]


meaningStyle : Maybe Language -> String
meaningStyle l =
    case l of
        Just NL ->
            "is-primary"

        Just EN ->
            "is-info"

        _ ->
            ""


languageToString : Maybe Language -> String
languageToString l =
    case l of
        Just NL ->
            "NL"

        Just EN ->
            "EN"

        _ ->
            ""


postBody : String -> Html msg
postBody html =
    Html.node "rendered-html"
        [ property "content" <| string html ]
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
