module Main exposing (Meaning, Model, Msg(..), Translation, decodeMeaning, decodeTranslation, decodeTranslations, init, main, renderTranslation, renderTranslations, request, subscriptions, translate, update, view)

import Browser
import Browser.Dom as Dom
import Debug exposing (log)
import Html exposing (Attribute, Html, br, button, div, form, input, label, li, p, span, text, ul)
import Html.Attributes exposing (autocomplete, autofocus, class, for, id, style, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Http
import Json.Decode as Decode exposing (decodeString, index)
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
    }


type alias Meaning =
    { language : String
    , text : String
    }



-- UPDATE


type Msg
    = Change String
    | Translate
    | Translated (Result Http.Error (List Translation))
    | Reset


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


translate : String -> Cmd Msg
translate text =
    let
        url =
            "https://cors-anywhere.herokuapp.com/https://glosbe.com/gapi/translate?from=nl&dest=eng&format=json&pretty=true&phrase=" ++ text
    in
    Http.send Translated (request url)


decodeTranslations : Decode.Decoder (List Translation)
decodeTranslations =
    Decode.field "tuc" (Decode.list decodeTranslation)


decodeTranslation : Decode.Decoder Translation
decodeTranslation =
    Decode.map2 Translation
        (Decode.maybe
            (Decode.at [ "phrase", "text" ] Decode.string)
        )
        (Decode.maybe
            (Decode.field "meanings" (Decode.list decodeMeaning))
        )


decodeMeaning : Decode.Decoder Meaning
decodeMeaning =
    Decode.map2 Meaning
        (Decode.field "language" Decode.string)
        (Decode.field "text" Decode.string)


request : String -> Http.Request (List Translation)
request url =
    Http.get url decodeTranslations



-- VIEWs


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "card blue-grey darken-1" ]
            [ div [ class "card-content white-text" ]
                [ div
                    [ class "input-field" ]
                    [ input
                        [ onInput Change
                        , onEnter Translate
                        , value model.text
                        , class "validate"
                        , id "translation_input"
                        , type_ "text"
                        , autofocus True
                        , autocomplete True
                        ]
                        []
                    , label
                        [ for "translation_input" ]
                        [ text "Dutch" ]
                    ]
                , div
                    []
                    [ button
                        [ onClick Translate
                        , class "waves-effect waves-light btn"
                        , type_ "submit"
                        ]
                        [ text "Translate" ]
                    , button
                        [ onClick Reset
                        , class "waves-effect waves-light btn"
                        , type_ "reset"
                        ]
                        [ text "Reset" ]
                    ]
                , renderTranslations model.data
                ]
            ]
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
            div [] [ text "Please, enter something" ]

        Loading ->
            renderLinearLoading

        Empty ->
            div [] [ text "No results found :(" ]

        Data translated ->
            ul [ class "collection" ] (List.map renderTranslation translated)

        Error error ->
            div [] [ text ("Some Error! " ++ error) ]


renderCircularLoading : Html Msg
renderCircularLoading =
    div
        [ class "preloader-wrapper big active" ]
        [ div
            [ class "spinner-layer spinner-blue" ]
            [ div
                [ class "circle-clipper left" ]
                [ div [ class "circle" ] [] ]
            , div
                [ class "gap-patch" ]
                [ div [ class "circle" ] [] ]
            , div
                [ class "circle-clipper right" ]
                [ div [ class "circle" ] [] ]
            ]
        ]


renderLinearLoading : Html Msg
renderLinearLoading =
    div
        [ class "progress" ]
        [ div [ class "indeterminate" ] [] ]


renderTranslation : Translation -> Html Msg
renderTranslation translation =
    li
        [ class "collection-item" ]
        [ span
            [ class "blue-text text-darken-2" ]
            [ text (withDefault "" translation.phrase)
            , renderMeanings translation.meanings
            ]
        ]


renderMeanings : Maybe (List Meaning) -> Html Msg
renderMeanings meanings =
    case meanings of
        Just m ->
            ul [ class "collection" ] (List.map renderMeaning m)

        Nothing ->
            div [] []


renderMeaning : Meaning -> Html Msg
renderMeaning meaning =
    li
        [ class "collection-item" ]
        [ span
            [ class "blue-text text-darken-2" ]
            [ text meaning.text
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
