module Main exposing (..)

import Html exposing (Html, br, button, div, input, text, ul)
import Html.Events exposing (onClick, onInput)
import List exposing (isEmpty, map)
import Maybe exposing (withDefault)
import Http
import Json.Decode as Decode exposing (decodeString, index)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( Model "" [] ""
    , Cmd.none
    )



-- MODEL


type alias Model =
    { text : String
    , translations : List Translation
    , error : String
    }


model : Model
model =
    { text = ""
    , translations = []
    , error = ""
    }


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
            ( model, translate model.text )

        Translated (Ok newContent) ->
            ( { model | translations = newContent }, Cmd.none )

        Translated (Err e) ->
            ( { model | error = (toString e), translations = [] }, Cmd.none )

        Reset ->
            ( { model | text = "", translations = [], error = "" }, Cmd.none )

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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput Change ] []
        , button [ onClick Translate ] [ text "Translate" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , renderTranslations model.translations
        ]


renderTranslations : List Translation -> Html Msg
renderTranslations translations =
    if isEmpty translations then
        div [] []
    else
        ul
            []
            (List.map renderTranslation translations)


renderTranslation : Translation -> Html Msg
renderTranslation translation =
    div []
        [ text
            (case translation.phrase of
                Just s ->
                    s

                Nothing ->
                    ""
            )
        , br [] []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
