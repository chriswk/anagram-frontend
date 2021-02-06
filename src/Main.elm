module Main exposing (..)

import Browser as B
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Json


type alias PangramRequest =
    { letters : String
    , mustContain : Char
    , language : String
    }


type alias Model =
    { pangramAnswers : List String
    , letters : String
    , mustContain : Char
    , language : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pangramAnswers = []
      , letters = ""
      , mustContain = 'c'
      , language = "us"
      }
    , Cmd.none
    )

type Msg
    = NoOp
    | FetchPangrams
    | SetLetters String
    | SetMustContain String
    | SetLanguage String


pangramForm : Model -> Html Msg
pangramForm model =
    form []
        [ label []
            [ text "Letters"
            , input [ A.type_ "text", A.placeholder "Letters", E.onInput SetLetters ] []
            ]
        , label []
            [ text "Must contain"
            , input [ A.type_ "text", A.placeholder "Must contain", E.onInput SetMustContain ] []
            ]
        , label []
            [ text "Language"
            , select [ E.on "change" (Json.map SetLanguage E.targetValue) ]
                [ option [ A.value "us", A.selected True ] [ text "American" ]
                , option [ A.value "no" ] [ text "Norwegian" ]
                , option [ A.value "gb" ] [ text "English" ]
                ]
            ]
        , label []
            [ text "Find pangrams"
            , button [ E.onClick FetchPangrams ] [ text "Go" ]
            ]
        ]


pangramAnswer : String -> Html Msg
pangramAnswer answer =
    tr []
        [ td [] [ text answer ]
        ]


pangramAnswers : Model -> Html Msg
pangramAnswers model =
    let
        answers =
            List.map pangramAnswer model.pangramAnswers
    in
    table [ A.style "border" "2px solid" ] answers


view : Model -> Html Msg
view model =
    div []
        [ pangramForm model
        , pangramAnswers model
        ]


submitForm : Model -> Cmd Msg
submitForm model =
    Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetLanguage lang ->
            ( { model | language = lang }, Cmd.none )

        SetMustContain mustContain ->
            let
                newLetter =
                    case String.uncons mustContain of
                        Just ( hd, _ ) ->
                            hd

                        Nothing ->
                            model.mustContain
            in
            ( { model | mustContain = newLetter }, Cmd.none )

        SetLetters letters ->
            ( { model | letters = letters }, Cmd.none )

        FetchPangrams ->
            ( model, submitForm model )


main =
    B.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \s -> Sub.none
        }
