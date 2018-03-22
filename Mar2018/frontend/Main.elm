port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Platform.Cmd as Cmd
import Platform.Sub as Sub


port trigger : Value -> Cmd msg


port messagePosted : (Value -> msg) -> Sub msg


type alias Model =
    { posts : Int
    , input : String
    , messages : List String
    }


type Msg
    = PostMessage String
    | Input String
    | MessagePosted Value


main : Program Never Model Msg
main =
    program
        { init = ( { posts = 0, input = "", messages = [] }, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> messagePosted MessagePosted
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            ( { model | input = input }, Cmd.none )

        PostMessage message ->
            ( { model
                | posts = model.posts + 1
                , input = ""
              }
            , trigger (Encode.string message)
            )

        MessagePosted value ->
            let
                newMessage =
                    decodeMessage value
            in
            ( { model | messages = newMessage :: model.messages }
            , Cmd.none
            )


decodeMessage : Value -> String
decodeMessage value =
    case Decode.decodeValue Decode.string value of
        Ok msg ->
            msg

        Err msg ->
            "Error: " ++ msg


view : Model -> Html Msg
view model =
    div []
        [ text "Hello, Functional Programmers!"
        , div []
            [ text (toString model.posts)
            , text " messages sent"
            ]
        , div []
            [ text "Current Input: "
            , text model.input
            ]
        , div []
            [ input [ onInput Input, value model.input ] []
            , button
                [ style [ ( "color", "blue" ) ]
                , onClick (PostMessage model.input)
                ]
                [ text "Send" ]
            ]
        , div []
            (List.map renderMessage model.messages)
        ]


renderMessage : String -> Html Msg
renderMessage message =
    div []
        [ text message
        ]
