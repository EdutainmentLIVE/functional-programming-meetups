module Main exposing (main)

import Html exposing (Html, button, div, text, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src, height)
import CSS

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model = Int

model : Model
model =
  0

-- UPDATE

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

-- VIEW

view : Model -> Html Msg
view model =
  div [CSS.margin1rem]
    [ button [ onClick Decrement ] [ text "-" ]
    , div [CSS.backgroundColorRed] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    , img [height 200, src "./cool-dog.jpg"] []
    ]
