module Main exposing (..)

-- Press a button to send a GET request for random quotes.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/json.html
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map4, field, int, string)
import Random



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model = Maybe String

init : () -> (Model, Cmd Msg)
init _ =
  (Just "", Http.get { url = "../static/words_list.txt", expect = Http.expectString Resp })



-- UPDATE


type Msg
  = Resp (Result Http.Error String)
  | RandNumber Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Resp result ->
        case result of
            Ok body -> (Just body, Random.generate RandNumber (Random.int 0 998))
            Err _ -> (Nothing, Cmd.none)
    RandNumber nbr ->
        case model of
            Just str -> 
                case String.split " " str of
                    [] -> (Nothing, Cmd.none)
                    (x::xs) -> (List.head (List.drop nbr (x::xs)), Cmd.none)
            Nothing -> (Nothing, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (case model of
                        Just str -> str
                        Nothing -> "Err!"
                    ) ]
    ]
