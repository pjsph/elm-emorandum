module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map2, field, int, string, list)
import Random
import Embed.Youtube
import Embed.Youtube.Attributes


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL


type Model
    = Word (Maybe String) 
    | Def (List Definition) String Bool
    | GaveUp (Maybe String)

type alias Meaning =
    {
        partOfSpeech : String,
        definitions : List String
    }

type alias Definition =
    {
        word : String,
        meanings : List Meaning
    }

init : () -> (Model, Cmd Msg)
init _ =
  (Word (Just ""), Http.get { url = "../static/words_list.txt", expect = Http.expectString Resp })



-- UPDATE


type Msg
  = Resp (Result Http.Error String)
  | RandNumber Int
  | GotDef (Result Http.Error (List Definition))
  | Type String
  | GiveUp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Resp result ->
        case result of
            Ok body -> (Word (Just body), Random.generate RandNumber (Random.int 0 998))
            Err _ -> (Word Nothing, Cmd.none)
    RandNumber nbr ->
        case model of
            Word (Just str) -> 
                case String.split " " str of
                    [] -> (Word Nothing, Cmd.none)
                    (x::xs) -> 
                        let word = List.head (List.drop nbr (x::xs))
                        in
                            case word of
                                Just str2 -> (Word word, getDefinition str2)
                                Nothing -> (Word word, Cmd.none)
            GaveUp _ -> (model, Cmd.none)
            Word Nothing -> (Word Nothing, Cmd.none)
            Def _ _ _ -> (model, Cmd.none)
    GotDef result -> 
        case result of
            Ok def -> (Def def "" False, Cmd.none)
            Err _ -> (Word Nothing, Cmd.none)
    Type inp ->
        case model of
            Word (Just str) -> (Word (Just str), Cmd.none)
            Word Nothing -> (Word Nothing, Cmd.none)
            Def defList _ _ -> 
                let h = List.head defList in
                    case h of
                        Just def -> let found = (inp == def.word) in (Def defList inp found, Cmd.none)
                        Nothing -> (Def defList inp False, Cmd.none)
            GaveUp _ -> (model, Cmd.none)
    GiveUp ->
        case model of
            Word _ -> (model, Cmd.none)
            Def defList _ _ -> 
                let h = List.head defList in
                    case h of
                        Just def -> (GaveUp (Just def.word), Cmd.none)
                        Nothing -> (Word Nothing, Cmd.none)
            GaveUp _ -> (model, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  div [] (renderMainPage model)
   

renderMainPage : Model -> List (Html Msg)
renderMainPage model =
    h1 [style "margin-left" "30px" ] [ text "el-Memorandum" ] ::
    ( case model of
        Word _ -> []
        GaveUp (Just str) -> div [ style "margin-left" "50px" ] [ Embed.Youtube.fromString "dQw4w9WgXcQ"
                |> Embed.Youtube.attributes
                    [ Embed.Youtube.Attributes.width 640
                    , Embed.Youtube.Attributes.height 400
                    , Embed.Youtube.Attributes.autoplay 
                    ]
                |> Embed.Youtube.toHtml ] :: h1 [style "margin-left" "50px"] [ text ("The word was '" ++ str ++ "'") ] :: []
        GaveUp Nothing -> h1 [] [ text "Err!" ] :: []
        Def defList inp found ->
            if found == True
            then renderDefList defList :: h2 [ style "margin-left" "50px" ] [ text "Bravo!" ] :: input [ style "margin-left" "50px", style "margin-bottom" "50px", placeholder "Your guess", value inp, onInput Type ] [] :: []
            else renderDefList defList :: h2 [ style "margin-left" "50px" ] [ text "Type your guess" ] :: input [ style "margin-left" "50px", style "margin-bottom" "50px", placeholder "Your guess", value inp, onInput Type ] [] :: button [ onClick GiveUp ] [ text "Give Up" ] :: []
    )


renderDefList : List Definition -> Html Msg
renderDefList lst =
    lst
        |> List.map (\d -> li [ style "margin-bottom" "50px" ] [ text "meaning", renderMeaningList d.meanings ])
        |> ul [ ]


renderMeaningList : List Meaning -> Html Msg
renderMeaningList lst =
    lst
        |> List.map (\m -> li [ style "margin-bottom" "20px" ] [ text m.partOfSpeech, renderSubDefList m.definitions ])
        |> ul []


renderSubDefList : List String -> Html Msg
renderSubDefList lst = 
    lst
        |> List.map (\s -> li [ style "margin-bottom" "10px" ] [ text s ])
        |> ol []


-- HTTP


getDefinition : String -> Cmd Msg
getDefinition word =
    Http.get {  url = "https://api.dictionaryapi.dev/api/v2/entries/en/"++word,
                expect = Http.expectJson GotDef (list defDecoder)
             }


defDecoder : Decoder Definition
defDecoder =
    map2 Definition
        (field "word" string)
        (field "meanings" (list meaningDecoder))


meaningDecoder : Decoder Meaning
meaningDecoder =
    map2 Meaning
        (field "partOfSpeech" string)
        (field "definitions" (list subDefDecoder))


subDefDecoder : Decoder String
subDefDecoder =
    (field "definition" string)
