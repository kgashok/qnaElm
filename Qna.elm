import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode

main : Program Never Model Msg
main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

kid : String
kid = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag="

knowledgebaseId = "b693c8be-313c-434d-b3a7-dad2d4656039"
qnamakerSubscriptionKey = "a6fbd18b9b2e45b59f2ce4f73a56e1e4"
qnamakerUriBase = "https://westus.api.cognitive.microsoft.com/qnamaker/v1.0"

builder = qnamakerUriBase ++ "/knowledgebases/" ++ knowledgebaseId ++ "/generateAnswer"

payload = "{\"question\":\"Why bother with hashing?\"}"
headers = {
    "ocp-apim-subscription-key": "a6fbd18b9b2e45b59f2ce4f73a56e1e4",
    "content-type": "application/json",
    "cache-control": "no-cache",
}


type alias Model =
  { topic : String
  , gifUrl : String
  -- , knowledgeBase : String 
  }

init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic "waiting.gif"  
  , getRandomGif topic
  )


-- UPDATE

type Msg
  = MorePlease
  | NewGif (Result Http.Error String)
  | Topic String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, getRandomGif model.topic)

    NewGif (Ok newUrl) ->
      ( { model | gifUrl = newUrl }, Cmd.none)

    NewGif (Err _) ->
      (model, Cmd.none)

    Topic s -> 
      ( {model |topic = s}, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.topic]
    , input [ type_ "text", placeholder "Topic", onInput Topic ] []
    , button [ onClick MorePlease ] [ text "More Please!" ]
    , br [] []
    , img [src model.gifUrl] []
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP -- 

getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url = 
      kid ++ topic

    request =
      Http.get url decodeGifUrl
  in
    Http.send NewGif request

decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string