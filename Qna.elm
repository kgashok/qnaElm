module Qna exposing (..) 

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..) 
import Json.Decode as Decode
import Json.Encode as Encode
import Version exposing (version, gitRepo)

--import Debug exposing (..) 

main : Program Never Model Msg
main =
  Html.program
    { init = init "barrel of monkeys"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

randomGifUrl : String
randomGifUrl = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag="

knowledgebaseId : String 
knowledgebaseId = "b693c8be-313c-434d-b3a7-dad2d4656039"

qnamakerSubscriptionKey: String 
qnamakerSubscriptionKey = "a6fbd18b9b2e45b59f2ce4f73a56e1e4"

qnamakerUriBase: String 
qnamakerUriBase = "https://westus.api.cognitive.microsoft.com/qnamaker/v1.0"

builder : String 
builder = qnamakerUriBase ++ "/knowledgebases/" ++ knowledgebaseId ++ "/generateAnswer"

payload : String 
payload = "{\"question\":\"Why bother with hashing?\"}"

type alias Model =
  { topic : String
  , gifUrl : String
  , knowledgeBase : String 
  , answer : String
  }

init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic "img/barrelOfMonkeys.gif" builder "Barrel of Monkeys"
  , getAnswer topic
  --, (Cmd.batch [getRandomGif topic, getAnswer topic])
  )


-- UPDATE

type Msg
  = MorePlease
  | NewGif (Result Http.Error String)
  | Topic String
  | NewAnswer (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      -- (model, getRandomGif model.topic)
      (model, getAnswer model.topic)

    NewGif (Ok newUrl) ->
      ( { model | gifUrl = newUrl }, Cmd.none)

    NewGif (Err _) ->
      (model, Cmd.none)

    NewAnswer (Ok answer) ->
      ( { model | answer = answer }, getRandomGif model.topic) 
    NewAnswer (Err err) -> 
      ( { model | answer = (toString err)}, Cmd.none)

    Topic s -> 
      ( {model |topic = s}, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.topic]
    , footer
    , input [ type_ "text", placeholder "Topic", onInput Topic ] []
    , button [ onClick MorePlease ] [ text "More Please!" ]
    , br [] []
    , img [src model.gifUrl] []
    , div [] [text (toString model.answer) ]
    ]

footer : Html Msg
footer = 
  div [id "footer"]
  [ a [href (gitRepo ++ "/issues/new"), 
    target "_blank", 
    rel "noopener noreferrer"] 
    [text version]
  ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP -- 

{-- POST register / login request
authUser : Model -> String -> Task Http.Error String
authUser model apiUrl =
    { verb = "POST"
    , headers = [ ("Content-Type", "application/json") ]
    , url = apiUrl
    , body = Http.string <| Encode.encode 0 <| userEncoder model
    }
    |> Http.send Http.defaultSettings
    |> Http.fromJson tokenDecoder
--}


getAnswer : String -> Cmd Msg
getAnswer topic =
  let
    settings =
      { method  = "POST"
      , headers = [ Http.header "Ocp-Apim-Subscription-Key" "a6fbd18b9b2e45b59f2ce4f73a56e1e4"
                  , Http.header "Cache-Control" "no-cache"
                  -- , Http.header "Content-Type" "application/json"
                  ]
      , url     = builder
      -- , body = emptyBody
      , body    = jsonBody (encodeQuestion topic) 
      , expect  = expectJson decodeAnswer
      , timeout = Nothing
      , withCredentials = False
      }
    request =
      Http.request settings 
  in
    Http.send NewAnswer request  

encodeQuestion : String -> Encode.Value        
encodeQuestion question =
  Encode.object 
    [ ("question", Encode.string question)]

decodeAnswer : Decode.Decoder String
decodeAnswer =
  Decode.at ["answer"] Decode.string

decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string


getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    settings =
      { verb = "POST"
      , headers = [ ("Content-Type", "application/json")
                  , ("Ocp-Apim-Subscription-Key", "a6fbd18b9b2e45b59f2ce4f73a56e1e4")
                  , ("Cache-Control", "no-cache") ]
      , url  = randomGifUrl
      }
    randomGifUrl_ = randomGifUrl ++ topic
    request =
      Http.get randomGifUrl_ decodeGifUrl
  in
    Http.send NewGif request

