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
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

knowledgebaseId : String 
knowledgebaseId = "b693c8be-313c-434d-b3a7-dad2d4656039"

knowledgebaseId2 : String 
knowledgebaseId2 = "8c59a93f-1622-4ce3-b848-dcc56f10f2b0"

knowledgebaseId3 : String 
knowledgebaseId3 = "ed3f0ded-b71e-43ff-93c6-a34454702b64"

qnamakerSubscriptionKey: String 
qnamakerSubscriptionKey = "a6fbd18b9b2e45b59f2ce4f73a56e1e4"

qnamakerUriBase: String 
qnamakerUriBase = "https://westus.api.cognitive.microsoft.com/qnamaker/v1.0"

builder : String -> String 
builder kid = 
  qnamakerUriBase ++ "/knowledgebases/" ++ kid ++ "/generateAnswer"

randomGifUrl : String
randomGifUrl = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag="

payload : String 
payload = "{\"question\":\"Why bother with hashing?\"}"

type alias Model =
  { topic : String
  , gifUrl : String
  , knowledgeBase : List String 
  , answer : List String
  }

initialModel : Model 
initialModel = 
  Model "barrel of monkeys" 
    "img/barrelOfMonkeys.gif" 
    [ builder knowledgebaseId2 
    , builder knowledgebaseId
    , builder knowledgebaseId3
    ] 
    ["Barrel of Monkeys"]

init : (Model, Cmd Msg)
init  =
  ( initialModel 
  , getAnswer initialModel 
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
      let 
        model_ = { model | answer = [], 
                      knowledgeBase = 
                        [ builder knowledgebaseId2 
                        , builder knowledgebaseId
                        , builder knowledgebaseId3
                        ] 
        } 
      in 
        (model_, getAnswer model_)

    NewGif (Ok newUrl) ->
      ( { model | gifUrl = newUrl }, Cmd.none)

    NewGif (Err _) ->
      (model, Cmd.none)

    NewAnswer (Ok answer) ->
      let 
        model_ = { model | answer = answer :: model.answer,
                    knowledgeBase = Maybe.withDefault ["QED"] (List.tail model.knowledgeBase)
                 }
      in 
        ( model_, getAnswer model_) 
    NewAnswer (Err err) -> 
      ( { model | answer = [(toString err)]}, Cmd.none)

    Topic s -> 
      ( {model |topic = s}, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.topic]
    , footer
    , input [ type_ "text", placeholder "Topic", onInput Topic ] []
    , button [ onClick MorePlease ] [ text "Get answer!" ]
    , div [] [text (toString model.answer) ]
    , br [] []
    , img [src model.gifUrl] []
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

getAnswer : Model -> Cmd Msg
getAnswer model =
  let
    settings =
      { method  = "POST"
      , headers = [ Http.header "Ocp-Apim-Subscription-Key" "a6fbd18b9b2e45b59f2ce4f73a56e1e4"
                  , Http.header "Cache-Control" "no-cache"
                  -- , Http.header "Content-Type" "application/json"
                  ]
      , url     = Maybe.withDefault "QED" (List.head model.knowledgeBase)
      -- , body = emptyBody
      , body    = jsonBody (encodeQuestion model.topic) 
      , expect  = expectJson decodeAnswer
      , timeout = Nothing
      , withCredentials = False
      }
    request =
      Http.request settings 
  in
    case settings.url of 
      "QED" -> 
        -- Cmd.none
        getRandomGif model.topic
      _-> 
        Http.send NewAnswer request

encodeQuestion : String -> Encode.Value        
encodeQuestion question =
  Encode.object 
    [ ("question", Encode.string question)]

decodeAnswer : Decode.Decoder String
decodeAnswer =
  Decode.at ["answer"] Decode.string

getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url     = randomGifUrl ++ topic
    request = Http.get url decodeGifUrl
  in
    Http.send NewGif request

decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string
