module Qna exposing (..) 
  
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..) 
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode
import Version exposing (version, gitRepo)

import ElmEscapeHtml exposing (..) 

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

qnamakerUriBase: String 
qnamakerUriBase = "https://westus.api.cognitive.microsoft.com/qnamaker/v1.0"

qnamakerSubscriptionKey: String 
qnamakerSubscriptionKey = "a6fbd18b9b2e45b59f2ce4f73a56e1e4"
 
knowledgebaseId : String 
knowledgebaseId = "b693c8be-313c-434d-b3a7-dad2d4656039"

knowledgebaseId2 : String 
knowledgebaseId2 = "8c59a93f-1622-4ce3-b848-dcc56f10f2b0"

knowledgebaseId3 : String 
knowledgebaseId3 = "ed3f0ded-b71e-43ff-93c6-a34454702b64"

builder : String -> String 
builder kid = 
  qnamakerUriBase ++ "/knowledgebases/" ++ kid ++ "/generateAnswer"

randomGifUrl : String
randomGifUrl = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag="

type alias Model =
  { topic : String
  , gifUrl : String
  , knowledgeBase : List QnAService
  , answer : List Answer
  }
  
type alias QnAService = 
  { name : String 
  , url  : String 
  }

type alias Answer = 
  { kBase : String 
  , text  : String
  , confidence : String
  }

type alias Response =
  { answer : String 
  , score : String
  }

kBase : List QnAService
kBase = 
  [ QnAService "cpp" (builder knowledgebaseId3)
  , QnAService "cse" (builder knowledgebaseId2)
  , QnAService "ds"  (builder knowledgebaseId) 
  ]

initialModel : Model 
initialModel = 
  Model "what are algorithms?" 
    "img/barrelOfMonkeys.gif" 
    kBase 
    [Answer "Unknown" "algorithms are eating the world!" "0.0"]

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
  | NewResponse (Result Http.Error Response)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      -- (model, getRandomGif model.topic)
      let 
        model_ = { model | answer   = [], 
                      knowledgeBase = kBase } 
      in
        ( model_, getAnswer model_ )

    NewGif (Ok newUrl) ->
      ( { model | gifUrl = newUrl }, Cmd.none)

    NewGif (Err _) ->
      ( model, Cmd.none)

    NewAnswer (Ok answer) ->
      let 
        model_ = addResponse model answer "0.0"
      in 
        ( model_, getAnswer model_)

    NewAnswer (Err error) ->
      let 
        model_ = addResponse model (toString error) "0.0" 
      in 
        ( model_, getAnswer model_)

    NewResponse (Ok response) -> 
      let 
        model_ = addResponse model response.answer response.score 
      in 
        ( model_, getAnswer model_)
    NewResponse (Err error) -> 
        let 
          model_ = addResponse model (toString error) "0.0"
        in 
          ( model_, getAnswer model_) 


    Topic s -> 
      ( {model |topic = s}, Cmd.none)

addResponse : Model -> String -> String -> Model 
addResponse model response score =
  case model.knowledgeBase of 
    [] -> 
      model 
    
    kHead :: kTail -> 
      { model | answer = 
          (Answer kHead.name (unescape response) score) :: model.answer, 
          knowledgeBase = kTail } 

-- VIEW

view : Model -> Html Msg
view model =
  div [ class "example example-dotted"]
    [ h1 [] [ text "Elm QnA" ]
    , hr [class "style8"] []
    , h2 [] [ text model.topic ]
    , footer
    , input [ type_ "text", placeholder "Topic", onInput Topic ] []
    , button [ onClick MorePlease ] [ text "Get Answers!" ]
    --, div [] [text (toString model.answer) ]
    --, br [] []
    , viewAllAnswers model 
    --, br [] []
    , img [src model.gifUrl] []
    ]

viewAllAnswers: Model -> Html Msg 
viewAllAnswers model =
  let 
    listOfAnswers = 
      List.map viewAnswer model.answer
  in 
    ul [] listOfAnswers  

viewAnswer: Answer -> Html Msg 
viewAnswer answer = 
  li []
    [ span [class "kBase" ] [text (answer.kBase ++ "  ")]
    , span [class "score" ] [text (answer.confidence ++ "  ")]
    , span [class "answer"] [text answer.text]
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

{--getReponse : Cmd Msg 
getReponse = 
  (Decode.string responseDecoder)
    |> Http.request settings
    |> Http.send NewResponse 
--}

getAnswer : Model -> Cmd Msg
getAnswer model =
  case model.knowledgeBase of 
    [] -> 
      getRandomGif model.topic 
    
    kHead :: kTail -> 
      
      let
        settings =
          { method  = "POST"
          , headers = 
              [ Http.header "Ocp-Apim-Subscription-Key" qnamakerSubscriptionKey
              , Http.header "Cache-Control" "no-cache"
              -- , Http.header "Content-Type" "application/json"
              ]
          , url     = kHead.url
          -- , body = emptyBody
          , body    = jsonBody (encodeQuestion model.topic) 
          , expect  = expectJson decodeResponse
          , timeout = Nothing
          , withCredentials = False
          }
        request =
          Http.request settings 
      in
        Http.send NewResponse request


encodeQuestion : String -> Encode.Value        
encodeQuestion question =
  Encode.object 
    [ ("question", Encode.string question)]

decodeResponse : Decode.Decoder Response 
decodeResponse = 
  Decode.map2 Response 
    (field "answer" Decode.string)
    (field "score" Decode.string)

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
