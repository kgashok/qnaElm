import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode

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

builder : String 
builder = qnamakerUriBase ++ "/knowledgebases/" ++ knowledgebaseId ++ "/generateAnswer"

payload : String 
payload = "{\"question\":\"Why bother with hashing?\"}"

{--
type alias Header = 
  { ocp-apim-subscription-key : "a6fbd18b9b2e45b59f2ce4f73a56e1e4",
  , content-type : "application/json",
  , cache-control : "no-cache"
  }
--}

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

-- Encode user to construct POST request body (for Register and Log In)
userEncoder : String -> Encode.Value
userEncoder query = 
  Encode.object 
    [ ("question", Encode.string query)]


getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    settings =
      { verb = "POST"
      , headers = [ ("Content-Type", "application/json")
                  , ("Ocp-Apim-Subscription-Key", "a6fbd18b9b2e45b59f2ce4f73a56e1e4")
                  , ("Cache-Control", "no-cache") ]
      , url  = kid
      , body = toString <| Encode.encode 0 <| userEncoder topic
      -- , body = Http.string <| Encode.encode 0 <| userEncoder topic
      }
  in
    -- Http.send settings Http.defaultSettings
    Http.send NewGif request
    -- |> Http.fromJson tokenDecoder 

decodeQAUrl : Decode.Decoder String
decodeQAUrl =
  Decode.at ["data", "answer"] Decode.string
  -- Decode.at ["answer"] Decode.string


{-- Decode POST response to get token
tokenDecoder : Decoder String
tokenDecoder =
    "answer" := Decode.string
--}
