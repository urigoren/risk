import Html exposing (..)
import Html.Attributes exposing (..)
import Platform.Cmd exposing (..)
import Html.Events exposing (..)
import String
import Random

type alias Flags =
  { foo : Int
  , nested : { bar : String }
  }

main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  {
   attacking: Bool
   , attackerPieces: Int
   , defenderPieces: Int
   , defenderDice : List Int
   , attackerDice : List Int
   , attackerLoss: Int
   , defenderLoss: Int
   , attackerGuardMinimum: Int
  }


init : Flags -> (Model, Cmd Msg)
init flags =
  {
  attacking = False
  , attackerGuardMinimum = 1
  , attackerPieces = 0
  , defenderPieces = 0
  , defenderDice = [0]
  , attackerDice = [0]
  , attackerLoss = 0
  , defenderLoss = 0
  } ! []



  -- UPDATE

type Msg =  Battle Int| SetAttackerPieces String | SetDefenderPieces String | Attack | NewAttackerList (List Int)| NewDefenderList (List Int)

update msg model = case msg of
  (Attack) ->  let numDiceAttacker = if model.attackerPieces-model.attackerLoss-model.attackerGuardMinimum > 3 then 3 else model.attackerPieces-model.attackerLoss-model.attackerGuardMinimum in
    case model.attacking of
    False -> ({model| attacking=True, attackerLoss=0, defenderLoss=0}, Random.generate NewAttackerList (Random.list numDiceAttacker (Random.int 1 6)))
    True -> ({model| attacking=True}, Random.generate NewAttackerList (Random.list numDiceAttacker (Random.int 1 6)))
  (NewAttackerList lst) ->  let numDiceDefender = Basics.min 2 model.defenderPieces-model.defenderLoss in
  ({model | attackerDice = List.reverse <| List.sort lst},  Random.generate NewDefenderList (Random.list numDiceDefender (Random.int 1 6)))
  (NewDefenderList lst) ->  ({model | defenderDice = List.reverse <| List.sort lst},  send Battle)
  (SetAttackerPieces str) ->  ({model | attackerPieces = toIntOrZeros str},  Cmd.none)
  (SetDefenderPieces str) ->  ({model | defenderPieces = toIntOrZeros str},  Cmd.none)
  (Battle i) -> let results = battle model in ({model |
  attackerLoss = model.attackerLoss - (List.sum (List.filter (\x->x<0) results))
  ,  defenderLoss = model.defenderLoss + (List.sum (List.filter (\x->x>0) results))
  }, Cmd.none)


  -- VIEW
view : Model -> Html Msg
view model =
    div []
      [ input [type_ "text", onInput SetAttackerPieces, placeholder "Attacker Pieces"] []
      , input [type_ "text", onInput SetDefenderPieces , placeholder "Defender Pieces"] []
      , input [type_ "button", onClick Attack, value "Attack"] []
      , table [] [ tr [] [
          td [] [
            text <| toString model.attackerLoss
          ]
          ,td [attribute "valign" "top"] [
            ul [color "red" ] (List.map (\x -> li [] [text (toString x)]) model.attackerDice)
          ]
          ,td [attribute "valign" "top"] [
            ul [color "blue" ] (List.map (\x -> li [] [text (toString x)]) model.defenderDice)
          ]
        ]]
      ]


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- HELPER FUNCS

color c = style [("color", c)]

battle model = List.map (\(x,y) -> if x>y then 1 else -1) (zip model.attackerDice model.defenderDice)

toIntOrZeros str = case String.toInt str of
                          Err msg -> 0
                          Ok val -> val

zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xBack, y :: yBack ) ->
        (x,y) :: zip xBack yBack

    (_, _) ->
        []

send msg =
  Random.generate msg (Random.int 1 6)
