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
  { logRolls: List (List Int , List Int)
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
  { logRolls = []
  , attackerGuardMinimum = 1
  , attackerPieces = 0
  , defenderPieces = 0
  , defenderDice = [0]
  , attackerDice = [0]
  , attackerLoss = 0
  , defenderLoss = 0
  } ! []



  -- UPDATE

type Msg =  Battle Int| SetAttackerPieces String | SetDefenderPieces String | SetMinimumAttackerGuard String| Attack Int | NewAttackerList (List Int)| NewDefenderList (List Int)

update msg model = case msg of
  (Attack again) ->  let numDiceAttacker = if model.attackerPieces-model.attackerLoss-model.attackerGuardMinimum > 3 then 3 else model.attackerPieces-model.attackerLoss-model.attackerGuardMinimum in
    if again==0 then
    ({model| attackerLoss=0, defenderLoss=0, logRolls=[]}, Random.generate NewAttackerList (Random.list numDiceAttacker (Random.int 1 6)))
    else
    ({model | logRolls = model.logRolls ++ [(model.attackerDice, model.defenderDice)]}, Random.generate NewAttackerList (Random.list numDiceAttacker (Random.int 1 6)))
  (NewAttackerList lst) ->  let numDiceDefender = Basics.min 2 model.defenderPieces-model.defenderLoss in
  ({model | attackerDice = List.reverse <| List.sort lst},  Random.generate NewDefenderList (Random.list numDiceDefender (Random.int 1 6)))
  (NewDefenderList lst) ->  ({model | defenderDice = List.reverse <| List.sort lst},  send Battle 0)
  (SetAttackerPieces str) ->  ({model | attackerPieces = toIntOrZeros str},  Cmd.none)
  (SetDefenderPieces str) ->  ({model | defenderPieces = toIntOrZeros str},  Cmd.none)
  (SetMinimumAttackerGuard str) ->  ({model | attackerGuardMinimum = toIntOrZeros str},  Cmd.none)
  (Battle i) -> let
    results = battle model
    aLoss = model.attackerLoss - (List.sum (List.filter (\x->x<0) results))
    dLoss = model.defenderLoss + (List.sum (List.filter (\x->x>0) results))
    next = if (model.attackerPieces-aLoss>model.attackerGuardMinimum) && (model.defenderPieces>dLoss) then send Attack 1 else  Cmd.none
  in
   ({model | attackerLoss = aLoss,  defenderLoss = dLoss}, next)


  -- VIEW
view : Model -> Html Msg
view model =
    div []
      [ input [type_ "text", onInput SetAttackerPieces, placeholder "Attacker Pieces"] []
      , input [type_ "text", onInput SetDefenderPieces , placeholder "Defender Pieces"] []
      , input [type_ "text", onInput SetMinimumAttackerGuard , placeholder "Minimum Attacker Guard"] []
      , input [type_ "button", onClick (Attack 0), value "Attack"] []
      , div [] [text ("Attacker loss: " ++ (toString model.attackerLoss) ++ ", Defender loss: " ++ (toString model.defenderLoss))]
      , table [] (List.map viewOneRoll (model.logRolls ++ [(model.attackerDice, model.defenderDice)]))
      ]


viewOneRoll tuple =
  let
    a = Tuple.first tuple
    d = Tuple.second tuple
  in
  tr [] [
    td [attribute "valign" "top"] [
      ul [color "red" ] (List.map (\x -> li [] [text (toString x)]) a)
    ]
    ,td [attribute "valign" "top"] [
      ul [color "blue" ] (List.map (\x -> li [] [text (toString x)]) d)
    ]
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

send msg v=
  Random.generate msg (Random.int v v)
