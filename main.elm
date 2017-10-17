import Html exposing (..)
import Html.Attributes exposing (..)
import Platform.Cmd exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)
import Random

type alias DefaultValues =
  { attackerPieces : Int
  , defenderPieces : Int
  , attackerGuardMinimum : Int
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


init : DefaultValues -> (Model, Cmd Msg)
init defaults =
  { logRolls = []
  , attackerGuardMinimum = defaults.attackerGuardMinimum
  , attackerPieces = defaults.attackerPieces
  , defenderPieces = defaults.defenderPieces
  , defenderDice = []
  , attackerDice = []
  , attackerLoss = 0
  , defenderLoss = 0
  } ! []



  -- UPDATE

type Msg =  Battle Int| SetAttackerPieces String | SetDefenderPieces String | SetMinimumAttackerGuard String| Attack Int | NewAttackerList (List Int)| NewDefenderList (List Int)

update msg model = case msg of
  (SetAttackerPieces str) ->  ({model | attackerPieces = toIntOrZeros str},  Cmd.none)
  (SetDefenderPieces str) ->  ({model | defenderPieces = toIntOrZeros str},  Cmd.none)
  (SetMinimumAttackerGuard str) ->  ({model | attackerGuardMinimum = toIntOrZeros str},  Cmd.none)
  (Attack again) ->  let
      numDiceAttacker = Basics.min 3 (model.attackerPieces-model.attackerLoss-model.attackerGuardMinimum)
      attackerRoll = Random.generate NewAttackerList (Random.list numDiceAttacker (Random.int 1 6))
    in
    if again == 0 then
    ({model| attackerLoss=0, defenderLoss=0, logRolls=[]}, attackerRoll)
    else
    ({model | logRolls = model.logRolls ++ [(model.attackerDice, model.defenderDice)]}, attackerRoll)
  (NewAttackerList lst) ->  let
      numDiceDefender = Basics.min 2 (model.defenderPieces-model.defenderLoss)
      defenderRoll = Random.generate NewDefenderList (Random.list numDiceDefender (Random.int 1 6))
    in
  ({model | attackerDice = List.reverse <| List.sort lst},  defenderRoll)
  (NewDefenderList lst) ->  ({model | defenderDice = List.reverse <| List.sort lst},  send Battle 0)
  (Battle i) -> let
    battle = List.map (\(x,y) -> if x>y then 1 else -1) (zip model.attackerDice model.defenderDice)
    aLoss = model.attackerLoss - (List.sum (List.filter (\x->x<0) battle))
    dLoss = model.defenderLoss + (List.sum (List.filter (\x->x>0) battle))
    next = if ((model.attackerPieces-aLoss)>model.attackerGuardMinimum) && (model.defenderPieces>dLoss) then send Attack 1 else  Cmd.none
  in
   ({model | attackerLoss = aLoss,  defenderLoss = dLoss}, next)


  -- VIEW
view : Model -> Html Msg
view model =
    div []
      [ h1 [] [(text "Risk Box-Game Dice Simulator")]
      , p [] [text "Attacker Pieces: ", input [type_ "text", onInput SetAttackerPieces, placeholder "Attacker Pieces", value (toString model.attackerPieces)] []]
      , p [] [text "Defender Pieces: ",input [type_ "text", onInput SetDefenderPieces , placeholder "Defender Pieces", value (toString model.defenderPieces)] []]
      , p [] [text "Attacker Minumum: ",input [type_ "text", onInput SetMinimumAttackerGuard , placeholder "Minimum Attacker Guard", value (toString model.attackerGuardMinimum)] []]
      , p[] [input [type_ "button", onClick (Attack 0), value "Attack"] []]
      , div [] [text ("Attacker loss: " ++ (toString model.attackerLoss) ++ ", Defender loss: " ++ (toString model.defenderLoss))]
      , table [] (List.map viewOneRoll (model.logRolls ++ [(model.attackerDice, model.defenderDice)]))
      ]


viewOneRoll (a,d) =
  tr [] [
    td [attribute "valign" "top"] [
      ul [color "red" ] (List.map (\x -> li [] [text (toString x)]) a)
    ]
    ,td [attribute "valign" "top"] [
      ul [color "blue" ] (List.map (\x -> li [] [text (toString x)]) d)
    ]
  ]

color c = style [("color", c)]


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- HELPER FUNCS

toIntOrZeros str = case toInt str of
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
