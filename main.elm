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
   attackerPieces: Int,
   defenderPieces: Int,
   defenderDice : List Int,
   attackerDice : List Int
  }


init : Flags -> (Model, Cmd Msg)
init flags =
  {
  attackerPieces = 0
  , defenderPieces = 0
  , defenderDice = [0]
  , attackerDice = [0]
  } ! []



  -- UPDATE

type Msg =  SetAttackerPieces String | SetDefenderPieces String | RollDice | NewAttackerList (List Int)| NewDefenderList (List Int)

update msg model = case msg of
  (RollDice) ->  (model, Random.generate NewAttackerList (Random.list 3 (Random.int 1 6)))
  (NewAttackerList lst) ->  ({model | attackerDice = List.reverse <| List.sort lst},  Random.generate NewDefenderList (Random.list 2 (Random.int 1 6)))
  (NewDefenderList lst) ->  ({model | defenderDice = List.reverse <| List.sort lst},  Cmd.none)
  (SetAttackerPieces str) ->  ({model | attackerPieces = case String.toInt str of
                                        Err msg -> 0
                                        Ok val -> val
                              },  Cmd.none)
  (SetDefenderPieces str) ->  ({model | defenderPieces = case String.toInt str of
                                        Err msg -> 0
                                        Ok val -> val
                              },  Cmd.none)


  -- VIEW
view : Model -> Html Msg
view model =
    div []
      [ input [type_ "text", onInput SetAttackerPieces, placeholder "Attacker Pieces"] []
      , input [type_ "text", onInput SetDefenderPieces , placeholder "Defender Pieces"] []
      , input [type_ "button", onClick RollDice, value "Roll"] []
      , table [] [ tr [] [
          td [] [
            ul [] (List.map (\x->li [] [text (toString x)]) (battle model))
          ]
          ,td [attribute "valign" "top"] [
            ul [color "red" ] (List.map (\x -> li [] [text (toString x)]) model.attackerDice)
          ]
          ,td [attribute "valign" "top"] [
            ul [color "blue" ] (List.map (\x -> li [] [text (toString x)]) model.defenderDice)
          ]
        ]]
      ]

color c = style [("color", c)]

battle model = List.map (\(x,y) -> if x>y then 1 else -1) (zip model.attackerDice model.defenderDice)

zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xBack, y :: yBack ) ->
        (x,y) :: zip xBack yBack

    (_, _) ->
        []

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
