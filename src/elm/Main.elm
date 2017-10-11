import Model
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App
import Array
import String
import Char

--========== Machine environment ====================--
type MachineSpeed = Paused | RunningSlow | RunningFast
type alias MemoryEditState = { addr : Int, value : String, validated : Maybe Int }

type alias State = {
  machineState: Model.MachineState,
  speed: MachineSpeed,
  editingMemory: Maybe MemoryEditState
}

type SpeedCmd = Reset | Pause | Step | RunSlow | RunFast
type MemInputCmd = SelectMem Int | UpdateMemEdit Int String | UpdateMem Int String
type Msg = SelectSpeed SpeedCmd | MemoryEvent MemInputCmd

---------- Main: ----------------------------------------

init : (State, Cmd Msg)
init =
  let
    model = {
      machineState = Model.init(),
      speed = Paused,
      editingMemory = Nothing
      }
    cmd = Cmd.none
  in (model,cmd)

update : Msg -> State -> (State, Cmd Msg)
update msg model =
       --(model, Cmd.none)
  (case msg of
    MemoryEvent(SelectMem addr) ->
      let memValue = Maybe.withDefault 0 (Array.get addr model.machineState.memory)
      in {model | editingMemory=Just {addr=addr, value=(toString memValue), validated=Just memValue}}
    MemoryEvent(UpdateMemEdit addr value) ->
      {model | editingMemory=Just {addr=addr, value=value, validated=validateDatum value}}
    MemoryEvent(UpdateMem addr valueStr) ->
      let ms = model.machineState
      in case String.toInt valueStr of
        Ok value -> {model | machineState={ms | memory=Array.set addr value model.machineState.memory}}
        Err reason -> model --TODO: Error feedback
    SelectSpeed Step ->
      {model | machineState=Model.step(model.machineState)}
    other -> model
   , Cmd.none)

validateDatum : String -> Maybe Int
validateDatum s =
  case String.toInt s of
    Ok value -> Just value
    Err reason -> Nothing

subscriptions : State -> Sub Msg
subscriptions model = Sub.none

view : State -> Html Msg
view model = body [
       style [ ("backgroundColor", "black") ]
     ] [
     div [style [ ("color", "white") ]] [
       div [] [text "Hej!"],
       button [onClick (SelectSpeed Step)] [text "Step"],
     memoryView model.machineState.memory {editingState=model.editingMemory, ip=model.machineState.ip}
     ]
     ]

memoryView mem state =
  div [] [
    table [] [
      thead [] [text "Memory"],
       tbody [] (memoryViewRows mem state)]
  ]

memoryViewRows mem state =
  Array.toList (Array.indexedMap (memoryViewRow state) mem)

ipArrow =
  String.fromChar (Char.fromCode 0x25B6) -- "Black Right-Pointing Triangle"

memoryViewRow state memAddr memValue =
  tr [] [
    td [] (if memAddr == state.ip
           then [ span [style [("color", "#6f6")]] [text ipArrow] ]
           else []),
    td [style [("text-align","right")] ] [text ((toString memAddr)++":")],
    td [] (memoryValueEditCell memAddr memValue state)
  ]

memoryValueEditCell memAddr memValue state =
  let editingValue =
        case state.editingState of
         Just em -> if em.addr==memAddr then Just em else Nothing
         Nothing -> Nothing
      (curValue,bgcolor) =
        case editingValue of
          Nothing -> (toString memValue, "#eef")
          Just {value,validated} -> (value, case validated of
                                              Nothing -> "#fcc"
                                              Just _ -> "#fff")
  in [
      Html.input [
        style [("text-align", "right"), ("backgroundColor", bgcolor)],
        value curValue,
        onFocus (MemoryEvent(UpdateMemEdit memAddr (toString memValue))),
        onClick (MemoryEvent(UpdateMemEdit memAddr (toString memValue))),
        onInput (\v->MemoryEvent(UpdateMemEdit memAddr v)),
        onBlur (MemoryEvent(UpdateMem memAddr curValue))
      ] []
     ]

main : Program Never
main =
 Html.App.program {
      init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- signature of `program` :
--     { init : ( a, Cmd b )
--     , subscriptions : a -> Sub b
--     , update : b -> a -> ( a, Cmd b )
--     , view : a -> Html b
--     }
