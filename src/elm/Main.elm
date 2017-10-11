import Model
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App
import Array
import String

--========== Machine environment ====================--
type MachineSpeed = Paused | RunningSlow | RunningFast
type alias MemoryEditState = { addr : Int, value : String }

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
      in {model | editingMemory=Just {addr=addr, value=(toString memValue)}}
    MemoryEvent(UpdateMemEdit addr value) ->
      {model | editingMemory=Just {addr=addr, value=value}}
    MemoryEvent(UpdateMem addr valueStr) ->
      let ms = model.machineState
      in case String.toInt valueStr of
        Ok value -> {model | machineState={ms | memory=Array.set addr value model.machineState.memory}}
        Err reason -> model --TODO: Error feedback
    other -> model
   , Cmd.none)

subscriptions : State -> Sub Msg
subscriptions model = Sub.none

view : State -> Html Msg
view model = body [
       style [ ("backgroundColor", "black") ]
     ] [
     div [style [ ("color", "white") ]] [
     text "Hej!",
     memoryView model.machineState.memory model.editingMemory
     ]
     ]

memoryView mem editingMemory =
           div [] [
             table [] [
               thead [] [text "Memory"],
                tbody [] (memoryViewRows mem editingMemory)]
           ]

memoryViewRows mem editingMemory =
  Array.toList (Array.indexedMap (memoryViewRow editingMemory) mem)

memoryViewRow editingMemory memAddr memValue =
  let editingValue =
    case editingMemory of
     Just({addr,value}) -> if addr==memAddr then Just value else Nothing
     Nothing -> Nothing
  in tr [] [
      td [style [("text-align","right")] ] [text ((toString memAddr)++":")],
      case editingValue of
        Nothing ->
          td [onClick (MemoryEvent(UpdateMemEdit memAddr (toString memValue)))] [text (toString memValue)]
        Just curValue ->
          td [] [
            Html.input [
              placeholder curValue,
              --focused True,
              value curValue,
              onInput (\v->MemoryEvent(UpdateMemEdit memAddr v)),
              onBlur (MemoryEvent(UpdateMem memAddr curValue))
            ] []
          ]
    ]

main : Program Never
main =
 Html.App.program {
      init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Function `program` is expecting the argument to be:
--     { init : ( a, Cmd b )
--     , subscriptions : a -> Sub b
--     , update : b -> a -> ( a, Cmd b )
--     , view : a -> Html b
--     }
