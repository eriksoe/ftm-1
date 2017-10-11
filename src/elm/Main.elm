import Model
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Array

--========== Machine environment ====================--
type MachineSpeed = Paused | RunningSlow | RunningFast
type alias MemoryEditState = { addr : Int, value : String }

type alias State = {
  machineState: Model.MachineState,
  speed: MachineSpeed,
  editingMemory: Maybe MemoryEditState
}

type SpeedCmd = Reset | Pause | Step | RunSlow | RunFast
type MemInputCmd = SelectMem Int | UpdateMem Int String
type Msg = SelectSpeed SpeedCmd | UpdateMemory MemInputCmd

---------- Main: ----------------------------------------

init =
  let
    model = {
      machineState = Model.init(),
      speed = Paused,
      editingMemory = Nothing
      }
    cmd = Cmd.none
  in (model,cmd)

update model msg = model

subscriptions model = Sub.none

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

memoryViewRow editingMemory addr value =
  tr [] [
    td [style [("text-align","right")] ] [text ((toString addr)++":")],
    td [] [text (toString value)]
  ]

main =
 Html.App.program {
      init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
