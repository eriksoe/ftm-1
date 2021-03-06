import Model exposing (MachineState)
import IODevice
import IODeviceList
import CPUView
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
type Msg = SelectSpeed SpeedCmd | MemoryEvent MemInputCmd | IOEvent IODevice.IOCmd

---------- Main: ----------------------------------------

init : (State, Cmd Msg)
init =
  let
    (devices, devCmds) = IODeviceList.createDeviceList()
    model = {
      machineState = Model.init(devices),
      speed = Paused,
      editingMemory = Nothing
      }
    cmd = Cmd.map (IOEvent) devCmds
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
    SelectSpeed Reset ->
      {model | machineState=Model.resetIP(model.machineState)}
    IOEvent evt -> handleIOEvent(model,evt)
    other -> model
   , Cmd.none)

handleIOEvent : (State,IODevice.IOCmd) -> State
handleIOEvent (state,evt) =
  let devices' = IODeviceList.handleEvent evt state.machineState.devices
      machineState0 = state.machineState
      machineState' = {machineState0 | devices=devices'}
  in {state | machineState=machineState'}

validateDatum : String -> Maybe Int
validateDatum s =
  case String.toInt s of
    Ok value -> Just value
    Err reason -> Nothing

subscriptions : State -> Sub Msg
subscriptions model = Sub.none

type alias ExtraMemoryViewState = {editingState: Maybe MemoryEditState, ip: Int, a: Int}

view : State -> Html Msg
view model = body [
       style []
     ] [
     div [style [("overflow","auto"), ("backgroundColor", "#333")]] [
     div [style [("float","left")]] [
       div [style [("padding", "0px"), ("margin", "5px"), ("border-color","#999"), ("border-style","outset"), ("background-color", "#999"), ("color", "black") ]] [
         fieldset [style [("margin","5px"),("border", "groove"),("border-color","#999")]] [
           legend [style [("color", "#444")]] [text "Control"],
           button [onClick (SelectSpeed Reset)] [text "Reset"],
           button [onClick (SelectSpeed Step)] [text "Step"]
         ],
         CPUView.cpuView model.machineState
       ],
       div [style [("padding", "0px"), ("margin", "5px"), ("border-color","#999"), ("border-style","outset"), ("background-color", "#999"), ("color", "black") ]] [
         memoryView model.machineState.memory {editingState=model.editingMemory, ip=model.machineState.ip, a=model.machineState.a}
       ]
     ],
     div [style [("float","left"),("width","60%")]] [ -- [("margin-left", "15px")]] [
       Html.App.map wrapIOEvent (IODeviceList.view model.machineState.devices)
     ]
     ]
     ]

wrapIOEvent cmd =
 IOEvent cmd

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

memoryViewRow : ExtraMemoryViewState -> Int -> Int -> Html Msg
memoryViewRow state memAddr memValue =
  tr [] [
    td [] (memMarkingA(memAddr, state)),
    td [] (memMarkingIP(memAddr, state)),
    td [style [("text-align","right")] ] [text ((toString memAddr)++":")],
    td [] (memoryValueEditCell memAddr memValue state),
    td [] [text (disassembleNumber memValue)]
  ]

memMarkingIP : (Int, ExtraMemoryViewState) -> List (Html Msg)
memMarkingIP(memAddr, state) =
  if memAddr == state.ip
  then [span [style [("color", "#6f6")]] [text ipArrow]]
  else []

memMarkingA : (Int, ExtraMemoryViewState) -> List (Html Msg)
memMarkingA(memAddr, state) =
  if memAddr == state.a
  then [span [style [("color", "#fc6")]] [text ipArrow]]
  else []

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
        size 10,
        value curValue,
        onFocus (MemoryEvent(UpdateMemEdit memAddr (toString memValue))),
        onClick (MemoryEvent(UpdateMemEdit memAddr (toString memValue))),
        onInput (\v->MemoryEvent(UpdateMemEdit memAddr v)),
        onBlur (MemoryEvent(UpdateMem memAddr curValue))
      ] []
     ]

disassembleNumber : Int -> String
disassembleNumber nr =
  let (lit,src,dest) = Model.decodeIns nr
      srcStr = if src==0 then (toString lit) else Model.srcRegisterName src
      destStr = Model.destRegisterName dest
  in srcStr++">"++destStr

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
