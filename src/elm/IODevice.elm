module IODevice exposing (
  State, Metadata, IODevice, IOCmd, Address, Datum,
  IODeviceCmd(..), IOCmd(..),
  create, render, applyCommand
  )

import Random
import Array exposing (Array)
import Html exposing (Html)
import Html.App

--========== I/O Devices ====================--
type alias Datum = Int
type alias Address = Int

-- In the absence of existential types, all IODevices need to have the
-- same representation :-(
type alias State = {
     seed : Maybe Random.Seed,
     control: Array Datum,
     data: Array Datum
}

type IODeviceCmd = IODeviceInput (State -> State)
type IOCmd = IOInputCmd {baseAddress:Int, event:IODeviceCmd}

type alias Metadata = {
  name : String,
  baseAddress : Address,
  ioSpaceSize : Int,

  -- Methods:
  reset : State -> (State, Cmd IODeviceCmd),
  input : (State, Address) -> (State, Datum),
  output : (State, Address, Datum) -> State,
  render : (State) -> Html IODeviceCmd
}

type alias IODevice = {metadata: Metadata, state: State}

--========================================--

initialState : () -> State
initialState() =
  {seed = Nothing,
   control = Array.repeat 0 0,
   data = Array.repeat 0 0}

create : Metadata -> (IODevice, Cmd IOCmd)
create md =
  let blankState = initialState()
      (state,cmd) = md.reset blankState
  in ({metadata=md, state=state},
      Cmd.map (wrapEvent md) cmd)

render : IODevice -> Html IOCmd
render dev =
  let html = dev.metadata.render dev.state
  in Html.App.map (wrapEvent dev.metadata) html

wrapEvent : Metadata -> IODeviceCmd -> IOCmd
wrapEvent metadata event =
  IOInputCmd {baseAddress=metadata.baseAddress, event=event}

applyCommand : (IODevice, IODeviceCmd) -> IODevice
applyCommand (device, cmd) =
  case cmd of
    IODeviceInput transformer ->
      {device | state = (transformer device.state)}
