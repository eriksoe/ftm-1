module IODevice exposing (State, Metadata, IODevice, IOCmd, create)
import Random
import Array exposing (Array)
import Html exposing (Html)

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
  reset : State -> (State, Cmd IOCmd),
  input : (State, Address) -> Datum,
  output : (State, Address, Datum) -> (),
  render : (State) -> Html IOCmd
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
  in ({metadata=md, state=state}, cmd)
