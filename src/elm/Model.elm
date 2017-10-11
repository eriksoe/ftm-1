module Model exposing (MachineState, init)

import Array exposing (Array)

--========== Machine model ====================--

kMEM_SIZE = 25 -- 10000

type alias Datum = Int
type alias Address = Int

type alias Memory = Array Datum
type alias MachineState = {
  memory: Memory,
  ip: Int,
  x:Datum, y:Datum, z:Datum, w:Datum,
  op:Int, -- TODO: other repr.
  a:Datum, lastIP:Datum
}

--========== I/O Devices ====================--
type alias IODevice = {
  name : String,
  baseAddress : Address,
  ioSpaceSize : Int,
  input : Address -> Datum,
  output : (Address,Datum) -> ()
  --render : () -> Html
}


init() = {
  memory = Array.set 3 42 (Array.repeat kMEM_SIZE 0),
  ip = 0,
  x = 0, y = 0, z = 0, w = 0,
  op = 0, a = 0, lastIP = 0}