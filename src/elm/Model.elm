module Model exposing (
  MachineState,
  init, step, resetIP,
  decodeIns, srcRegisterName, destRegisterName,
  memPeek
  )

import Array exposing (Array)
import IODevice
import IODeviceList

--==================== Data types ==============================--
--========== Machine model ====================--

kMEM_SIZE = 25 -- 10000
kIO_BASE = 1000000

type alias Datum = Int
type alias Address = Int

type alias Memory = Array Datum
type alias MachineState = {
  error: Maybe {}, -- TODO: define error enum
  memory: Memory,
  ip: Int,
  x:Datum, y:Datum, z:Datum, w:Datum,
  op:ALUOperation,
  a:Datum, lastIP:Datum,

  devices : List IODevice.IODevice
}

type ALUOperation =
    AluX | AluY
  | Add | Sub | Mul | Div | Rem
  | Lt | Gt | Lte | Gte | Eq | Neq
  | And | Or | Xor | NotX


--==================== Encoding ==============================--

decodeAluOp v =
  case v of
  0 -> Just AluX
  1 -> Just AluY
  2 -> Just Add
  3 -> Just Sub
  4 -> Just Mul
  5 -> Just Div
  6 -> Just Rem
  10 -> Just Lt
  11 -> Just Gt
  12 -> Just Lte
  13 -> Just Gte
  14 -> Just Eq
  15 -> Just Neq
  16 -> Just And
  17 -> Just Or
  18 -> Just Xor
  19 -> Just NotX
  _ -> Nothing

--==================== Execution ==============================--

init : (List IODevice.IODevice) -> MachineState
init(devices) = {
  error = Nothing,
  memory = Array.repeat kMEM_SIZE 0,
  ip = 0,
  x = 0, y = 0, z = 0, w = 0,
  op = AluX, a = 0, lastIP = 0,
  devices = devices}

-- Reset IP and clear error flag.
resetIP state =
  {state | ip=0, error=Nothing} -- TODO: Clear IO devices?

step : MachineState -> MachineState
step state =
     case state.error of
       Nothing -> doStep state
       Just _ -> state -- No change, machine is halted.

doStep state =
     let (ins,state2) = fetchIns state
         (lit,src,dest) = decodeIns ins
         (d,devices') = fetchPhase (state2,src,lit)
         state3 = {state2 | devices=devices'}
         state4 = storePhase (state3,dest,d)
     in state4

fetchIns state =
   let ip = state.ip
       (ins,devices') = fetch(state, state.ip)
       newIP = ip+1
   in (ins, {state| ip=newIP, devices=devices'})

decodeIns ins =
  let lit = ins // 100
      rest = abs(ins % 100)
      src = rest // 10
      dest = rest % 10
  in (lit,src,dest)

fetchPhase(state, src, lit) =
 case src of
   0 -> (lit,     state.devices)
   1 -> (state.x, state.devices)
   2 -> (state.y, state.devices)
   3 -> (aluResult(state), state.devices)
   4 -> (state.z, state.devices)
   5 -> (state.w, state.devices)
   6 -> (state.a, state.devices)
   7 -> fetch(state, state.a)
   8 -> (state.ip, state.devices)
   9 -> (state.lastIP, state.devices)
   _ -> (0, state.devices) -- TODO: Internal error.

storePhase(state, dest, d) =
  case dest of
    0 -> {state | error=Just {}} -- Error!
    1 -> {state | x=d}
    2 -> {state | y=d}
    3 -> case decodeAluOp(d) of
           Just op -> {state | op=op}
           Nothing -> {state | error=Just {}}
    4 -> {state | z=d}
    5 -> {state | w=d}
    6 -> {state | a=d}
    7 -> store(state, state.a, d)
    8 -> goto(state,d)
    9 -> if i2b(aluResult(state))
         then goto(state,d) -- Jump
         else state         -- No jump
    _ -> state -- TODO: Internal error.

goto(state,newIP) =
  {state | ip=newIP, lastIP=state.ip}

aluResult(state) =
  let x=state.x
      y=state.y
  in case state.op of
     AluX -> x
     AluY -> y
     Add  -> x + y
     Sub  -> x - y
     Mul  -> x * y
     Div  -> x // y
     Rem  -> x % y
     Lt   -> b2i (x < y)
     Gt   -> b2i (x > y)
     Lte  -> b2i (x <= y)
     Gte  -> b2i (x >= y)
     Eq   -> b2i (x == y)
     Neq  -> b2i (x /= y)
     And  -> b2i ((i2b x) && (i2b y))
     Or   -> b2i ((i2b x) || (i2b y))
     Xor  -> b2i (xor (i2b x) (i2b y))
     NotX -> b2i (not (i2b x))

b2i x = if x then 1 else 0
i2b x = x /= 0
i2b2(x,y) = (i2b x, i2b y)

--========== Memory access ====================--

fetch(state, addr) =
  if addr < kIO_BASE
  then (Array.get addr state.memory |> Maybe.withDefault 0, state.devices)
  else let (devices', value) = IODeviceList.input(state.devices, addr)
       in (value |> Maybe.withDefault 0, devices')

memPeek(state, addr) =
  if addr < kIO_BASE
  then Array.get addr state.memory
  else Nothing

store(state, addr, value) =
  if addr < kIO_BASE
  then {state | memory=Array.set addr value state.memory}
  else {state | devices=IODeviceList.output(state.devices, addr, value)}

--========== Utilities for external use ====================--
srcRegisterName : Int -> String
srcRegisterName x =
  case x of
   0 -> "K"
   1 -> "X"
   2 -> "Y"
   3 -> "R"
   4 -> "Z"
   5 -> "W"
   6 -> "A"
   7 -> "M"
   8 -> "IP"
   9 -> "L"
   _ -> "??"

destRegisterName : Int -> String
destRegisterName x =
  case x of
   0 -> "K" -- (Invalid)
   1 -> "X"
   2 -> "Y"
   3 -> "C" -- Different from srcRegisterName
   4 -> "Z"
   5 -> "W"
   6 -> "A"
   7 -> "M"
   8 -> "IP"
   9 -> "J" -- Different from srcRegisterName
   _ -> "??"
