module Model exposing (
  MachineState,
  init, step, resetIP,
  decodeIns, srcRegisterName, destRegisterName,
  fetchOpt
  )

import Array exposing (Array)

--==================== Data types ==============================--
--========== Machine model ====================--

kMEM_SIZE = 25 -- 10000

type alias Datum = Int
type alias Address = Int

type alias Memory = Array Datum
type alias MachineState = {
  error: Maybe {}, -- TODO: define error enum
  memory: Memory,
  ip: Int,
  x:Datum, y:Datum, z:Datum, w:Datum,
  op:ALUOperation,
  a:Datum, lastIP:Datum
}

type ALUOperation =
    AluX | AluY
  | Add | Sub | Mul | Div | Rem
  | Lt | Gt | Lte | Gte | Eq | Neq
  | And | Or | Xor | NotX

--========== I/O Devices ====================--
type alias IODevice = {
  name : String,
  baseAddress : Address,
  ioSpaceSize : Int,
  input : Address -> Datum,
  output : (Address,Datum) -> ()
  --render : () -> Html
}


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

init : () -> MachineState
init() = {
  error = Nothing,
  memory = Array.repeat kMEM_SIZE 0,
  ip = 0,
  x = 0, y = 0, z = 0, w = 0,
  op = AluX, a = 0, lastIP = 0}

-- Reset IP and clear error flag.
resetIP state =
  {state | ip=0, error=Nothing}

step : MachineState -> MachineState
step state =
     case state.error of
       Nothing -> doStep state
       Just _ -> state -- No change, machine is halted.

doStep state =
     let (ins,state2) = fetchIns state
         (lit,src,dest) = decodeIns ins
         d = fetchPhase (state2,src,lit)
         state3 = storePhase (state2,dest,d)
     in state3

fetchIns state =
   let ip = state.ip
       ins = fetch(state, state.ip)
       newIP = ip+1
   in (ins, {state| ip=newIP})

decodeIns ins =
  let lit = ins // 100
      rest = abs(ins % 100)
      src = rest // 10
      dest = rest % 10
  in (lit,src,dest)

fetchPhase(state, src, lit) =
 case src of
   0 -> lit
   1 -> state.x
   2 -> state.y
   3 -> aluResult(state)
   4 -> state.z
   5 -> state.w
   6 -> state.a
   7 -> fetch(state, state.a)
   8 -> state.ip
   9 -> state.lastIP
   _ -> 0 -- TODO: Internal error.

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
  Maybe.withDefault 0 (Array.get addr state.memory)

fetchOpt(state, addr) =
  Array.get addr state.memory

store(state, addr, value) =
  {state | memory=Array.set addr value state.memory}

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
