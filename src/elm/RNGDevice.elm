module RNGDevice exposing (metadata)

-- PURPOSE: I/O device containing an RNG.
-- Address map:
-- Base+0: Read random number (between min and max, inclusive).
-- Base+1: Write range minimum.
-- Base+2: Write range maximum.

import IODevice exposing (Metadata, State, IOCmd, Address, Datum, IODeviceCmd(..))
import Array
import Html exposing (Html)
import Random


metadata : Metadata
metadata = {
  name = "Random Number Generator",
  baseAddress = 1123000,
  ioSpaceSize = 3,

  -- Methods:
  reset = reset,
  input = input,
  output = output,
  render = render
  }

reset : State -> (State, Cmd IODeviceCmd)
reset state =
   let toCmd = \rnd -> IODeviceInput (setSeed rnd)
   in ({state | control=Array.repeat 2 0, data=Array.repeat 0 0},
       Random.generate toCmd (Random.int Random.minInt Random.maxInt))

setSeed : Int -> State -> State
setSeed rnd model =
  {model | seed=Just(Random.initialSeed rnd)}

input : (State, Address) -> (State, Datum)
input (state,addr) =
  generateRandom' state


output : (State, Address, Datum) -> State
output (state, addr, value) =
  if addr>=1 && addr<=2
  then {state | control = Array.set (addr-1) value state.control}
  else state

generateRandom' : State -> (State, Int)
generateRandom' state =
  let min = minValue(state)
      max = maxValue(state)
  in case state.seed of
     Nothing ->
       let _ = Debug.log "Rnd error: seed not set :-(" state.seed
       in (state, min)
     Just seed ->
       if min < max
       then
         let (rnd,newSeed) = Random.step (Random.int min max) seed
         in ({state | seed=Just newSeed}, rnd)
       else
         (state, min)

render : (State) -> Html IODeviceCmd
render model =
  Html.div [] [
    Html.br [] [], Html.text "Min: ", Html.text (toString (minValue model))
  , Html.br [] [], Html.text "Max: ", Html.text (toString (maxValue model))
  --Html.br [] [], Html.text "Seed: ", Html.text (toString model.seed)
  ]

minValue model =
  Array.get 0 model.control |> Maybe.withDefault 0
maxValue model =
  Array.get 1 model.control |> Maybe.withDefault 0
