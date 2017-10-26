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
   ({state | control=Array.repeat 2 0}, Cmd.none)

reset2 : State -> (State, Cmd IODeviceCmd)
reset2 state =
   let toCmd = \rnd -> IODeviceInput (setSeed rnd)
   in (state, Random.generate toCmd (Random.int Random.minInt Random.maxInt))

setSeed : Int -> State -> State
setSeed rnd model = model

input : (State, Address) -> Datum
input (model,addr) =
  0 --TODO

output : (State, Address, Datum) -> State
output (state, addr, value) =
  state --TODO

render : (State) -> Html IODeviceCmd
render model =
  Html.div [] [
  Html.text "<RNG device>",
  Html.br [] [], Html.text "Min: ", Html.text (toString (minValue model)),
  Html.br [] [], Html.text "Max: ", Html.text (toString (maxValue model))
  ]

minValue model =
  Array.get 0 model.control |> Maybe.withDefault 0
maxValue model =
  Array.get 1 model.control |> Maybe.withDefault 0
