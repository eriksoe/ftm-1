module CPUView exposing (cpuView)
import Model
import Svg exposing (Svg, svg, use, text', text)
import Svg.Attributes exposing (..)

cpuView state =
   svg [
     viewBox "0 0 600 500",
     width "480",
     height "400"
  ] [
      use [xlinkHref "cpu-parts.svg#background"] [],
      use [xlinkHref "cpu-parts.svg#data-path"] [],

      use [xlinkHref "cpu-parts.svg#reg-K"] [],
      use [xlinkHref "cpu-parts.svg#reg-X"] [], regContents(80,180, state.x),
      use [xlinkHref "cpu-parts.svg#reg-Y"] [], regContents(200,180, state.y),
      use [xlinkHref "cpu-parts.svg#reg-Z"] [], regContents(400,180, state.z),
      use [xlinkHref "cpu-parts.svg#reg-W"] [], regContents(520,180, state.w),

      use [xlinkHref "cpu-parts.svg#ALU"] [],
      use [xlinkHref "cpu-parts.svg#path-C"] [],
      use [xlinkHref "cpu-parts.svg#path-R"] [],

      use [xlinkHref "cpu-parts.svg#reg-A"] [], regContents(200,340, state.a),
      use [xlinkHref "cpu-parts.svg#reg-M"] [], regContentsStr(270,410,
           Model.memPeek(state, state.a) |> Maybe.map toString |> Maybe.withDefault "??"),

      use [xlinkHref "cpu-parts.svg#reg-IP"] [], regContents(490,340, state.ip),
      use [xlinkHref "cpu-parts.svg#path-J"] [],
      use [xlinkHref "cpu-parts.svg#reg-L"] [], regContents(370,340, state.lastIP),

      use [xlinkHref "cpu-parts.svg#dummy"] []
  ]

regContents : (Int,Int,Int) -> Svg a
regContents (x0,y0, value) =
  regContentsStr(x0, y0, toString value)

regContentsStr : (Int,Int,String) -> Svg a
regContentsStr(x0,y0, s) =
  text' [
      x (px x0),
      y (px y0),
      fill "#000", stroke "none",
      textAnchor "middle", dominantBaseline "middle",
      fontSize "25px", fontFamily "Bitstream Vera Sans"
  ] [text s]

px v = (toString v)++"px"

--main = cpuView ()

