module IODeviceList exposing (deviceSpecList, createDeviceList,
  input, output,
  view, handleEvent)

import IODevice exposing (IODevice, IOCmd, IODeviceCmd, Address, Datum)

-- Concrete devices:
import RNGDevice

--import Random exposing (Generator)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.App

deviceSpecList : List IODevice.Metadata
deviceSpecList = [
  RNGDevice.metadata
  ]

createDeviceList : () -> (List IODevice, Cmd IOCmd)
createDeviceList() =
  let (devs, cmds) = List.unzip (List.map IODevice.create deviceSpecList)
  in (devs, Cmd.batch cmds)

init =
  createDeviceList()

handleEvent cmd model =
  applyInputToDevices(model,cmd)

--==================================================--
update cmd model =
  (applyInputToDevices(model,cmd),
   Cmd.none)

applyInputToDevices : (List IODevice, IOCmd) -> List IODevice
applyInputToDevices (devs, cmd) =
  case (devs, cmd) of
    ([], _) -> Debug.log ("Uncaught input command: "++(toString cmd)) []
    (dev::rest, IODevice.IOInputCmd inputCmd) ->
      if dev.metadata.baseAddress == inputCmd.baseAddress
      then IODevice.applyCommand(dev,inputCmd.event)::rest
      else dev::applyInputToDevices(devs,cmd)

input : (List IODevice, Address) -> (List IODevice, Maybe Datum)
input (devs, addr) =
  applyAtAddress inputFromDevice (devs, addr)

inputFromDevice : (IODevice,Address) -> (IODevice.State,Datum)
inputFromDevice(dev, relativeAddr) =
  dev.metadata.input(dev.state, relativeAddr)

output : (List IODevice, Address, Datum) -> List IODevice
output (devs,addr,value) =
  let (devs', _) = applyAtAddress (outputToDevice value) (devs, addr)
  in devs'

outputToDevice : Datum -> (IODevice,Address) -> (IODevice.State,())
outputToDevice value (dev, relativeAddr) =
  (dev.metadata.output(dev.state, relativeAddr, value), ())

applyAtAddress : ((IODevice, Address) -> (IODevice.State, a)) -> (List IODevice,Address) -> (List IODevice, Maybe a)
applyAtAddress func (devs,addr) =
  case devs of
    [] -> ([], Nothing)
    dev::rest ->
      case deviceRelativeAddress(dev, addr) of
        Just relAddr ->
          let (devState', value) = func(dev, relAddr)
              dev' = {dev | state=devState'}
          in (dev'::rest, Just value)
        Nothing ->
          let (rest', valueOpt) = applyAtAddress func (rest, addr)
          in  (dev::rest', valueOpt)

deviceRelativeAddress (dev,addr) =
      if addr >= dev.metadata.baseAddress
      then let relAddr = addr - dev.metadata.baseAddress
           in  if relAddr < dev.metadata.ioSpaceSize
               then Just relAddr
               else Nothing
      else Nothing

--======================================================--

view model =
  Html.div [] (List.map devView model)

devView : IODevice -> Html IOCmd
devView dev =
  Html.div [style [("padding", "0px"), ("margin", "5px"), ("border-color","#999"), ("border-style","outset"), ("background", "#999"), ("color", "black") ]] [
    Html.fieldset [style [("border", "groove"),("border-color","#999"), ("margin", "10px 5px")]] [
      Html.legend [style [("color", "#444")], Html.Attributes.title ("Base address: "++toString dev.metadata.baseAddress)] [
        Html.text dev.metadata.name
      ],
      Html.div [] [IODevice.render dev]
    ]
  ]

subscriptions model =
  Sub.none

main =
  Html.App.program {
    init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
