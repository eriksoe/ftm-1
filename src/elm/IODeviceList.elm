module IODeviceList exposing (deviceSpecList, createDeviceList)

import IODevice exposing (IODevice, IOCmd, IODeviceCmd)

-- Concrete devices:
import RNGDevice

--import Random exposing (Generator)
import Html exposing (Html)
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

view model =
  Html.div [] (List.map devView model)

devView : IODevice -> Html IOCmd
devView dev =
  Html.div [] [
    Html.div [] [Html.text ("Device "++toString dev.metadata.name)],
    Html.div [] [IODevice.render dev]
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
