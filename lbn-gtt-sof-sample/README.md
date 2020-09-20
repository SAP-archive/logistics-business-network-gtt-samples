# Sales Order Fulfillment app

## Limitation
* Limitation for Document Flow implementation:<br>
Recommended number of layers in document flow ≤6, for number of layers > 6, please use table view or extend the document flow click by click <br>
Recommended number of nodes in document flow: ≤500, for number of nodes > 500, please use table view <br>

* Limitation for ERP Extractor implementation: <br>
Shipment’s planned event’ eventMatchKey = shipmentNo+stopId, stopId is set by stage’s sequence. <br>

## FAQs

* Why my shipment events could not be correlated to the delivery and then to delivery item? <br>
The correlation period starts 90 minutes before the shipment’s first stop’ planned departure date time and ends when the shipment’s last stop’ POD has been reported. Only in the correlation period, the shipment events will be correlated to the delivery and then to delivery item.
You can set your own correlation period in Event-to-Action by updating “validFrom” and “validTo” logic.

* Why I cannot find any stops and any planned routes in the map? <br>
You haven’t assigned the delivery to any shipments, or the shipments don’t have any stages.

* Why some stops are missing in the map? <br>
You haven’t maintained the right geocoordinates for those locations in the Manage Location’s app.

* Why some actual events are missing in the map? <br>
Only events with valid geocoordinates will be shown in the map.

* How the shipment’s execution status is changed? <br>
By default, the shipment’s execution status is Not Started; if the shipment’s planned event has been reported, the execution status changed to “In Transit”; If shipment’s all planned POD has been reported, the execution status changed to “Executed”. Once the execution status is set to “Executed”, then it cannot be changed any more.
You can set your own execution status logic in Event-to-Action.

* How the delivery’s execution status is changed? <br>
By default, the delivery item’ execution status is Not Started; if the delivery item’s planned event has been reported, the execution status changed to “In Transit”; If delivery item’s all planned POD from shipment has been reported or the delivery item’s own planned POD has been reported, the execution status changed to “Executed”. Once the execution status is set to “Executed”, then it cannot be changed any more.
You can set your own execution status logic in Event-to-Action.



## License
Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved. This file is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.   
