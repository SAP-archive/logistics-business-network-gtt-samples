# Sales Order Fulfillment - a GTT template app

## Description
Sales Order Fulfillment is designed for internal sales representative to monitor its sales order fulfillment status, it mainly answers following questions:
* How many deliveries in my sales order are delayed?
* How many are completed?
* Where are my sales orders?
* What is the ETA of my sales orders?
* ……
![image](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-sof-sample/Documents/screenshot.png)


## Requirements
* An SAP Cloud Platform global account with entitlements for SAP LBN Track and Trace option, 1 portal service quota and 2 GB Application Runtime quota.
* To integrate with ERP, you need a S/4, SAP ERP or SAP ECC system running on Netweaver 7.31 or higher with SAP NOTE 2937175 being implemented. 
* To integrate with visibility provider, please log your incident in SAP [BCP]( https://support.wdf.sap.corp/)  system under component “SCM-LBN-GTT-COR”. Currently it will be supported manually.

## Download and Installation
* [Sales Order Fulfillment – Implementation Guide](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/dev/lbn-gtt-sof-sample/Documents/01_Implementation%20Guide%20-%20SOF.pdf)

## Limitation
* Limitation for Document Flow implementation: </br>
Recommended number of layers in document flow ≤6, for number of layers > 6, please use table view or extend the document flow click by click. </br>
Recommended number of nodes in document flow: ≤500, for number of nodes > 500, please use table view. </br>
* Limitation for ERP Extractor implementation:
Shipment’s planned event’ eventMatchKey = shipmentNo+stopId, stopId is set by stage’s sequence.## FAQ. </br>

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

## How to obtain support
The project is provided "as-is", with no expected support. 
If it is an LBN Global Track & Trace option issue, please log your incidents in SAP [BCP](https://support.wdf.sap.corp/) system with component “SCM-LBN-GTT-COR”

## License
Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved. This file is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.   
