# Sales Order Fulfillment - a gloabl track and trace template app

## Description
Sales Order Fulfillment is designed for internal sales representative to monitor its sales order fulfillment status, it mainly answers following questions:
* How many deliveries in my sales order are delayed?
* How many are completed?
* Where are my sales orders?
* What is the ETA of my sales orders?
* ……
![image](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-sof-sample/Documents/screenshot.png)

## Requirements
* An SAP Cloud Platform global account with entitlement to the global track and trace option for SAP Logistics Business Network, 1 portal service quota and 2 GB Application Runtime quota. 
* To integrate with ERP, an SAP ERP or SAP ECC system running on Netweaver 7.31 or higher with SAP NOTE 2937175 being implemented. 
* To integrate with visibility provider, log your incidents in SAP [BCP](https://support.wdf.sap.corp/) system with component “SCM-LBN-GTT-COR”. 

## Download and Installation
* [Sales Order Fulfillment – Implementation Guide](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/dev/lbn-gtt-sof-sample/Documents/01_Implementation%20Guide%20-%20SOF.pdf)

## Limitation
* Limitation for Document Flow implementation: </br>
Recommended number of layers in document flow: ≤6; </br>
For number of layers > 6, please use table view or extend the document flow by clicking. </br>
Recommended number of nodes in document flow: ≤500; </br>
For number of nodes > 500, please use table view. </br>

* Limitation for ERP Extractor implementation:
Shipment’s planned event’ eventMatchKey = shipmentNo+stopId, stopId is set by stage’s sequence. </br>

## Known Issue
* If multiple iDoc payloads are generated at the same time or in a very short time in ERP, those payload might enter global track and trace system in a disorder way. This might cause update error for some tracked processes. It is a known issue and is expected to fix with following global track and trace release.

## FAQs
* Why couldn’t my shipment events be correlated with the delivery and then with the delivery item? </br>
The correlation period starts from 90 minutes before the shipment’s first stop’s planned departure time to the time when the shipment’s last stop’s POD is reported. </br>
Only during the correlation period can the shipment events be correlated with the delivery and then with the delivery item. </br>
You can set your own correlation period in Event-to-Action by updating “validFrom” and “validTo” logic. </br>

* Why can’t I find any stops and any planned routes in the map? </br>
Check if you have assigned the delivery to any shipments. Or the shipments do not have any stages. </br>
 
* Why are some stops missing in the map? </br>
Check if you have updated the right geocoordinates for those locations in the Manage Locations app. </br>

* Why are some actual events missing in the map? </br>
Only events with valid geocoordinates are shown in the map. </br>

* How is the shipment’s execution status changed? </br>
By default, the shipment’s execution status is “Not Started”. </br>
If the shipment’s planned event is reported, the execution status will change to “In Transit”. </br>
If the shipment’s all planned PODs are reported, the execution status will change to “Completed”. </br>
Once the execution status is set as “Completed”, it cannot be changed any more. </br>
You can set your own execution status logic in Event-to-Action. </br>

* How is the delivery’s execution status changed? </br>
By default, the delivery item’ execution status is “Not Started”. </br>
If the delivery item’s planned event is reported, the execution status will change to “In Transit”. </br>
If the delivery item’s all planned PODs from shipment are reported or the delivery item’s own planned POD is reported, the execution status will change to “Completed”. </br>
Once the execution status is set as “Completed”, it cannot be changed any more. </br>
You can set your own execution status logic in Event-to-Action. </br>

## How to obtain support
The project is provided "as-is", with no further support. </br>
If your issue is concerned with global track and trace option, log your incident in SAP [BCP]( https://support.wdf.sap.corp/) system with component “SCM-LBN-GTT-COR”. 

## License
Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved. This file is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.   
