# Track Purchase Orders Template App

## Description
Track Purchase Orders template app is designed for purchase representatives to monitor the purchase order fulfillment status in case that the procuring party manages the delivery transportation. The app mainly answers the following questions:</br>
* How many deliveries in my purchase order are delayed?
* How many deliveries in my purchase order are completed?
* Where are my purchase orders?
* ……  

## Requirements
* An SAP Cloud Platform global account with entitlement to the global track and trace option for SAP Logistics Business Network, 1 portal service quota and 2 GB Application Runtime quota
* To integrate with ERP, an SAP ERP or SAP ECC system running on Netweaver 7.31 or higher with SAP NOTE 2937175 being implemented
* The ABAP codes to support sample applications for GTT v2 can be implemented in S4 HANA 2101 on premise, which is not validated in lower release.
* To integrate with visibility provider, log your incident in SAP BCP system with component “SCM-LBN-GTT-COR”</br>

## Download and Installation
* TBD

## Notes for ERP Extractor Implementation
* The eventMatchKey of the shipment’s planned event = shipmentNo + stopId. "stopId" is set by the stage’s sequence.</br>
* The eventMatchKey of the purchase order item and delivery item's planned event is null.</br>
* To integrate with visibility provider, the following code list sent out from ERP system should be consistent with the code list in Track Purchase Orders template model – Purchase Order fulfillment:</br>
Transportation mode code </br>
Shipping type code </br>
Tracked resource code </br>
Carrier reference document type code </br>

## Known Issue
* If multiple IDOC payloads are generated at the same time or in a very short time gap in ERP, these payloads will reach the global track and trace system disorderly, which might lead to an update failure. This failure frequency has been minimized by asynchronous processing mode of core engine within the global track and trace system. However, it might still come up when the IDOCS are falling into different processing queues.</br>
* By now, on ERP side, the EXTENSION segment of process IDOC is not enabled for the planned event part, which means that user cannot set the user-defined fields based on the planned event level in Manage Models application within the global track and trace system. The workaround is to take use of Control Parameter’s segment in IDOC and make the field mapping on process level in Manage Models application.</br>

## FAQs
* Why couldn’t my shipment events be correlated with the delivery and then with the delivery item?</br>
The correlation starts on 90 minutes before the planned departure time of the delivery-assigned shipment stages’ first stop, and ends when the delivery-assigned shipment stages’ last stop’s POD or ARRIVAL is reported which depends on whether the delivery item is POD relevant or not.</br>
Only within the correlation period the shipment events can be correlated with the delivery and then with the delivery item.</br>
You can set your own correlation period in Event-to-Action by updating “validFrom” and “validTo” logic.</br>

* Why can’t I find any stops and any planned routes in the map?</br>
Check if you have assigned the delivery to any shipment stages.</br>

* Why are some stops missing in the map?</br>
Check if you have updated the right geocoordinates for those locations in the Manage Locations application.</br>

* Why are some actual events missing in the map?</br>
Only events with valid geocoordinates are shown in the map.</br>

* How is the shipment’s execution status changed?</br>
By default, the shipment’s execution status is “Not Started”.</br>
If the shipment’s planned event is reported, the execution status will be changed to “In Transit”.</br>
If the shipment’s all planned PODs and ARRIVALs are reported, the execution status will be changed to “Completed”.</br>
Once the execution status is set as “Completed”, it cannot be changed any more.</br>
You can set your own execution status logic in Event-to-Action.</br>

* How is the delivery’s execution status changed?</br>
By default, the delivery item’ execution status is “Not Started”.</br>
If the delivery item’s planned event or the delivery-assigned shipments’ Check-in, Load Begin, or Load End event is reported, the execution status will be changed to “In Transit”.</br>
If the delivery item’s all planned PODs from the delivery-assigned shipments are reported when the item is POD relevant, or the delivery item’s planned goods receipt quantity are fully reported when the item is not POD relevant, the execution status will be changed to “Completed”. </br>
Once the execution status is set as “Completed”, it cannot be changed any more. </br>
You can set your own execution status logic in Event-to-Action. </br>

## How to Obtain Support
The project is provided "as-is", with no expected support. </br>
If your issue is concerned with global track and trace option, log your incident in SAP BCP system with component “SCM-LBN-GTT-COR”.</br>
