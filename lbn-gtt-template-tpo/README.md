# Track Purchase Orders Template App

## Description
Track Purchase Orders template app is for purchase representatives to monitor the purchase order fulfillment status when the procuring party manages the delivery transportation. The template app answers the following questions: </br>
* How many deliveries in my purchase order are delayed?
* How many deliveries in my purchase order are completed?
* Where are my purchase orders?
* ……  

## Requirements
* An SAP Cloud Platform global account with entitlement to the global track and trace option for SAP Logistics Business Network, 1 portal service quota and 2 GB Application Runtime quota
* To integrate with ERP, an SAP ERP or SAP ECC system running on Netweaver 7.31 or higher with SAP NOTE 2937175 being implemented
* The ABAP codes to support sample applications for GTT v2 can be implemented in S4 HANA 2101 on premise, which is not validated in lower release
* To integrate with visibility providers, log your incident in SAP BCP system with component “SCM-LBN-GTT-COR”</br>

## Download and Installation
* 01_Implementation_Guide-TPO </br>
* 02_Extractor_Creation_Guide-TPO </br>

## Limitations
* The eventMatchKey of the shipment’s planned event = shipmentNo + stopId. "stopId" is set by the stage’s sequence.</br>
* The eventMatchKey of the purchase order item's and delivery item's planned event is null.</br>
* To integrate with visibility providers, the following code list sent out from ERP system should be consistent with the code list in the template model of Track Purchase Orders – "Purchase Order fulfillment":</br>
Transportation mode code </br>
Shipping type code </br>
Tracked resource code </br>
Carrier reference document type code </br>

## Known Issue
* If multiple IDOC payloads are generated at the same time or in a very short time in ERP, these payloads will enter the GTT system in disorder. This will cause update failure in some situations. The situation has been improved by asynchronous processing of core engine within the GTT system. But it still might happen when the IDOCs fall into different processing queues.</br>
* For ERP, the EXTENSION segment of processing IDOC is not enabled for planned events. This means that you cannot set up user-defined fields for planned events in the Manage Models app. But you can use Control Parameter’s segment in IDOC and do the field mapping on the tracked process level in the Manage Models app.</br>

## FAQs
* Why couldn’t my shipment events be correlated with the delivery and then with the delivery item?</br>
The correlation starts 90 minutes before the planned departure time of the first stop of delivery-assigned shipment stages, and ends when the last stop’s POD or ARRIVAL of delivery-assigned shipment stages is reported. (If the delivery item is POD relevant, it ends when POD is reported. Otherwise, it ends when ARRIVAL is reported.)</br>
Only during the correlation period can the shipment events be correlated with the delivery and then with the delivery item.</br>
You can set your own correlation period in Event-to-Action by updating “validFrom” and “validTo” logic.</br>

* Why can’t I find any stops and any planned routes on the map?</br>
Check if you have assigned the delivery to any shipment stages.</br>

* Why are some stops missing on the map?</br>
Check if you have updated the right geocoordinates for those locations in the Manage Locations application.</br>

* Why are some actual events missing on the map?</br>
Only events with valid geocoordinates are shown on the map.</br>

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
If your issue is concerned with the global track and trace option, log your incident in SAP BCP system with component “SCM-LBN-GTT-COR”.</br>
