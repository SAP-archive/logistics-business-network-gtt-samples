# Track SO Fulfillment Template App
>:heavy_exclamation_mark:**Important:**
>The sample codes provided here are only for the **template** Track SO Fulfillment app, which is out of mainstream maintenance. If you are using the **standard** app, please download the codes at [Fulfillment Tracking Apps for SAP Business Network Global Track and Trace](https://github.com/SAP-samples/logistics-business-network-gtt-standardapps-samples).
## Announcement (2022.08.20) 
In this release, the following changes are made: 
* The UI5 version for Track SO Fulfillment template app is upgraded to 1.96.10.
* For visibility provider integration, the mapping of the field 'trackid" is removed in the model for Track SO Fulfillment template app.

## Announcement (2022.07.16) 
In this release, the following change is made: 
* The valid from/to segment (1970/9999) is removed from all the Tracking ID functions.

## What's New in Track SO Fulfillment app (Micro Delivery 2021.02.26)
* [04_What's New(Feb_2021)-TSOF.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tso/Documents/04_What'sNew_Feb_2021-TSOF.pdf) 

## Description
Track SO Fulfillment template app is designed for internal sales representatives to monitor the sales order fulfillment status. The app mainly answers the following questions:
* How many deliveries in my sales order are delayed?
* How many deliveries in my sales order are completed?
* Where are my sales orders?
* What is the ETA of my sales orders?
* …… </br>
More details, please refer to </br>
[01_Application_Introduction-TSOF.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tso/Documents/01_Application_Introduction-TSOF.pdf)</br>
![image](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tso/Documents/screenshot.png)

## Requirements
* An SAP Business Technology Platform global account with entitlement to SAP Business Network Global Track and Trace, 1 portal service quota and 2 GB Application Runtime quota.
* Make sure that you have met the requirements for the product version mentioned in the [Prerequisites](https://help.sap.com/docs/business-network-global-track-and-trace/cea0ff17c5ab4c1d96de9ccda35b6a6f/c9f7baf5f6e14be4ba9045786961de14.html) chapter of How to Send Documents from SAP ERP to SAP Business Network Global Track and Trace. You can find this guide at http://help.sap.com/gtt. 
* The ABAP codes on Github to support sample apps for SAP Business Network Global Track and Trace shall be implemented in SAP S/4HANA 1909 SP03 on premise or higher. Please note that the codes are not validated in its lower version or other ECC series of products, so you might need to do further adaptation work or build your own extractor.
* To integrate with visibility providers, log your incident in SAP BCP system with component “SBN-LBN-GTT-APP”.

## Download and Installation
* [02_Implementation_Guide-TSOF.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tso/Documents/02_Implementation_Guide-TSOF.pdf) 
* [03_Extractor_Creation_Guide-TSOF.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tso/Documents/03_Extractor_Creation_Guide-TSOF.pdf) 

## Limitations
* Limitations for Document Flow Implementation: </br>
Recommended number of layers in document flow: ≤6; </br>
For number of layers > 6, please use table view or extend the document flow by click. </br>
Recommended number of nodes in document flow: ≤500; </br>
For number of nodes > 500, please use table view. </br>

* Notes for ERP Extractor Implementation: </br>
The eventMatchKey of the shipment’s planned event = shipmentNo + stopId. "stopId" is set by the stage’s sequence. </br>
The eventMatchKey of the delivery's and delivery item's planned event is null. </br>
To integrate with visibility provider, the following code list sent out from ERP system should be consistent with the code list in Track SO Fulfillment template model: </br>
transportation mode code, shipping type code, tracked process type code, carrier refrence document type code  

## Known Issue
* If multiple IDOC payloads are generated at the same time or in a very short time in ERP, these payloads will enter the GTT system in disorder. This will cause update error in some situations. It is a known issue and is expected to be fixed in the next release.

## FAQs
* Why couldn’t my shipment events be correlated with the delivery and then with the delivery item? </br>
The correlation starts 90 minutes before the planned departure time of the shipment’s first stop and ends when the shipment’s last stop’s POD is reported. </br>
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
If the shipment’s planned event is reported, the execution status will be changed to “In Transit”. </br>
If the shipment’s all planned PODs are reported, the execution status will be changed to “Completed”. </br>
Once the execution status is set as “Completed”, it cannot be changed any more. </br>
You can set your own execution status logic in Event-to-Action. </br>

* How is the delivery’s execution status changed? </br>
By default, the delivery item’ execution status is “Not Started”. </br>
If the delivery item’s planned event is reported, the execution status will be changed to “In Transit”. </br>
If the delivery item’s all planned PODs from shipment are reported or the delivery item’s own planned POD is reported, the execution status will be changed to “Completed”. </br>
Once the execution status is set as “Completed”, it cannot be changed any more. </br>
You can set your own execution status logic in Event-to-Action. </br>

## How to Obtain Support
The project is provided "as-is", with no expected support. </br>
If your issue is concerned with SAP Business Network Global Track and Trace, log your incident in SAP BCP system with component “SBN-LBN-GTT-APP”. 
