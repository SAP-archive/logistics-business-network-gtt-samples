# Track Shipments Template App

## Announcement (2022.08.20) 
In this release, the following changes are made: 
* The UI5 version for Track Shipments template app is upgraded to 1.96.10.
* For visibility provider integration, the mapping of the field 'trackid" is removed in the model.

## Announcement (2022.07.16) 
In this release, the following changes are made: 
* The valid from/to segment (1970/9999) is removed from all the Tracking ID functions.
* The key specification "SOURCE_STOP_KEY" is enhanced.

## What's New in Track Shipments app (Micro Delivery 2021.03.21)
* Enable air tracking mode with integration of visibility provider and TM (air freight booking) in the Track Shipments template app
* Enhance forwardEventtoTM event-to-action script with actual event’s message type in Track Shipments template app.

## What's New in Track Shipments app (Micro Delivery 2021.02.26)
* [04_What's New(Feb_2021)-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/04_What'sNew_Feb_2021-TS.pdf) 
## Description
Track Shipments template app is designed for transportation planner to monitor the shipment, road freight order or ocean freight booking execution status and report events manually. The app mainly answers the following questions:
* Where is my shipment/road freight order/ocean freight booking?
* What is the next stop of my shipment/road freight order/ocean freight booking?
* What is the ETA of my shipment/road freight order/ocean freight booking?
* …… </br>
More details, please refer to </br>
[01_Application_Introduction-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/01_Application_Introduction-TS.pdf)

## Requirements
* An SAP Business Technology Platform global account with entitlement to the SAP Business Network Global Track and Trace, 1 portal service quota and 2 GB Application Runtime quota.
* Make sure that you have met the requirements for the product version mentioned in the [Prerequisites](https://help.sap.com/docs/SAP_LBN_GTT_OPTION/d0802f41861a4f81a3610d873fdcf148/c9f7baf5f6e14be4ba9045786961de14.html) section of Appendix one: Connect to SAP ERP in Administration Guide for Version 2. You can find this guide at http://help.sap.com/gtt. 
* The ABAP codes on Github to support sample apps for SAP Business Network Global Track and Trace shall be implemented in SAP S/4HANA 1909 SP03 on premise or higher. Please note that the codes are not validated in its lower version or other ECC series of products, so you might need to do further adaptation work or build your own extractor.
* To integrate with visibility providers, log your incident in SAP BCP system with component “SBN-LBN-GTT-APP”.

## Download and Installation
* [02_Implementation_Guide-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/02_Implementation_Guide-TS.pdf) 
* [03_Synchronize_Actual_Events_To_TM-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/03_Synchronize_Actual_Events_To_TM-TS.pdf) 
* [04_Extractor_Creation_Guide-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/04_Extractor_Creation_Guide-TS.pdf)
* [05_What'sNew(Feb_2021)-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/05_What'sNew(Feb_2021)-TS.pdf)

## Limitations
1.	To integrate with visibility providers, the following code list sent out from ERP system should be consistent with the code list in Track SO Fulfillment template model: </br>
transportation mode code, shipping type code, tracked process type code, carrier reference document type code</br>  
2.	To integrate with shipment and freight order/freight booking at the same time, please implement the Track Shipment Extractor and Track SO Fulfillment Extractor in different ERP system. This is because Track Shipment Extractor and Track SO Fulfillment Extractor share the same AOT Type name. </br>

## FAQs
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

## How to Obtain Support
The project is provided "as-is", with no expected support. </br>
If your issue is concerned with SAP Business Network Global Track and Trace, log your incident in SAP BCP system with component “SBN-LBN-GTT-APP”.
