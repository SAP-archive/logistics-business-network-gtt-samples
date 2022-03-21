# Track Shipments Template App

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
* An SAP Business Technology Platform global account with entitlement to the global track and trace option for SAP Logistics Business Network, 1 portal service quota and 2 GB Application Runtime quota.
* To integrate the global track and trace option with ERP, your SAP ERP system should be running on Netweaver 7.31 or higher with SAP NOTE 2937175 being implemented. Besides, to implement the template apps' extractor codes, your SAP ERP system version should be S4 1909 SP03 on premise or higher.
* The node “Interface to Global Track and Trace” in the IMG and the related GTT-specific versions of the IMG activities are in Netweaver 740 only available from SP 18 on, and in Netweaver 750 only from SP 08 on. They cannot be downloaded as a correction via note assistant. We recommend upgrading to the service package level accordingly.
* To integrate with visibility provider, log your incident in SAP BCP system with component “SBN-LBN-GTT-APP”.

## Download and Installation
* [02_Implementation_Guide-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/02_Implementation_Guide-TS.pdf) 
* [03_Synchronize_Actual_Events_To_TM-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/03_Synchronize_Actual_Events_To_TM-TS.pdf) 
* [04_Extractor_Creation_Guide-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/04_Extractor_Creation_Guide-TS.pdf)
* [05_What'sNew(Feb_2021)-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/05_What'sNew(Feb_2021)-TS.pdf)

## Limitations
1.	To integrate with visibility provider, the following code list sent out from ERP system should be consistent with the code list in Track SO Fulfillment template model: </br>
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
If your issue is concerned with global track and trace option, log your incident in SAP BCP system with component “SBN-LBN-GTT-APP”.
