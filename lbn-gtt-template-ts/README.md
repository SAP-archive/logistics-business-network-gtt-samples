# Track Shipments Template App

## Description
Track Shipments template app is designed for transportation planner to monitor the shipment, road freight order or ocean freight booking execution status and report events manually. The app mainly answers the following questions:
* Where is my shipment/road freight order/ocean freight booking?
* What is the next stop of my shipment/road freight order/ocean freight booking?
* What is the ETA of my shipment/road freight order/ocean freight booking?
* …… </br>
More details, please refer to [01_Application_Introduction-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/01_Application_Introduction-TS.pdf)

## Requirements
* An SAP Cloud Platform global account with entitlement to the global track and trace option for SAP Logistics Business Network, 1 portal service quota and 2 GB Application Runtime quota
* To integrate with ERP, an SAP ERP or SAP ECC system running on Netweaver 7.31 or higher with SAP NOTE 2937175 being implemented 
* To integrate with visibility provider, log your incident in SAP BCP system with component “SCM-LBN-GTT-COR”

## Download and Installation
* [02_Implementation_Guide-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/02_Implementation_Guide-TS.pdf) 
* [03_Synchronize_Actual_Events_To_TM-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/03_Synchronize_Actual_Events_To_TM-TS.pdf) 
* [04_Extractor_Creation_Guide-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/04_Extractor_Creation_Guide-TS.pdf)

## Limitations
1.	To integrate with visibility provider, the following code list sent out from ERP system should be consistent with the code list in Track Sales Orders template model: </br>
transportation mode code, shipping type code, tracked process type code, carrier reference document type code</br>  
2.	To integrate with shipment and freight order/freight booking at the same time, please implement the Track Shipment Extractor and Track Sales Orders Extractor in different ERP system. This is because Track Shipment Extractor and Track Sales Orders Extractor share the same AOT Type name. </br>

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
If your issue is concerned with global track and trace option, log your incident in SAP BCP system with component “SCM-LBN-GTT-COR”.
