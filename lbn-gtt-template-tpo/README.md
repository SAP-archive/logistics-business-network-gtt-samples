# Track PO Fulfillment Template App
>:heavy_exclamation_mark:**Important:**
>The sample codes provided here are only for the **template** Track PO Fulfillment app, which is out of mainstream maintenance. If you are using the **standard** app, please download the codes at [Fulfillment Tracking Apps for SAP Business Network Global Track and Trace](https://github.com/SAP-samples/logistics-business-network-gtt-standardapps-samples).
## Announcement (2022.08.20) 
In this release, the following changes are made: 
* The UI5 version for Track PO Fulfillment template app is upgraded to 1.96.10.
* For visibility provider integration, the mapping of the field 'trackid" is removed in the model for Track PO Fulfillment template app.

## Announcement (2022.07.16) 
In this release, the following change is made: 
* The valid from/to segment (1970/9999) is removed from all the Tracking ID functions.

## What's New in the Track PO Fulfillment Template App (Micro Delivery 2021.03.21)
* Enable Air Tracking mode with integration of visibility provider
* Enable Quick Refresh feature in the Delivery Item page
* Enhance the calculation part, such as filtering by Delay Status, showing fields of 'Delay Impact', 'Completed and Late Value', 'Revised / Initial Planned Delivery Date', 'Planned vs Actual Arrival Time'
* Refine exception texts
* Enhance code quality
* Fix bugs
* Enhance documentation.</br>

For more information, refer to What’s New in the relevant document folder [04_What's_New_TPOF_March2021.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tpo/Documents/04_What's_New_TPOF_March2021.pdf) </br>

## What's New in the Track PO Fulfillment Template App (Micro Delivery 2021.02.26)
*	Refactor the folder path ‘/lbn-gtt-template-tpo/ABAP/src/’ to '/lbn-gtt-template-tpo/abap/zsrc/' to adapt to the newest version of ABAPGIT tool.
*	Rename the template app Track Purchase Orders to Track PO Fulfillment.
*	Adapt extract & model logic for the Visibility Provider integration technical change.
*	Add actual technical timestamp and actual technical time zone for all AOT and event extractors to minimize IDOC sequencing issues.
* Enable correlation period to be controlled by the Event-to-Action script to manage event correlating to delivery based on events at the stop level of shipment.
* Enhance UI consistency.
* Fix issues of variant management on the List page.
* Improve the performance for asynchronous processing mode of core engine.
* Enhance the calculation part, such as Last Reported Event & Execution Status & Completion Rate.
* Enable milestones and map interaction on the Delivery Item page.
* Enable new filters of 'Delivery number' & 'Shipment number' on the List page.
* Enable tracking for Goods Receipt at the delivery header level instead of item level.
* Enable features for new model field type of one-to-many association: purchase order – purchase order items, purchase order item – delivery items, delivery – delivery items.
* Enable error logging on Cross TP Update BADI.
* Minor enhancement for the Delivery Item page.
*	Add the requirement “To implement the extractor code, your ERP system version should be S4 1909 SP03 or higher”.
* Include combined IDOC implementation guide into the document 02_Extractor_Creation_Guide-TPOF.pdf.
* Include sample extractor configuration list into the document 02_Extractor_Creation_Guide-TPOF.pdf.
* Include new introduction material [03_Introduction_Material_for_TPOF.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tpo/Documents/03_Introduction_Material_for_TPOF.pdf).
* Include new FAQ material [05_FAQs_for_Template_Code_Implementation.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tpo/Documents/05_FAQs_for_Template_Code_Implementation.pdf).</br>

If you choose not to upgrade to the latest version, here are some mandatory steps for you to adapt to SAP Business Network Global Track and Trace V2 February 2021 Delivery.</br>
*	[Mandatory for VP integration] Add LBN# as the prefix for Service Agent LBN ID for the shipment extractor.
*	[Mandatory for receiving actual events from data contributor] Grant report and read authorization to Service Agent LBN ID in the shipment and resource process type of the model 'Purchase Order Fulfillment'.</br>

For more information, refer to What’s New in the relevant document folder [04_What's_New_TPOF_Feb2021.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tpo/Documents/04_What's_New_TPOF_Feb2021.pdf) </br>


## Description
Track PO Fulfillment template app is designed for purchase representatives to monitor the purchase order fulfillment status when the procuring party manages the delivery transportation. The app answers the following questions:
* How many deliveries in my purchase order are delayed?
* How many deliveries in my purchase order are completed?
* Where are my purchase orders?
* What is the ETA of my purchase orders?
* ……
![image](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tpo/Documents/screenshot.png)

## Requirements
* An SAP Business Technology Platform global account with entitlement to SAP Business Network Global Track and Trace, 1 portal service quota and 2 GB Application Runtime quota.
* Make sure that you have met the requirements for the product version mentioned in the [Prerequisites](https://help.sap.com/docs/business-network-global-track-and-trace/cea0ff17c5ab4c1d96de9ccda35b6a6f/c9f7baf5f6e14be4ba9045786961de14.html) chapter of How to Send Documents from SAP S/4HANA to SAP Business Network Global Track and Trace. You can find this guide at http://help.sap.com/gtt. 
* The ABAP codes on Github to support sample apps for SAP Business Network Global Track and Trace shall be implemented in SAP S/4HANA 1909 SP03 on premise or higher. Please note that the codes are not validated in its lower version or other ECC series of products, so you might need to do further adaptation work or build your own extractor.
* To integrate with visibility providers, log your incident in SAP BCP system with component “SBN-LBN-GTT-APP”.

## Download and Installation
* [01_Implementation_Guide-TPOF.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tpo/Documents/01_Implementation_Guide-TPOF.pdf) 
* [02_Extractor_Creation_Guide-TPOF.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tpo/Documents/02_Extractor_Creation_Guide-TPOF.pdf)

## Limitations
* Notes for ERP Extractor Implementation:</br>
The eventMatchKey of the shipment’s planned event at the stage level = shipmentNo + stopId. "stopId" is set by the stage’s sequence.</br>
The eventMatchKey of the purchase order item's and delivery item's planned event is null.</br>
To integrate with the Manage Locations app on the platform of SAP Business Network for Logistics, the following code list sent out from ERP system should be consistent with the code list in the template model of Track PO Fulfillment – "Purchase Order fulfillment":</br>
  -- Location type code</br>
To integrate with Visibility Providers, the following code list sent out from ERP system should be consistent with the code list in the template model of Track PO Fulfillment – "Purchase Order fulfillment":</br>
  -- Transportation mode code</br>
  -- Shipping type code</br>
  -- Tracked resource type code</br>
  -- Carrier reference document type code</br>

## Known Issue
* If multiple IDOC payloads are generated at the same time or in a very short time in ERP, these payloads will enter the GTT system in an incorrect order. This will cause update failure in some situations. The situation has been improved by asynchronous processing of core engine within the GTT system. But it still might happen when the IDOCs fall into different processing queues.
* For ERP, the EXTENSION segment of processing IDOC is not enabled for planned events. This means that you cannot set up user-defined fields for planned events in the Manage Models app. But you can use Control Parameter’s segment in the tracked process IDOC and do the field mapping at the tracked process level in the Manage Models app.

## FAQs
* Why couldn’t my shipment events be correlated with the delivery and then with the delivery item?</br>
The correlation starts 90 minutes before the planned departure time of the first stop of delivery-assigned shipment stages, and ends when the last stop’s POD or ARRIVAL of delivery-assigned shipment stages is reported. (If the delivery item is POD relevant, it ends when POD is reported. Otherwise, it ends when ARRIVAL is reported.)</br>
Only during the correlation period can the shipment events be correlated with the delivery and then with the delivery item.</br>
You can set your own correlation period in Event-to-Action by updating “validFrom” and “validTo” logic.</br>

* Why can’t I find any stops and any planned routes on the map?</br>
Check if you have assigned the delivery to any shipment's stages.</br>

* Why are some stops missing on the map?</br>
Check if you have updated the right geocoordinates for those locations in the Manage Locations app.</br>

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
If the delivery item’s planned event or the delivery-assigned shipments’ Departure, Arrival, POD is reported, the execution status will be changed to “In Transit”.</br>
If the delivery item’s all planned PODs from the delivery-assigned shipments are reported when the item is POD relevant, or the delivery item’s planned goods receipt quantity are fully reported when the item is not POD relevant, the execution status will be changed to “Completed”. </br>
Once the execution status is set as “Completed”, it cannot be changed any more. </br>
You can set your own execution status logic in Event-to-Action. </br>
 
## How to Obtain Support
The project is provided "as-is", with no expected support. </br>
If your issue is concerned with SAP Business Network Global Track and Trace, log your incident in SAP BCP system with component “SBN-LBN-GTT-APP”.</br>
