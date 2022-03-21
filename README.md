[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/logistics-business-network-gtt-samples)](https://api.reuse.software/info/github.com/SAP-samples/logistics-business-network-gtt-samples) 
# Template Apps for SAP Logistics Business Network, Global Track and Trace Option

## Announcement (2021.03.21) 
In this release, the following changes are made. For more information, refer to Read Me and What’s New in the relevant document folder.  </br>
* Enable Air Tracking mode with integration of visibility provider in the Track SO Fulfillment and Track PO Fulfillment template apps
* Enable Air Tracking mode with integration of visibility provider and SAP TM (Air Freight Booking) in the Track Shipments template app
* Enable Quick Refresh feature in the Track PO Fulfillment template app
* Enhance the calculation part in the Track PO Fulfillment template app
* Refine exception texts in the Track PO Fulfillment template app
* Enhance forwardEventtoTM event-to-action script with actual event’s message type in Track Shipments template app
* Enhance code quality
* Fix bugs
* Enhance documentation. </br>


## Announcement (2021.02.26) 
In this release, the following changes are made. For more information, refer to Read Me and What’s New in the relevant document folder.  </br>
* Rename Track Sales Orders to Track SO Fulfillment 
* Rename Track Purchase Orders to Track PO Fulfillment 
* Add the requirement “To implement the template apps' extractor codes, your SAP ERP system version should be S4 1909 SP03 on premise or higher.”
* Integrate with deliveries and freight units in the Track Shipments template app 
* Deliver non-feature enhancements in the Track SO Fulfillment and Track PO Fulfillment template apps
* Deliver feature enhancements in the Track PO Fulfillment template app
* Provide introduction material in the Track PO Fulfillment template app
* Provide FAQs for Template Code Implementation
* Add annotations for the Event-to-Action script to make each function more understandable. </br>

If you choose not to upgrade to the latest version, here are some mandatory steps for you to adapt to GTT V2 February Delivery. 
* [Mandatory for VP integration] Add LBN# as a prefix for Service Agent LBN ID for the shipment and freight order/freight booking and freight unit extractors. 
* [Mandatory for receiving actual events from data contributors] Grant report and read authorization to Service Agent LBN ID in the shipment and resource process types of the corresponding model. 

## Description
You can find the template code for [SAP Logistics Business Network, global track and trace option]( https://help.sap.com/viewer/product/SAP_LBN_GTT_OPTION/LBN/en-US?task=discover_task) in this project. The project aims to provide examples to build your own tracking application based on the option. You can either implement the sample code or customize it to fit your needs. For each tracking scenario, the template code includes: 
* ERP extractors to send out the tracked processes and events to global track and trace option (ABAP) 
* Tracked process model that needs to be imported into global track and trace option (model file) 
* Backend and frontend codes to build Fiori application on global track and trace option (Java and JavaScript)
 
## Requirements
* A SAP Business Technology Platform global account with entitlement to the global track and trace option for SAP Logistics Business Network, 1 portal service quota and 2 GB Application Runtime quota.
* To integrate the global track and trace option with ERP, your SAP ERP system should be running on Netweaver 7.31 or higher with SAP NOTE 2937175 being implemented. Besides, to implement the template apps' extractor codes, your SAP ERP system version should be S4 1909 SP03 on premise or higher.
* The node “Interface to Global Track and Trace” in the IMG and the related GTT-specific versions of the IMG activities are in Netweaver 740 only available from SP 18 on, and in Netweaver 750 only from SP 08 on. They cannot be downloaded as a correction via note assistant. We recommend upgrading to the service package level accordingly.
* To integrate with visibility provider, log your incidents in SAP BCP system with component “SBN-LBN-GTT-APP”.

## Download and Installation
Click the link below to find the detailed installation guide for each tracking scenario. You can also find them in the “Document” folder of the corresponding scenario.
* Check the FAQs when you get errors during template code implmentation </br> 
[FAQs_for_Template_Code_Implementation.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/FAQs_for_Template_Code_Implementation.pdf) </br>
* Track SO Fulfillment app </br>
[02_Implementation_Guide-TSOF.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tso/Documents/02_Implementation_Guide-TSOF.pdf)  </br>
[03_Extractor_Creation_Guide-TSOF.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tso/Documents/03_Extractor_Creation_Guide-TSOF.pdf)  </br>

* Track Shipments app </br>
[01_Application_Introduction-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/01_Application_Introduction-TS.pdf) </br>
[02_Implementation_Guide-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/02_Implementation_Guide-TS.pdf) </br>
[03_Synchronize_Actual_Events_To_TM-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/03_Synchronize_Actual_Events_To_TM-TS.pdf) </br>
[04_Extractor_Creation_Guide-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/Documents/04_Extractor_Creation_Guide-TS.pdf)

* Track Purchase Orders app </br>
[01_Implementation_Guide-TPOF.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tpo/Documents/01_Implementation_Guide-TPOF.pdf) </br>
[02_Extractor_Creation_Guide-TPOF.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tpo/Documents/02_Extractor_Creation_Guide-TPOF.pdf) </br>
[03_Introduction_Material_for_TPOF.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tpo/Documents/03_Introduction_Material_for_TPOF.pdf) </br>

## Limitations
Click the link below to check the limitations of each tracking scenario. You can also find them in the “README.md” file of the corresponding scenario.
* [Track SO Fulfillment – Limitations](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tso/README.md#limitations)
* [Track Shipments – Limitations](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/README.md#limitations)
* [Track PO Fulfillment – Limitations](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tpo/README.md#limitations)

## Known Issues
Click the link below to check known issues for each tracking scenario. You can also find them in the “README.md” file of the corresponding scenario.
* [Track SO Fulfillment – Known Issue](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tso/README.md#known-issue)
* [Track PO Fulfillment – Known Issue](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tpo/README.md#known-issue)

## FAQs
Click the link below to check FAQs for each tracking scenario. You can also find them in the “README.md” file of the corresponding scenario.
* [Track SO Fulfillment – FAQ](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tso/README.md#faqs)
* [Track Shipments – FAQ](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-ts/README.md#faqs)
* [Track PO Fulfillment – FAQ](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/lbn-gtt-template-tpo/README.md#faqs)

## How to Obtain Support
The project is provided "as-is", with no expected support. </br>
If your issue is concerned with global track and trace option, log your incident in SAP BCP system with component “SBN-LBN-GTT-APP”. 

For additional support, [ask a question in SAP Community](https://answers.sap.com/questions/ask.html?additionalTagId=73555000100800000602).

## License
Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/LICENSES/Apache-2.0.txt) file.   
