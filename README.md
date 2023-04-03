[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/logistics-business-network-gtt-samples)](https://api.reuse.software/info/github.com/SAP-samples/logistics-business-network-gtt-samples) 
# Template Apps for SAP Business Network Global Track and Trace
## Description
You can find the template code for [SAP Business Network Global Track and Trace]( https://help.sap.com/viewer/product/SAP_LBN_GTT_OPTION/LBN/en-US?task=discover_task) in this project. The project aims to provide examples to build your own tracking application based on the solution. You can either implement the sample code or customize it to fit your needs. For each tracking scenario, the template code includes: 
* ERP extractors to send out the tracked processes and events to SAP Business Network Global Track and Trace (ABAP) 
* Tracked process model that needs to be imported into SAP Business Network Global Track and Trace (model file) 
* Backend and frontend codes to build Fiori application on SAP Business Network Global Track and Trace (Java and JavaScript)
 
## Requirements
* An SAP Business Technology Platform global account with entitlement to SAP Business Network Global Track and Trace, 1 portal service quota and 2 GB Application Runtime quota.
* Make sure that you have met the requirements for the product version mentioned in the [Prerequisites](https://help.sap.com/docs/SAP_LBN_GTT_OPTION/d0802f41861a4f81a3610d873fdcf148/c9f7baf5f6e14be4ba9045786961de14.html) section of Appendix one: Connect to SAP ERP in Administration Guide for Version 2. You can find this guide at http://help.sap.com/gtt. 
* The ABAP codes on Github to support sample apps for SAP Business Network Global Track and Trace shall be implemented in SAP S/4HANA 1909 SP03 on premise or higher. Please note that the codes are not validated in its lower version or other ECC series of products, so you might need to do further adaptation work or build your own extractor.
* To integrate with visibility providers, log your incidents in SAP BCP system with component “SBN-LBN-GTT-APP”.

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
[03_Synchronize_Actual_Events_To_TM-TS.pdf](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/main/lbn-gtt-template-ts/Documents/03_Synchronize%20Actual%20Events%20Back%20to%20TM-TS.pdf) </br>
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
If your issue is concerned with SAP Business Network Global Track and Trace, log your incident in SAP BCP system with component “SBN-LBN-GTT-APP”. 

For additional support, [ask a question in SAP Community](https://answers.sap.com/questions/ask.html?additionalTagId=73555000100800000602).

## License
Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/master/LICENSES/Apache-2.0.txt) file.   
