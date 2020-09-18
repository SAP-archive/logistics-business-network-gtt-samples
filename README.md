# SAP LBN Global Track and Trace option template apps

## Description
The project aims to provide examples to build your own tracking application on top of [SAP LBN Global Track and Trace Option]( https://help.sap.com/viewer/product/SAP_LBN_GTT_OPTION/LBN/en-US?task=discover_task). You could just implement the template code or do the modification according to your specific requirements. For each tracking scenario, the template code includes:
* ERP extractor to send out the tracked process to SAP LBN Track and Trace Option (ABAP)
* Tracked process model required to import into SAP LBN Track and Trace Option (model file)
* Backend and Frontend code to build Fiori application on SAP LBN Track and Trace Option (Java and JavaScript)

## Requirements
* An SAP Cloud Platform global account with entitlements for SAP LBN Track and Trace option, 1 portal service quota and 2 GB Application Runtime quota.
* To integrate with ERP, you need a S/4, SAP ERP or SAP ECC system running on Netweaver 7.31 or higher with SAP NOTE 2937175 being implemented. 
* To integrate with visibility provider, please log your incident in SAP [BCP]( https://support.wdf.sap.corp/)  system under component “SCM-LBN-GTT-COR”. Currently it will be supported manually.

## Download and Installation
Click below link to check detailed installation guide for each tracking scenario, or you can find them in corresponding scenario’s “Document” folder.
* [Sales Order Fulfillment – Implementation Guide] (https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/dev/lbn-gtt-sof-sample/Documents/01_Implementation%20Guide%20-%20SOF.pdf)

## Limitation
Click below link to check limitation for each tracking scenario, or you can find them in corresponding scenario’s “Readme” file.
* [Sales Order Fulfillment – Limitation]( https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/dev/lbn-gtt-sof-sample/README.md#limitation)
## FAQ
Click below link to check FAQs for each tracking scenario, or you can find them in corresponding scenario’s “Document” folder.
* [Sales Order Fulfillment – FAQ] (https://github.com/SAP-samples/logistics-business-network-gtt-samples/blob/dev/lbn-gtt-sof-sample/Documents/02_FAQ%20-%20SOF.pdf)

## How to obtain support
The project is provided "as-is", with no expected support. 
If it is an LBN Global Track & Trace option issue, please log your incidents in SAP [BCP]( https://support.wdf.sap.corp/) system with component “SCM-LBN-GTT-COR”

## License
Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved. This file is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.   
