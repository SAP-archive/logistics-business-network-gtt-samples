# Title
SAP LBN Track and Trace option sample apps

## Description
You can find SAP LBN Track and Trace Option sample code in this project. The project aims to provide examples to build your own tracking application on the top of SAP LBN Track and Trace Option. You could just implement the sample code or do the modification according to your specific requirements. For each tracking scenario, SAP LBN Track and Trace Option sample code includes:
1.	ERP extractor to send out the tracked process to SAP LBN Track and Trace Option (ABAP)
2.	Tracked process model required to import into SAP LBN Track and Trace Option (model file)
3.	Backend and Frontend code to build Fiori application on SAP LBN Track and Trace Option (Java and JavaScript)

## Requirements
1.	An SAP Cloud Platform global account with entitlements for SAP LBN Track and Trace option, 1 portal service quota and 10 GB Application Runtime quota.
2.	To integrate with ERP, you need an SAP ERP or SAP ECC system running on Netweaver 7.31 or higher with SAP NOTE 2937175 being implemented. 
3.	To integrate with visibility provider, please log your incident in SAP BCP system under component “SCM-LBN-GTT-COR”. Currently it will be supported manually.

## Download and Installation
Check document folder in this GitHub to find detailed installation guide for each tracking scenario


## Limitation
Sales Order Fulfillment app
Limitation for Document Flow implementation:
Recommended number of layers in document flow ≤6, for number of layers > 6, please use table view or extend the document flow click by click
Recommended number of nodes in document flow: ≤500, for number of nodes > 500, please use table view

Limitation for ERP Extractor implementation:
Shipment’s planned event’ eventMatchKey = shipmentNo+stopId, stopId is set by stage’s sequence.

## FAQ
Check document folder in this GitHub to find detailed installation guide for each tracking scenario:

## How to obtain support
The project is provided "as-is", with no expected support.
If it is an LBN Track & Trace option issue, please log your incidents in SAP BCP system with component “SCM-LBN-GTT-COR”

## Contributing     

## License
Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved. This file is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.   
