[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/logistics-business-network-gtt-samples)](https://api.reuse.software/info/github.com/SAP-samples/logistics-business-network-gtt-samples) 
# Whats' New for Template Apps of SAP Business Network Global Track and Trace
## Announcement (2022.08.20) 
In this release, the following changes are made: 
* The UI5 version for Track Shipments, Track SO Fulfillment and Track PO Fulfillment template apps is upgraded to 1.96.10.
* For visibility provider integration, the mapping of the field 'trackid" is removed in the models for Track SO Fulfillment and Track PO Fulfillment template apps.

## Announcement (2022.07.16) 
In this release, the following changes are made: 
* The valid from/to segment (1970/9999) is removed from all the Tracking ID functions.
* The key specification "SOURCE_STOP_KEY" is enhanced.

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

If you choose not to upgrade to the latest version, here are some mandatory steps for you to adapt to SAP Business Network Global Track and Trace V2 February Delivery. 
* [Mandatory for VP integration] Add LBN# as a prefix for Service Agent LBN ID for the shipment and freight order/freight booking and freight unit extractors. 
* [Mandatory for receiving actual events from data contributors] Grant report and read authorization to Service Agent LBN ID in the shipment and resource process types of the corresponding model. 

