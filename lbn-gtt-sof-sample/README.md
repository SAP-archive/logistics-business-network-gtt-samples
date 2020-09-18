# Sales Order Fulfillment app

## Limitation
* Limitation for Document Flow implementation:<br>
Recommended number of layers in document flow ≤6, for number of layers > 6, please use table view or extend the document flow click by click <br>
Recommended number of nodes in document flow: ≤500, for number of nodes > 500, please use table view <br>

* Limitation for ERP Extractor implementation: <br>
Shipment’s planned event’ eventMatchKey = shipmentNo+stopId, stopId is set by stage’s sequence. <br>

## License
Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved. This file is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.   
