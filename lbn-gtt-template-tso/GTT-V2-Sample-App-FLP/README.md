# GTT-V2-Sample-App-FLP

The FLP project includes 3 template applications which are Track SO Fulfillment, Track Shipments and Track Purchase Orders. Before you build and deploy it, please deploy all the UI projects you need in advance, otherwise the deployment will fail.

## Deploy It with One Project

If you only need Track SO Fulfillment app, do the following changes:

1. Open `mta.yaml`

- Delete `lbn-gtt-sst-api` and `lbn-gtt-pof-api` in `lbn-gtt-sample-app-appRouter` and `resources` modules

- Delete `gtt-ui-sample-track-shipments-app-host` and `gtt-ui-sample-track-purchaseorders-app-host` in `flp` and `resources` modules

2. Open `flp/portal-site/CommonDataModel.json`,

- Delete viz items `com.sap.gtt.app.sample.sst` and `com.sap.gtt.app.sample.pof` in `payload/catalogs/payload/viz` array

- Delete groups `shipment` and `purchaseOrder` in `payload/groups` array

- Delete orders `shipment` and `purchaseOrder` in `payload/sites/payload/groupsOrder` array
