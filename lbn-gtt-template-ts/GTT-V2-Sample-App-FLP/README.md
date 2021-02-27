# GTT-V2-Sample-App-FLP

The FLP project includes 3 template applications which are Track SO Fulfllment, Track Shipments and Track Purchase Orders. Before you build and deploy it, please deploy all the UI projects you need in advance, otherwise the deployment will fail.

## Deploy It with One Project

If you only need Track Shipments app, do the following changes:

1. Open `mta.yaml`

- Delete `lbn-gtt-sof-api` and `lbn-gtt-pof-api` in `lbn-gtt-sample-app-appRouter` and `resources` modules

- Delete `gtt-ui-sample-track-salesorders-app-host` and `gtt-ui-sample-track-purchaseorders-app-host` in `flp` and `resources` modules

2. Open `flp/portal-site/CommonDataModel.json`,

- Delete viz items `com.sap.gtt.app.sample.sof` and `com.sap.gtt.app.sample.pof` in `payload/catalogs/payload/viz` array

- Delete groups `salesOrder` and `purchaseOrder` in `payload/groups` array

- Delete orders `salesOrder` and `purchaseOrder` in `payload/sites/payload/groupsOrder` array
