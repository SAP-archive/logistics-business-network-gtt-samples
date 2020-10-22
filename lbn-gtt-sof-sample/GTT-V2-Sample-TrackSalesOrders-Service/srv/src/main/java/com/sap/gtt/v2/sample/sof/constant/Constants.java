package com.sap.gtt.v2.sample.sof.constant;

public class Constants {
    public static final String GTT_MODEL_NAMESPACE = "com.lbngttsamples.gtt.app.sof";
    public static final String GTT_MODEL_NAMESPACE_WRITE_SERVICE = GTT_MODEL_NAMESPACE + ".sofWriteService";
    public static final String MODEL_NAMESPACE = GTT_MODEL_NAMESPACE + ".sofService";

    public static final String URL_SPLITTER = "/";
    public static final String ENTITY_CONTAINER_NAME = "EntityContainer";
    public static final String SAP_ICON_LEAD = "sap-icon://lead";
    public static final String ID = "id";

    public static final String EVENT_STATUS_PLANNED = "PLANNED";

    public static final String PROCESS_STATUS_OVERDUE = "OVERDUE";
    public static final String PROCESS_STATUS_DELAYED = "DELAYED";
    public static final String PROCESS_STATUS_LATE = "LATE";

    public static final String CORRELATION_TYPE_CODE = "correlationType_code";
    public static final String BLANK = " ";
    public static final String PROCESS_EVENT_DIRECTORY_ENTITY_NAME = "ProcessEventDirectory";
    public static final String EVENT_EXPAND = "event";
    public static final String AND = "and";
    public static final int MAX_LONGITUDE = 180;
    public static final int MAX_LATITUDE = 90;

    public static final String EXECUTION_STATUS_COMPLETED = "COMPLETED";
    public static final String PLANNED_EVENT_EXPAND = "plannedEvent";
    public static final String PLANNED_EVENT_ENTITY_NAME = "PlannedEvent";

    public static final String SHIPMENT = "SHIPMENT_ORDER";

    public static final String EVENT_STATUS_REPORTED = "REPORTED";
    public static final String EVENT_STATUS_EARLY_REPORTED = "EARLY_REPORTED";
    public static final String EVENT_STATUS_LATE_REPORTED = "LATE_REPORTED";
    public static final String RESOURCE = "RESOURCE";
    public static final String SHIPMENT_ARRIVAL = "Shipment.Arrival";
    public static final String SHIP_TO_PARTY_LOCATION = "shipToPartyLocation";


    private Constants() {
        // Constant class, mark it private
    }

}
