package com.sap.gtt.v2.sample.sof.constant;

public class Constants {
    public static final String MODEL_ATTRIBUTE_KEY = "GTT_CORE_ENGINE_MODEL";
    public static final String URL_SPLITTER = "/";
    public static final String RESULTS_NODE = "results";
    public static final String D_NODE = "d";
    public static final String INLINECOUNT_NODE = "__count";
    public static final String MODEL_ATTRIBUTE_ORIGIN_KEY = "MODEL_ATTRIBUTE_ORIGIN_KEY";
    public static final String ODATA_BATCH_PARENT_CONTEXT = "~odataBatchParentContext";
    public static final String EDM_DATETIME_TYPE = "Edm.DateTime";
    public static final String EDM_GUID_TYPE = "Edm.Guid";
    public static final String EDM_DECIMAL_TYPE = "Edm.Decimal";
    public static final String EDM_DATEOFFSET_TYPE = "Edm.DateTimeOffset";
    public static final String EDM_INT16_TYPE = "Edm.Int16";
    public static final String EDM_INT32_TYPE = "Edm.Int32";
    public static final String EDM_INT64_TYPE = "Edm.Int64";
    public static final String EDM_DOUBLE_TYPE = "Edm.Double";
    public static final String GTT_MODEL_NAMESPACE = "com.lbngttsamples.gtt.app.sof";
    public static final String MODEL_NAMESPACE = "com.lbngttsamples.gtt.app.sof.sofService";
    public static final String WRITE_SERVICE_MODEL_NAMESPACE = "com.lbngttsamples.gtt.app.sof.sofWriteService";
    public static final String ENTITY_CONTAINER_NAME = "EntityContainer";
    public static final String SAP_ICON_LEAD = "sap-icon://lead";
    public static final String ID = "id";

    public static final String EVENT_STATUS_PLANNED = "PLANNED";

    public static final String PROCESS_STATUS_OVERDUE = "OVERDUE";
    public static final String PROCESS_STATUS_DELAYED = "DELAYED";
    public static final String PROCESS_STATUS_LATE = "LATE";

    public static final String EXECUTION_STATUS_POD = "POD";

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


    private Constants() {
        // Constant class, mark it private
    }

}
