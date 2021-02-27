package com.sap.gtt.v2.sample.pof.constant;

public class Constants {
    public static final String GTT_MODEL_NAMESPACE = "com.lbngttsamples.gtt.app.pof";
    public static final String GTT_MODEL_NAMESPACE_WRITE_SERVICE = GTT_MODEL_NAMESPACE + ".pofWriteService";
    public static final String MODEL_NAMESPACE = GTT_MODEL_NAMESPACE + ".pofService";

    public static final String URL_SPLITTER = "/";
    public static final String ENTITY_CONTAINER_NAME = "EntityContainer";
    public static final String ID = "id";

    public static final String EVENT_STATUS_PLANNED = "PLANNED";

    public static final String GOODS_RECEIPT_EVENT = "GoodsReceipt";
    public static final String CONFIRMATION_EVENT = "ConfirmationEvent";
    public static final String POD_EVENT = "POD";

    public static final String PROCESS_STATUS_AS_PLANNED = "AS_PLANNED";
    public static final String PROCESS_STATUS_OVERDUE = "OVERDUE";
    public static final String PROCESS_STATUS_DELAYED = "DELAYED";
    public static final String PROCESS_STATUS_LATE = "LATE";
    public static final String PROCESS_STATUS_EARLY = "EARLY";

    public static final String CORRELATION_TYPE_CODE = "correlationType_code";
    public static final String BLANK = " ";
    public static final String PROCESS_EVENT_DIRECTORY_ENTITY_NAME = "ProcessEventDirectory";
    public static final String EVENT_EXPAND = "event";
    public static final String AND = "and";
    public static final int MAX_LONGITUDE = 180;
    public static final int MAX_LATITUDE = 90;

    public static final String PLANNED_EVENT_EXPAND = "plannedEvent";
    public static final String PLANNED_EVENT_ENTITY_NAME = "PlannedEvent";
    public static final String DELETION_EVENT_ENTITY_NAME = "DeletionEvent";
    public static final String UNDELETION_EVENT_ENTITY_NAME = "UndeletionEvent";

    public static final String EXPAND = "$expand";

    public static final String SHIPMENT = "SHIPMENT_ORDER";

    public static final String EVENT_STATUS_REPORTED = "REPORTED";
    public static final String EVENT_STATUS_EARLY_REPORTED = "EARLY_REPORTED";
    public static final String EVENT_STATUS_LATE_REPORTED = "LATE_REPORTED";
    public static final String RESOURCE = "RESOURCE";
    public static final String SHIPMENT_ARRIVAL = "Shipment.Arrival";
    public static final String PLANT_LOCATION = "toPlantLocation";
    public static final String SUPPLIER_LOCATION = "toSupplierLocation";
    public static final String RECEIVING_LOCATION = "toReceivingLocation";


    public static final String DATE_TIME_PATTERN = "yyyy-MM-dd'T'HH:mm:ss'Z'";
    public static final String PROCESS_ID = "process_id";
    public static final String EVENT_ACTUAL_BUSINESS_TIMESTAMP = "event/actualBusinessTimestamp";

    public static final String MARK_REVERSAL = "X";

    private Constants() {
        // Constant class, mark it private
    }

}
