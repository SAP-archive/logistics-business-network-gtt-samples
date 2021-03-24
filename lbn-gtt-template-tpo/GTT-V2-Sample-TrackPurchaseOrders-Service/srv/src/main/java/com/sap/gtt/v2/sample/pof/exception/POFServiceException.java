package com.sap.gtt.v2.sample.pof.exception;


public class POFServiceException extends BaseRuntimeException {
    private static final long serialVersionUID = 1L;
    public static final String ERROR_CODE = "INTERNAL_SERVER_ERROR";

    public static final String MESSAGE_CODE = POFServiceException.class.getName();
    public static final String MESSAGE_CODE_INTERNAL_SERVER_ERROR = POFServiceException.class.getName()+".InternalServerError";
    public static final String MESSAGE_CODE_ERROR_NO_DATA_FOUND = POFServiceException.class.getName() + ".NoDataFound";
    public static final String MESSAGE_CODE_ERROR_UNKNOWN_MODEL_NAME = POFServiceException.class.getName() + ".UnknownModelName";
    public static final String MESSAGE_CODE_ERROR_ODATA_INIT_FAILED = POFServiceException.class.getName() + ".ODataInitFailed";
    public static final String MESSAGE_CODE_ERROR_PACKAGE_SCAN = POFServiceException.class.getName() + ".PackageScanError";
    public static final String MESSAGE_CODE_ERROR_FEED_RESULT = POFServiceException.class.getName() + ".FeedDataError";
    public static final String MESSAGE_CODE_ERROR_SET_NAVIGATION_PROPERTY = POFServiceException.class.getName() + ".SetNavigationPropertyError";
    public static final String MESSAGE_CODE_ERROR_NO_DELAY_EVENT_FOUND = POFServiceException.class.getName() + ".NoDelayEventFound";
    public static final String MESSAGE_CODE_INVALID_JSON = POFServiceException.class.getName() + ".InvalidJson";
    public static final String MESSAGE_CODE_GET_TP_ID_FAILED = POFServiceException.class.getName() + ".GetTpIdFailed";
    public static final String MESSAGE_CODE_UNSUPPORTED_TRACKING_ID_TYPE = POFServiceException.class.getName() + ".UnsupportedTrackingIdType";
    public static final String MESSAGE_CODE_CALL_READ_SERVICE_FAILED = POFServiceException.class.getName() + ".CallReadServiceFailed";
    public static final String MESSAGE_CODE_CALL_WRITE_SERVICE_FAILED = POFServiceException.class.getName() + ".CallWriteServiceFailed";
    public static final String MESSAGE_CODE_CALL_METADATA_SERVICE_FAILED = POFServiceException.class.getName() + ".CallMetadataServiceFailed";
    public static final String MESSAGE_CODE_CALL_LOCATION_SERVICE_FAILED = POFServiceException.class.getName() + ".CallLocationServiceFailed";
    public static final String MESSAGE_CODE_DESTINATION_SERVICE_BINDING_NOT_FOUND = POFServiceException.class.getName() + ".DestinationServiceBindingNotFound";
    public static final String MESSAGE_CODE_UNSUPPORTABLE_DOCUMENT_FLOW_STATUS = POFServiceException.class.getName()+ ".UnsupportableDocumentFlowGeneralStatusEnum";

    private int httpStatus;


    public POFServiceException(String messageCode, Object[] localizedMsgParams,int httpStatus) {
        super(messageCode, localizedMsgParams);
        this.httpStatus = httpStatus;
    }

    public POFServiceException(Throwable cause,String message, int httpStatus) {
        super(ERROR_CODE, cause, message, null);
        this.httpStatus = httpStatus;
    }
    public POFServiceException(String message, int httpStatus) {
        super(message, null);
        this.httpStatus = httpStatus;
    }

    public POFServiceException(String message,String rootMessage, int httpStatus) {
        super(message, rootMessage,ERROR_CODE,null);
        this.httpStatus = httpStatus;
    }

    public POFServiceException(Throwable cause, int httpStatus) {
        super(ERROR_CODE, cause, MESSAGE_CODE_INTERNAL_SERVER_ERROR, null);
        this.httpStatus = httpStatus;
    }
    public POFServiceException(String messageCode,String message, Throwable cause, int httpStatus) {
        super(message, cause, messageCode, null);
        this.httpStatus = httpStatus;
    }
    @Override
    public int getHttpStatus() {
        return httpStatus;
    }

    @Override
    public String getErrorCode() {
        return ERROR_CODE;
    }

}
