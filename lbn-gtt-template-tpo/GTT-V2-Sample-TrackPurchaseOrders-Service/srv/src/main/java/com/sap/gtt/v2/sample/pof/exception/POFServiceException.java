package com.sap.gtt.v2.sample.pof.exception;


import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.http.HttpStatus;

import java.util.Locale;

public class POFServiceException extends BaseRuntimeException {
    private static final long serialVersionUID = 1L;

    public static final String MESSAGE_CODE = POFServiceException.class.getName();
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

    public POFServiceException(String internalMessage, Throwable cause, String messageCode) {
        super(internalMessage, cause, messageCode, new Object[]{});
    }

    public POFServiceException(String internalMessage, Throwable cause, String messageCode, Object[] localizedMsgParams) {
        super(internalMessage, cause, messageCode, localizedMsgParams);
    }

    public POFServiceException(String messageCode) {
        super(messageCode, new Object[]{});
    }

    public POFServiceException(Throwable cause) {
        super(cause.getMessage(), cause, MESSAGE_CODE, new Object[]{});
    }

    public POFServiceException(String messageCode, Object[] localizedMsgParams) {
        super(messageCode, localizedMsgParams);
    }

    @Override
    public String getLocalizedMessage() {
        Locale locale = LocaleContextHolder.getLocale();
        return getLocalizedMessage(locale);
    }

    @Override
    public int getHttpStatus() {
        return HttpStatus.INTERNAL_SERVER_ERROR.value();
    }
}
