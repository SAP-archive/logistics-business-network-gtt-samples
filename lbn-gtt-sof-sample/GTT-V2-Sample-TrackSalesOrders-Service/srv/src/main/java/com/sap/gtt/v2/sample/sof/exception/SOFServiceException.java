package com.sap.gtt.v2.sample.sof.exception;


import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.http.HttpStatus;

import java.util.Locale;

public class SOFServiceException extends BaseRuntimeException {
    private static final long serialVersionUID = 1L;

    public static final String MESSAGE_CODE = SOFServiceException.class.getName();
    public static final String MESSAGE_CODE_ERROR_NO_DATA_FOUND = SOFServiceException.class.getName() + ".NoDataFound";
    public static final String MESSAGE_CODE_ERROR_UNKNOWN_MODEL_NAME = SOFServiceException.class.getName() + ".UnknownModelName";
    public static final String MESSAGE_CODE_ERROR_ODATA_INIT_FAILED = SOFServiceException.class.getName() + ".ODataInitFailed";
    public static final String MESSAGE_CODE_ERROR_PACKAGE_SCAN = SOFServiceException.class.getName() + ".PackageScanError";
    public static final String MESSAGE_CODE_ERROR_FEED_RESULT = SOFServiceException.class.getName() + ".FeedDataError";
    public static final String MESSAGE_CODE_ERROR_SET_NAVIGATION_PROPERTY = SOFServiceException.class.getName() + ".SetNavigationPropertyError";
    public static final String MESSAGE_CODE_ERROR_NO_DELAY_EVENT_FOUND = SOFServiceException.class.getName() + ".NoDelayEventFound";
    public static final String MESSAGE_CODE_INVALID_JSON = SOFServiceException.class.getName() + ".InvalidJson";
    public static final String MESSAGE_CODE_GET_TP_ID_FAILED = SOFServiceException.class.getName() + ".GetTpIdFailed";
    public static final String MESSAGE_CODE_UNSUPPORTED_TRACKING_ID_TYPE = SOFServiceException.class.getName() + ".UnsupportedTrackingIdType";
    public static final String MESSAGE_CODE_CALL_READ_SERVICE_FAILED = SOFServiceException.class.getName() + ".CallReadServiceFailed";
    public static final String MESSAGE_CODE_CALL_WRITE_SERVICE_FAILED = SOFServiceException.class.getName() + ".CallWriteServiceFailed";
    public static final String MESSAGE_CODE_CALL_METADATA_SERVICE_FAILED = SOFServiceException.class.getName() + ".CallMetadataServiceFailed";
    public static final String MESSAGE_CODE_CALL_LOCATION_SERVICE_FAILED = SOFServiceException.class.getName() + ".CallLocationServiceFailed";
    public static final String MESSAGE_CODE_DESTINATION_SERVICE_BINDING_NOT_FOUND = SOFServiceException.class.getName() + ".DestinationServiceBindingNotFound";

    public SOFServiceException(String internalMessage, Throwable cause, String messageCode) {
        super(internalMessage, cause, messageCode, new Object[]{});
    }

    public SOFServiceException(String internalMessage, Throwable cause, String messageCode, Object[] localizedMsgParams) {
        super(internalMessage, cause, messageCode, localizedMsgParams);
    }

    public SOFServiceException(String messageCode) {
        super(messageCode, new Object[]{});
    }

    public SOFServiceException(Throwable cause) {
        super(cause.getMessage(), cause, MESSAGE_CODE, new Object[]{});
    }

    public SOFServiceException(String messageCode, Object[] localizedMsgParams) {
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
