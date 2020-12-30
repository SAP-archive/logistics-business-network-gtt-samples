package com.sap.gtt.v2.sample.sst.common.exception;

import java.util.Locale;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.http.HttpStatus;

/**
 * @author Min Li
 */
public class SSTServiceException extends BaseRuntimeException {

    public static final String MESSAGE_CODE = SSTServiceException.class.getName();
    public static final String MESSAGE_CODE_ERROR_ODATA_INIT_FAILED = SSTServiceException.class.getName() + ".ODataInitFailed";
    public static final String MESSAGE_CODE_ERROR_PACKAGE_SCAN = SSTServiceException.class.getName() + ".PackageScanError";
    public static final String MESSAGE_CODE_ERROR_FEED_RESULT = SSTServiceException.class.getName() + ".FeedDataError";
    public static final String MESSAGE_CODE_INVALID_JSON = SSTServiceException.class.getName() + ".InvalidJson";
    public static final String MESSAGE_CODE_CALL_READ_SERVICE_FAILED = SSTServiceException.class.getName() + ".CallReadServiceFailed";
    public static final String MESSAGE_CODE_CALL_WRITE_SERVICE_FAILED = SSTServiceException.class.getName() + ".CallWriteServiceFailed";
    public static final String MESSAGE_CODE_CALL_METADATA_SERVICE_FAILED = SSTServiceException.class.getName() + ".CallMetadataServiceFailed";
    public static final String MESSAGE_CODE_CALL_LOCATION_SERVICE_FAILED = SSTServiceException.class.getName() + ".CallLocationServiceFailed";
    public static final String MESSAGE_CODE_DESTINATION_SERVICE_BINDING_NOT_FOUND = SSTServiceException.class.getName() + ".DestinationServiceBindingNotFound";

    private static final long serialVersionUID = 1L;

    public SSTServiceException(String internalMessage, Throwable cause, String messageCode) {
        super(internalMessage, cause, messageCode, new Object[]{});
    }

    public SSTServiceException(String internalMessage, Throwable cause, String messageCode, Object[] localizedMsgParams) {
        super(internalMessage, cause, messageCode, localizedMsgParams);
    }

    public SSTServiceException(String messageCode) {
        super(messageCode, new Object[]{});
    }

    public SSTServiceException(Throwable cause) {
        super(cause.getMessage(), cause, MESSAGE_CODE, new Object[]{});
    }

    public SSTServiceException(String messageCode, Object[] localizedMsgParams) {
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
