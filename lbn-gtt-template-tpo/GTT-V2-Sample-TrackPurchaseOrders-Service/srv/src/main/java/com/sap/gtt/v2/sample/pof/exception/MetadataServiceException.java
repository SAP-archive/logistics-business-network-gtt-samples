package com.sap.gtt.v2.sample.pof.exception;

public class MetadataServiceException extends BaseRuntimeException implements ILogable {
    public static final String ERROR_CODE = "CALL_METADATA_SERVICE_ERROR";

    public static final String MESSAGE_CODE = MetadataServiceException.class.getName();
    public static final String ERROR_MESSAGE = MESSAGE_CODE+".CallMetadataServicError";

    public int statusCode;

    public MetadataServiceException(Throwable cause,int statusCode) {
        super(ERROR_CODE, cause, ERROR_MESSAGE, null);
        this.statusCode = statusCode;
    }

    @Override
    public int getHttpStatus() {
        return statusCode;
    }

    @Override
    public String getErrorCode() {
        return ERROR_CODE;
    }

    @Override
    public String getMessage() {
        return ERROR_MESSAGE;
    }

}
