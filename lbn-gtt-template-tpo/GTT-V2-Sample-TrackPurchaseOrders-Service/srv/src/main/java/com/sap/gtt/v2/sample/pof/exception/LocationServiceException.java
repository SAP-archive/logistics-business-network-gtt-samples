package com.sap.gtt.v2.sample.pof.exception;

public class LocationServiceException extends BaseRuntimeException implements ILogable {
    public static final String ERROR_CODE = "CALL_LOCATION_SERVICE_ERROR";

    public static final String MESSAGE_CODE = LocationServiceException.class.getName();
    public static final String ERROR_MESSAGE = MESSAGE_CODE+".CallLocationServicError";

    public int statusCode;

    public LocationServiceException(Throwable cause,int statusCode) {
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
