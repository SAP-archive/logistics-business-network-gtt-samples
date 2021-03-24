package com.sap.gtt.v2.sample.pof.exception;

public class WriteServiceException extends BaseRuntimeException implements ILogable {
    public static final String ERROR_CODE = "CALL_WRITE_SERVICE_ERROR";

    public static final String MESSAGE_CODE = WriteServiceException.class.getName();
    public static final String ERROR_MESSAGE = MESSAGE_CODE+".CallWriteServicError";

    public int statusCode;

    public WriteServiceException(Throwable cause,int statusCode) {
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


}
