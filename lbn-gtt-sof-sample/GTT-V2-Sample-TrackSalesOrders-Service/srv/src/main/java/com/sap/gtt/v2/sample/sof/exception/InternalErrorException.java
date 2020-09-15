package com.sap.gtt.v2.sample.sof.exception;


import org.springframework.http.HttpStatus;

public class InternalErrorException extends BaseRuntimeException implements ILogable {
    public static final String ERROR_CODE = "ERROR_CODE_INTERNAL_ERROR";

    public static final String MESSAGE_CODE = InternalErrorException.class.getName();

    public InternalErrorException(Throwable cause) {
        super(null, cause, MESSAGE_CODE, null);
    }

    public InternalErrorException(String message, Throwable cause) {
        super(message, cause, MESSAGE_CODE, null);
    }

    public InternalErrorException(String message) {
        super(message, null, MESSAGE_CODE, null);
    }


    @Override
    public int getHttpStatus() {
        return HttpStatus.INTERNAL_SERVER_ERROR.value();
    }

    @Override
    public String getErrorCode() {
        return ERROR_CODE;
    }

}
