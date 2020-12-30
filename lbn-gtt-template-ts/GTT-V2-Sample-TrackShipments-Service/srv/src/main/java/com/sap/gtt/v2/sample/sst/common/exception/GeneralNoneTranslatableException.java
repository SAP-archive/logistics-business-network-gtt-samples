package com.sap.gtt.v2.sample.sst.common.exception;

/**
 * @author Min Li
 */
public class GeneralNoneTranslatableException extends BaseRuntimeException implements ILogable {

    public static final String ERROR_CODE = "ERROR_CODE_NONE_TRANSLATE";

    private final int httpStatus;

    public GeneralNoneTranslatableException(String message, Throwable cause, int httpStatus) {
        super(message, cause, null, null);
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
