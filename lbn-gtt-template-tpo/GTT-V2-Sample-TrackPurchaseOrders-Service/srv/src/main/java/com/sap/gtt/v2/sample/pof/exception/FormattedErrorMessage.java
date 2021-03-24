package com.sap.gtt.v2.sample.pof.exception;

import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

public class FormattedErrorMessage {

    private transient int httpStatus;
    private Error error;

    public int getHttpStatus() {
        return httpStatus;
    }

    public Error getError() {
        return error;
    }

    public static class Error {
        private String code;
        private String message;
        private String rootCauseMessage;

        public String getRootCauseMessage() {
            return rootCauseMessage;
        }

        public void setRootCauseMessage(String rootCauseMessage) {
            this.rootCauseMessage = rootCauseMessage;
        }

        public String getCode() {
            return code;
        }

        public String getMessage() {
            return message;
        }

    }

    public FormattedErrorMessage(String messageCode, String message, String rootCauseMessage,int httpStatus) {
        this.error = new Error();
        this.error.code = messageCode;
        this.error.message = message;
        this.error.rootCauseMessage = rootCauseMessage;
        this.httpStatus = httpStatus;
    }

    public String toJsonString() {
        return POFUtils.generateJsonStringFromBean(this);
    }

    public ResponseEntity<String> getHttpResponse() {
        String jsonString = this.toJsonString();
        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.setContentType(MediaType.APPLICATION_JSON);
        return new ResponseEntity<>(jsonString, httpHeaders, HttpStatus.valueOf(this.getHttpStatus()));

    }
}
