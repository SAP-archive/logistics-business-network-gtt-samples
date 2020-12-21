package com.sap.gtt.v2.sample.pof.exception;

import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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
        private List<Detail> details;

        public String getCode() {
            return code;
        }

        public String getMessage() {
            return message;
        }

        public List<Detail> getDetails() {
            return details;
        }

        public static class Detail {
            private String lang;
            private String code;
            private String message;

            public String getLang() {
                return lang;
            }

            public String getCode() {
                return code;
            }

            public void setCode(String code) {
                this.code = code;
            }

            public String getMessage() {
                return message;
            }

            public void setMessage(String message) {
                this.message = message;
            }
        }


    }

    public FormattedErrorMessage(String messageCode, String message, int httpStatus) {
        this.error = new Error();
        this.error.code = messageCode;
        this.error.message = message;
        this.httpStatus = httpStatus;
        this.error.details = Collections.emptyList();
    }

    public FormattedErrorMessage(List<FormattedErrorMessage> containedFormattedErrorMessages, String lang, int httpStatus) {
        this.error = new Error();
        List<Error.Detail> detailList = new ArrayList<>();
        this.error.code = "";
        this.error.message = "";
        for (FormattedErrorMessage e : containedFormattedErrorMessages) {
            Error.Detail detail = new Error.Detail();
            if (e.error != null) {
                detail.code = e.error.code;
                detail.lang = lang;
                detail.message = e.error.message;
            }
            detailList.add(detail);
        }
        this.error.details = detailList;
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
