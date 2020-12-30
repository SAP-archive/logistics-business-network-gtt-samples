package com.sap.gtt.v2.sample.sst.common.exception;

import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

/**
 * {@link FormattedErrorMessage} is used to present error message in readable format.
 *
 * @author Min Li
 */
public class FormattedErrorMessage {

    private final int httpStatus;

    private final Error error;

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

    public ResponseEntity<String> getHttpResponse() {
        String jsonString = this.toJsonString();
        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.setContentType(MediaType.APPLICATION_JSON);
        return new ResponseEntity<>(jsonString, httpHeaders, HttpStatus.valueOf(this.getHttpStatus()));
    }

    public String toJsonString() {
        return SSTUtils.generateJsonStringFromBean(this);
    }

    public int getHttpStatus() {
        return httpStatus;
    }

    public Error getError() {
        return error;
    }
}
