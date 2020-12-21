package com.sap.gtt.v2.sample.sst.common.exception;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationCredentialsNotFoundException;
import org.springframework.web.HttpMediaTypeException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.multipart.MultipartException;

/**
 * {@link DefaultExceptionHandler} is a default exception handler.
 *
 * @author Min Li
 */
@ControllerAdvice
public class DefaultExceptionHandler {

    private static final Logger logger = LoggerFactory.getLogger(DefaultExceptionHandler.class);

    @ExceptionHandler(Exception.class)
    public ResponseEntity<String> handleException(Exception e) {
        return handleExceptionAndTransformMessage(e).getHttpResponse();
    }

    public FormattedErrorMessage handleExceptionAndTransformMessage(Exception e) {
        BaseRuntimeException finalException;
        if (!(e instanceof BaseRuntimeException)) {
            if (e instanceof HttpStatusCodeException) {
                HttpStatusCodeException httpStatusCodeException = (HttpStatusCodeException) e;
                finalException = new GeneralNoneTranslatableException(e.getMessage(), e, httpStatusCodeException.getStatusCode().value());
            } else if (e instanceof HttpRequestMethodNotSupportedException) {
                finalException = new GeneralNoneTranslatableException(e.getMessage(), e, HttpStatus.METHOD_NOT_ALLOWED.value());
            } else if (e instanceof HttpMediaTypeException) {
                finalException = new GeneralNoneTranslatableException(e.getMessage(), e, HttpStatus.UNSUPPORTED_MEDIA_TYPE.value());
            } else if (e instanceof AuthenticationCredentialsNotFoundException) {
                finalException = new GeneralNoneTranslatableException(e.getMessage(), e, HttpStatus.UNAUTHORIZED.value());
            } else if (e instanceof MultipartException) {
                finalException = new GeneralNoneTranslatableException(e.getMessage(), e, HttpStatus.URI_TOO_LONG.value());
            } else if (e instanceof MissingServletRequestParameterException
                    || e instanceof HttpClientErrorException) {
                finalException = new GeneralNoneTranslatableException(e.getMessage(), e, HttpStatus.BAD_REQUEST.value());
            } else {
                finalException = new InternalErrorException(e.getMessage(), e);
            }
        } else {
            finalException = (BaseRuntimeException) e;
        }

        if (finalException instanceof ILogable) {
            logger.error(finalException.getMessage(), finalException);
        }
        return finalException.getFormattedErrorMessage();
    }
}
