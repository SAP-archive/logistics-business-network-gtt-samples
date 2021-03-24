package com.sap.gtt.v2.sample.pof.exception;

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

@ControllerAdvice
public class DefaultExceptionHandler {
    private static final String MESSAGE_PATH = "/error/message";
    private static Logger logger = LoggerFactory.getLogger(DefaultExceptionHandler.class);

    @ExceptionHandler(Exception.class)
    public ResponseEntity<String> handleException(Exception e) {
        return handleExceptionAndTransformMessage(e).getHttpResponse();
    }

    public FormattedErrorMessage handleExceptionAndTransformMessage(Exception e) {
        BaseRuntimeException finalException = null;
        if (!(e instanceof BaseRuntimeException)) {
           if (e instanceof HttpStatusCodeException) {
                HttpStatusCodeException httpStatusCodeException = (HttpStatusCodeException) e;
                finalException = new POFServiceException(e, httpStatusCodeException.getStatusCode().value());
            } else if (e instanceof HttpRequestMethodNotSupportedException) {
                finalException = new POFServiceException(e, HttpStatus.METHOD_NOT_ALLOWED.value());
            } else if (e instanceof HttpMediaTypeException) {
                finalException = new POFServiceException( e, HttpStatus.UNSUPPORTED_MEDIA_TYPE.value());
            } else if (e instanceof AuthenticationCredentialsNotFoundException) {
                finalException = new POFServiceException( e, HttpStatus.UNAUTHORIZED.value());
            } else if (e instanceof MultipartException) {
                finalException = new POFServiceException( e, HttpStatus.URI_TOO_LONG.value());
            } else if (e instanceof MissingServletRequestParameterException
                    || e instanceof HttpClientErrorException) {
                finalException = new POFServiceException(e, HttpStatus.BAD_REQUEST.value());
            } else {
                finalException = new POFServiceException(e, HttpStatus.INTERNAL_SERVER_ERROR.value());
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
