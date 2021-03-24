package com.sap.gtt.v2.sample.pof.exception;


import static org.apache.commons.lang3.StringUtils.isNotBlank;

import com.sap.gtt.v2.sample.pof.utils.SpringContextUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.i18n.LocaleContextHolder;

import java.util.Locale;

public abstract class BaseRuntimeException extends RuntimeException {
    public static final String ERROR_CODE = "ERROR_CODE_BASE";

    private final transient Object[] localizedMsgParams;
    private final String messageCode;
    private Throwable cause;
    private String rootMessage;

    public BaseRuntimeException(String internalMessage, Throwable cause, String messageCode, Object[] localizedMsgParams) {
        super(internalMessage, cause);
        this.messageCode = messageCode;
        this.cause = cause;
        if (localizedMsgParams != null) {
            this.localizedMsgParams = localizedMsgParams.clone();
        } else {
            this.localizedMsgParams = new Object[]{};
        }
    }
    public BaseRuntimeException(String internalMessage, String rootMessage, String messageCode, Object[] localizedMsgParams) {
       super(internalMessage,null);
        this.messageCode = messageCode;
        this.rootMessage = rootMessage;
        if (localizedMsgParams != null) {
            this.localizedMsgParams = localizedMsgParams.clone();
        } else {
            this.localizedMsgParams = new Object[]{};
        }
    }
    public BaseRuntimeException(String messageCode, Object[] localizedMsgParams) {
        this(null,(Throwable) null, messageCode, localizedMsgParams);
    }

    public Object[] getLocalizedMsgParams() {
        return localizedMsgParams.clone();
    }

    public String getMessageCode() {
        return messageCode;
    }

    @Override
    public String getLocalizedMessage() {
        Locale currentRequestedLocale = LocaleContextHolder.getLocale();
        return this.getLocalizedMessage(currentRequestedLocale);

    }

    public String getLocalizedMessage(Locale locale) {
        if (StringUtils.isBlank(this.getMessageCode())) {
            return this.getMessage();
        }
        if (SpringContextUtils.isSpringEnable()) {
            org.springframework.context.MessageSource messageSource = SpringContextUtils.getMessageSource();
            return messageSource.getMessage(this.getMessageCode(), this.getLocalizedMsgParams(), null, locale);
        } else {
            return this.getMessage();
        }
    }

    @Override
    public String getMessage() {
        Locale currentRequestedLocale = LocaleContextHolder.getLocale();
        org.springframework.context.MessageSource messageSource = SpringContextUtils.getMessageSource();
        return messageSource.getMessage(this.getMessageCode(), this.getLocalizedMsgParams(), null, currentRequestedLocale);
       /* String msg = super.getMessage();
        if (!StringUtils.isBlank(msg)) {
            return msg;
        }
        if (!StringUtils.isBlank(this.getMessageCode())) {
            msg = this.getLocalizedMessage(Locale.ENGLISH);
        }

        return msg;*/

    }

    public abstract int getHttpStatus();

    public String getErrorCode() {
        return ERROR_CODE;
    }

    public String rootCauseMessage() {
        if(isNotBlank(rootMessage)) {
            return rootMessage;
        }
        return cause.getMessage();
    }

    public FormattedErrorMessage getFormattedErrorMessage() {
        return new FormattedErrorMessage(this.getErrorCode(), this.getMessage(),this.rootCauseMessage(),this.getHttpStatus());
    }

}
