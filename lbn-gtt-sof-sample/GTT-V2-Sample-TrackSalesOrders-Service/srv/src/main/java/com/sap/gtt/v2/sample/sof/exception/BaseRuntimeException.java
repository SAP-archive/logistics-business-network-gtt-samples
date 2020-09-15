package com.sap.gtt.v2.sample.sof.exception;


import com.sap.gtt.v2.sample.sof.utils.SpringContextUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.i18n.LocaleContextHolder;

import java.util.Locale;

public abstract class BaseRuntimeException extends RuntimeException {
    public static final String ERROR_CODE = "ERROR_CODE_BASE";

    private final transient Object[] localizedMsgParams;
    private final String messageCode;


    public BaseRuntimeException(String internalMessage, Throwable cause, String messageCode, Object[] localizedMsgParams) {
        super(internalMessage, cause);
        this.messageCode = messageCode;
        if (localizedMsgParams != null) {
            this.localizedMsgParams = localizedMsgParams.clone();
        } else {
            this.localizedMsgParams = new Object[]{};
        }
    }

    public BaseRuntimeException(String messageCode, Object[] localizedMsgParams) {
        this(null, null, messageCode, localizedMsgParams);

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
        String msg = super.getMessage();
        if (!StringUtils.isBlank(msg)) {
            return msg;
        }

        if (!StringUtils.isBlank(this.getMessageCode())) {
            msg = this.getLocalizedMessage(Locale.ENGLISH);
        }

        return msg;

    }

    public abstract int getHttpStatus();

    public String getErrorCode() {
        return ERROR_CODE;
    }

    public FormattedErrorMessage getFormattedErrorMessage() {
        return new FormattedErrorMessage(this.getMessageCode(), this.getLocalizedMessage(), this.getHttpStatus());
    }

}
