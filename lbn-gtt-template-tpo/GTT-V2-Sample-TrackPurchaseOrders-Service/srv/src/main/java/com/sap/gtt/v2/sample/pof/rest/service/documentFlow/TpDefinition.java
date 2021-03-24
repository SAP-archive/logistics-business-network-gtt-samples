package com.sap.gtt.v2.sample.pof.rest.service.documentFlow;

import com.sap.gtt.v2.sample.pof.exception.POFServiceException;
import java.lang.reflect.Method;
import java.util.Optional;
import java.util.UUID;
import org.springframework.http.HttpStatus;
import org.springframework.util.ReflectionUtils;

public class TpDefinition {
    private UUID id;
    private String trackingIdType;
    private Object tp;

    public TpDefinition(Object tp) {
        this.tp = tp;
        this.id = getTPId(tp);
        this.trackingIdType = getTPTrackingType(tp);
    }

    private static UUID getTPId(Object tp) {
        Method method = ReflectionUtils.findMethod(tp.getClass(), "getId");
        return (UUID) Optional.ofNullable(method).map(m -> ReflectionUtils.invokeMethod(m, tp))
                .orElseThrow(() -> new POFServiceException(POFServiceException.ERROR_CODE,"Unprocessable EDM type",
                    HttpStatus.INTERNAL_SERVER_ERROR.value()));
    }

    private static String getTPTrackingType(Object tp) {
        Method method = ReflectionUtils.findMethod(tp.getClass(), "getTrackingIdType");
        return (String) Optional.ofNullable(method).map(m -> ReflectionUtils.invokeMethod(m, tp))
                .orElseThrow(() -> new POFServiceException(POFServiceException.ERROR_CODE,"Unprocessable EDM type",HttpStatus.INTERNAL_SERVER_ERROR.value()));
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getTrackingIdType() {
        return trackingIdType;
    }

    public void setTrackingIdType(String trackingIdType) {
        this.trackingIdType = trackingIdType;
    }

    public Object getTp() {
        return tp;
    }

    public void setTp(Object tp) {
        this.tp = tp;
    }
}
