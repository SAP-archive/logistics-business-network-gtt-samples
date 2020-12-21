package com.sap.gtt.v2.sample.sst.rest.service;

import javax.validation.constraints.NotNull;

/**
 * {@link SSTService} is a common service for SST properties.
 *
 * @author Min Li
 */
public interface SSTService {

    /**
     * Retrieves UI annotation.
     *
     * @return UI annotation
     */
    String getUiAnnotation();

    /**
     * Retrieves i18m properties.
     *
     * @param properties - provided properties name
     * @return i18n properties
     */
    String getI18n(@NotNull final String properties);

    /**
     * Retrieves map key.
     *
     * @return map key
     */
    String getHereMapKey();
}
