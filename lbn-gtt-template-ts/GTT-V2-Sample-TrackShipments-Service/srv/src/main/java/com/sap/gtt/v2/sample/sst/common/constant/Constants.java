package com.sap.gtt.v2.sample.sst.common.constant;

import java.math.BigDecimal;

/**
 * {@link Constants} contains common constants.
 *
 * @author Min Li
 */
public class Constants {

    public static final String GTT_MODEL_NAMESPACE = "com.lbngttsamples.gtt.app.sof";
    public static final String GTT_MODEL_NAMESPACE_WRITE_SERVICE = GTT_MODEL_NAMESPACE + ".sofWriteService";
    public static final String MODEL_NAMESPACE = GTT_MODEL_NAMESPACE + ".sofService";
    public static final String ENTITY_CONTAINER_NAME = "EntityContainer";
    public static final String COMBINED_MODEL_PATH_SEGMENT = "combined-model";
    public static final String ELEMENTS_PATH_SEGMENT = "elements";
    public static final String REST_ROOT_URL = "/sap/logistics/gtt/sample/sst/rest/v1";
    public static final String ODATA_ROOT_URL = "/sap/logistics/gtt/sample/sst/odata/v1";
    public static final String DATE_TIME_PATTERN = "yyyy-MM-dd'T'HH:mm:ss'Z'";
    public static final BigDecimal MAX_LONGITUDE = BigDecimal.valueOf(180);
    public static final BigDecimal MAX_LATITUDE = BigDecimal.valueOf(90);

    private Constants() {
    }
}
