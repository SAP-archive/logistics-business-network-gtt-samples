package com.sap.gtt.v2.sample.sst.rest.service;

import com.google.gson.JsonObject;
import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * @author Min Li
 */
@Validated
@Service
public class SSTServiceImpl implements SSTService {

    @Value("${HERE_MAP_KEY}")
    private String hereMapKey;

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    @Override
    public String getUiAnnotation() {
        return gttCoreServiceClient.getUiAnnotation();
    }

    @Override
    public String getI18n(@NotNull final String properties) {
        return gttCoreServiceClient.getI18n(properties);
    }

    @Override
    public String getHereMapKey() {
        JsonObject jsonObject = new JsonObject();
        jsonObject.addProperty("key", hereMapKey);
        return jsonObject.toString();
    }
}
