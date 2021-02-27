package com.sap.gtt.v2.sample.sst.rest.service;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;
import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.rest.model.AdmissibleUnplannedEvent;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * @author Min Li
 */
@Validated
@Service
public class ModelServiceImpl implements ModelService {

    public static final String ADMISSIBLE_UNPLANNED_EVENTS = "admissibleUnplannedEvents";

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    @Override
    public List<AdmissibleUnplannedEvent> getUnplannedEventTypesOfTp(@NotNull final String trackedProcess) {
        String response = gttCoreServiceClient.getUnplannedEventTypesOfTp(trackedProcess);
        return parseEventTypesOfTp(response);
    }

    @Override
    public String getEventTypesMetadata(@NotNull final String trackedProcess, @NotNull final String eventType) {
        return gttCoreServiceClient.getEventTypesMetadata(trackedProcess, eventType);
    }

    private List<AdmissibleUnplannedEvent> parseEventTypesOfTp(String response) {
        JsonObject object = new Gson().fromJson(response, JsonObject.class);
        JsonArray array = object.getAsJsonArray(ADMISSIBLE_UNPLANNED_EVENTS);
        Type listType = new TypeToken<ArrayList<AdmissibleUnplannedEvent>>() {
        }.getType();
        return new Gson().fromJson(array, listType);
    }
}
