package com.sap.gtt.v2.sample.sof.odata.model;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntityType;
import org.apache.olingo.odata2.api.annotation.edm.EdmKey;
import org.apache.olingo.odata2.api.annotation.edm.EdmProperty;

import java.util.UUID;

import static com.sap.gtt.v2.sample.sof.constant.Constants.MODEL_NAMESPACE;

@EdmEntityType(namespace = MODEL_NAMESPACE)
@EdmEntitySet(name = "Event", container = Constants.ENTITY_CONTAINER_NAME)
public class Event {

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }
}
