package com.sap.gtt.v2.sample.sst.common.constant;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getEventTypeShortName;

import java.util.Arrays;
import javax.validation.constraints.NotNull;

/**
 * {@link ShipmentEventType} contains event types of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity.
 *
 * @author Aliaksandr Miron
 */
public enum ShipmentEventType {

    GATE_IN_START("GateInStart"),
    GATE_IN_END("GateInEnd"),
    GATE_OUT_START("GateOutStart"),
    GATE_OUT_END("GateOutEnd"),
    UNLOADING_START("UnloadingStart"),
    UNLOADING_END("UnloadingEnd"),
    LOCATION_UPDATE("LocationUpdate"),
    DELAY("Delay"),
    STUFFING("Stuffing"),
    RETURN("Return"),
    POPU("POPU"),
    POD("POD"),
    DEPARTURE("Departure"),
    ARRIVAL("Arrival"),
    LOADING_END("LoadingEnd"),
    LOADING_START("LoadingStart"),
    CHECK_IN("CheckIn"),
    DECOUPLING("Decoupling"),
    COUPLING("Coupling"),
    RECEIVE("Receive"),
    UNSTUFFING("Unstuffing"),
    DELIVERED("Delivered"),
    OUT_FOR_DELIVERY("OutForDelivery"),
    OTHER_EVENT("OtherEvent"),
    EXCEPTIONAL_EVENT("ExceptionalEvent");

    private final String value;

    ShipmentEventType(final String value) {
        this.value = value;
    }

    public static boolean containsEventType(@NotNull final String eventType) {
        return Arrays.stream(values())
                .map(ShipmentEventType::getValue)
                .anyMatch(shipmentEventType -> shipmentEventType.equals(getEventTypeShortName(eventType)));
    }

    public String getValue() {
        return value;
    }
}
