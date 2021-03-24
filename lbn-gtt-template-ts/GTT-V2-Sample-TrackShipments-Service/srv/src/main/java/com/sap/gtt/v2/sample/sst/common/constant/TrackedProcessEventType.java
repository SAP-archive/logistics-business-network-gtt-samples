package com.sap.gtt.v2.sample.sst.common.constant;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getEventTypeShortName;

import java.util.Arrays;

/**
 * {@link TrackedProcessEventType} contains event types of tracked process.
 *
 * @author Aliaksandr Miron
 */
public enum TrackedProcessEventType {

    GATE_IN_START("GateInStart"),
    GATE_IN_END("GateInEnd"),
    GATE_OUT_START("GateOutStart"),
    GATE_OUT_END("GateOutEnd"),
    UNLOADING_START("UnloadingStart"),
    UNLOADING_END("UnloadingEnd"),
    LOCATION_UPDATE("LocationUpdate"),
    LOCATION_UPDATE_NEW("LocationUpdateNew"),
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

    TrackedProcessEventType(final String value) {
        this.value = value;
    }

    public static boolean containsEventType(final String eventType) {
        return Arrays.stream(values())
                .map(TrackedProcessEventType::getValue)
                .anyMatch(trackedProcessEventType -> trackedProcessEventType.equals(getEventTypeShortName(eventType)));
    }

    public String getValue() {
        return value;
    }
}
