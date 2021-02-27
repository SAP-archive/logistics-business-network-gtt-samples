package com.sap.gtt.v2.sample.sst.rest.helper;

import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventType.ARRIVAL;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getEventTypeShortName;
import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.EXPAND;
import static java.lang.String.format;
import static java.util.Collections.emptyList;
import static java.util.Objects.nonNull;
import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.sst.odata.helper.DeliveryItemHelper;
import com.sap.gtt.v2.sample.sst.odata.helper.FreightUnitHelper;
import com.sap.gtt.v2.sample.sst.odata.model.DeliveryItem;
import com.sap.gtt.v2.sample.sst.odata.model.FreightUnit;
import com.sap.gtt.v2.sample.sst.odata.model.FreightUnitItem;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import com.sap.gtt.v2.sample.sst.odata.service.ShipmentService;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.EstimatedArrival;
import com.sap.gtt.v2.sample.sst.rest.model.TimelineEvent;
import com.sap.gtt.v2.sample.sst.rest.model.converter.DeliveryItemConverter;
import com.sap.gtt.v2.sample.sst.rest.model.dto.DeliveryItemDto;
import com.sap.gtt.v2.sample.sst.rest.service.CurrentLocationService;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.util.UriComponentsBuilder;

/**
 * {@link TimelineEventHelper} is a helper class for {@link TimelineEvent} entities.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class TimelineEventHelper {

    private static final String SHIPMENT_ENDPOINT = "/Shipment";
    private static final String DELIVERY_PARAM = "delivery";
    private static final String DELIVERY_TPS_PARAM = "deliveryTPs";
    private static final String DELIVERY_ITEM_PARAM = "deliveryItem";
    private static final String DELIVERY_ITEMS_PARAM = "deliveryItems";
    private static final String PLANNED_EVENTS_PARAM = "plannedEvents";
    private static final String FREIGHT_UNIT_TPS_PARAM = "freightUnitTPs";
    private static final String FREIGHT_UNIT_PARAM = "freightUnit";
    private static final String FREIGHT_UNIT_ITEMS_PARAM = "freightUnitItems";

    @Autowired
    private DeliveryItemConverter deliveryItemConverter;

    @Autowired
    private CurrentLocationService currentLocationService;

    @Autowired
    private ShipmentService shipmentService;

    @Autowired
    private DeliveryItemHelper deliveryItemHelper;

    @Autowired
    private FreightUnitHelper freightUnitHelper;

    /**
     * Retrieves and sets {@link DeliveryItem} and {@link FreightUnitItem} entities into {@link TimelineEvent}.
     *
     * @param timelineEvents - {@link TimelineEvent} entities
     * @param shipmentId     - UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity
     */
    public void fillDeliveryItems(@NotNull final List<TimelineEvent> timelineEvents, @NotNull final String shipmentId) {
        final Optional<Shipment> shipmentOpt = getShipment(shipmentId);
        final List<DeliveryItem> deliveryItems = shipmentOpt.map(deliveryItemHelper::retrieveFromShipment).orElse(emptyList());
        final List<FreightUnit> freightUnits = shipmentOpt.map(freightUnitHelper::retrieveFromShipment).orElse(emptyList());
        updateWithDeliveryItems(timelineEvents, deliveryItems);
        updateWithFreightUnitItems(timelineEvents, freightUnits);
    }

    /**
     * Fills {@link TimelineEvent} with ETA data.
     *
     * @param timelineEvents   - {@link TimelineEvent} entities
     * @param trackedProcessId - UUID of tracked process
     */
    public void fillETAData(@NotNull final List<TimelineEvent> timelineEvents, @NotNull final String trackedProcessId) {
        final Optional<CurrentLocation> currentLocationOpt = currentLocationService.getByTrackedProcessId(trackedProcessId);
        currentLocationOpt.map(CurrentLocation::getEstimatedArrival)
                .ifPresent(estimatedArrivals -> filterAndSetETAData(timelineEvents, estimatedArrivals));
    }

    private void filterAndSetETAData(final List<TimelineEvent> timelineEvents, final List<EstimatedArrival> estimatedArrivals) {
        timelineEvents.stream()
                .filter(timelineEvent -> nonNull(timelineEvent.getPlannedEvent()))
                .filter(timelineEvent -> isArrivalEventType(timelineEvent.getPlannedEvent().getEventType()))
                .forEach(timelineEvent -> updateWithETAData(timelineEvent, estimatedArrivals));
    }

    private boolean isArrivalEventType(final String eventType) {
        return ARRIVAL.getValue().equals(getEventTypeShortName(eventType));
    }

    private void updateWithETAData(final TimelineEvent timelineEvent, final List<EstimatedArrival> estimatedArrivals) {
        final String plannedEventMatchKey = timelineEvent.getPlannedEvent().getEventMatchKey();
        final Optional<EstimatedArrival> matchedEstimatedArrivalOpt =
                findMatchedEstimatedArrival(estimatedArrivals, plannedEventMatchKey);
        matchedEstimatedArrivalOpt.ifPresent(timelineEvent::setEstimatedArrival);
    }

    private Optional<EstimatedArrival> findMatchedEstimatedArrival(
            final List<EstimatedArrival> estimatedArrivals, final String plannedEventMatchKey) {
        return estimatedArrivals.stream()
                .filter(estimatedArrival -> hasEqualPlannedEvents(plannedEventMatchKey, estimatedArrival))
                .findFirst();
    }

    private boolean hasEqualPlannedEvents(final String plannedEventMatchKey, final EstimatedArrival estimatedArrival) {
        final String stopId = estimatedArrival.getStopId();
        return nonNull(plannedEventMatchKey) && plannedEventMatchKey.equals(stopId);
    }

    private void updateWithFreightUnitItems(final List<TimelineEvent> timelineEvents, final List<FreightUnit> freightUnits) {
        timelineEvents.forEach(timelineEvent -> setFreightUnitItemsIfPresent(timelineEvent, freightUnits));
    }

    private void setFreightUnitItemsIfPresent(final TimelineEvent timelineEvent, final List<FreightUnit> freightUnits) {
        final Optional<PlannedEvent> plannedEventOpt = Optional.ofNullable(timelineEvent.getPlannedEvent());
        plannedEventOpt.ifPresent(plannedEvent -> setFreightUnitItems(timelineEvent, freightUnits, plannedEvent));
    }

    private void setFreightUnitItems(
            final TimelineEvent timelineEvent, final List<FreightUnit> freightUnits, final PlannedEvent plannedEvent) {
        final List<FreightUnit> matchingFreightUnits = findMatchingByPlannedEventsFreightUnits(plannedEvent, freightUnits);
        updateFreightUnitItemsEventStatusCodes(matchingFreightUnits, plannedEvent);
        updateFreightUnitItemsAltKeys(matchingFreightUnits);
        updateFreightUnitItemsEventMatchKeys(matchingFreightUnits);
        final List<FreightUnitItem> freightUnitItems = retrieveFreightUnitItems(matchingFreightUnits);
        final List<DeliveryItemDto> deliveryItemDtos = deliveryItemConverter.fromFreightUnitItems(freightUnitItems);
        final List<DeliveryItemDto> existingDeliveryItems = timelineEvent.getDeliveryItems();
        existingDeliveryItems.addAll(deliveryItemDtos);
    }

    private void updateFreightUnitItemsEventMatchKeys(final List<FreightUnit> freightUnits) {
        freightUnits.forEach(this::updateFreightUnitEventMatchKey);
    }

    private void updateFreightUnitEventMatchKey(final FreightUnit freightUnit) {
        final String trackingId = freightUnit.getTrackingId();
        final List<FreightUnitItem> freightUnitItems = freightUnit.getFreightUnitItems();
        freightUnitItems.forEach(freightUnitItem -> freightUnitItem.setEventMatchKey(trackingId));
    }

    private void updateFreightUnitItemsAltKeys(final List<FreightUnit> freightUnits) {
        freightUnits.forEach(this::updateFreightUnitItemsAltKey);
    }

    private void updateFreightUnitItemsAltKey(final FreightUnit freightUnit) {
        final String altKey = freightUnit.getAltKey();
        final List<FreightUnitItem> freightUnitItems = freightUnit.getFreightUnitItems();
        freightUnitItems.forEach(freightUnitItem -> freightUnitItem.setFreightUnitAltKey(altKey));
    }

    private List<FreightUnitItem> retrieveFreightUnitItems(final List<FreightUnit> freightUnits) {
        return freightUnits.stream()
                .map(FreightUnit::getFreightUnitItems)
                .flatMap(List::stream)
                .collect(toList());
    }

    private void updateFreightUnitItemsEventStatusCodes(final List<FreightUnit> freightUnits, final PlannedEvent plannedEvent) {
        freightUnits.forEach(freightUnit -> updateFreightUnitItemsEventStatusCode(freightUnit, plannedEvent));
    }

    private void updateFreightUnitItemsEventStatusCode(final FreightUnit freightUnit, final PlannedEvent plannedEvent) {
        final List<PlannedEvent> freightUnitPlannedEvents = freightUnit.getPlannedEvents();
        final Optional<PlannedEvent> matchingPlannedEventOpt = findMatchingPlannedEventForFreightUnit(freightUnitPlannedEvents, plannedEvent);
        matchingPlannedEventOpt.map(PlannedEvent::getEventStatusCode)
                .ifPresent(eventStatusCode -> setFreightUnitItemsEventStatusCode(freightUnit, eventStatusCode));
    }

    private Optional<PlannedEvent> findMatchingPlannedEventForFreightUnit(
            final List<PlannedEvent> plannedEvents, final PlannedEvent timelinePlannedEvent) {
        return plannedEvents.stream()
                .filter(plannedEvent -> plannedEventsEqualForFreightUnit(timelinePlannedEvent, plannedEvent))
                .findFirst();
    }

    private void setFreightUnitItemsEventStatusCode(final FreightUnit freightUnit, final String eventStatusCode) {
        final List<FreightUnitItem> freightUnitItems = freightUnit.getFreightUnitItems();
        freightUnitItems.forEach(freightUnitItem -> freightUnitItem.setEventStatusCode(eventStatusCode));
    }

    private List<FreightUnit> findMatchingByPlannedEventsFreightUnits(
            final PlannedEvent plannedEvent, final List<FreightUnit> freightUnits) {
        return freightUnits.stream()
                .filter(Objects::nonNull)
                .filter(freightUnit -> freightUnitHasEqualPlannedEvent(freightUnit.getPlannedEvents(), plannedEvent))
                .collect(toList());
    }

    private boolean freightUnitHasEqualPlannedEvent(final List<PlannedEvent> plannedEvents, final PlannedEvent timelinePlannedEvent) {
        return plannedEvents.stream()
                .anyMatch(plannedEvent -> plannedEventsEqualForFreightUnit(timelinePlannedEvent, plannedEvent));
    }

    private void updateWithDeliveryItems(final List<TimelineEvent> timelineEvents, final List<DeliveryItem> deliveryItems) {
        timelineEvents.forEach(timelineEvent -> setDeliveryItemsIfPresent(timelineEvent, deliveryItems));
    }

    private void setDeliveryItemsIfPresent(final TimelineEvent timelineEvent, final List<DeliveryItem> deliveryItems) {
        final Optional<PlannedEvent> plannedEventOpt = Optional.ofNullable(timelineEvent.getPlannedEvent());
        plannedEventOpt.ifPresent(plannedEvent -> setDeliveryItems(timelineEvent, deliveryItems, plannedEvent));
    }

    private void setDeliveryItems(
            final TimelineEvent timelineEvent, final List<DeliveryItem> deliveryItems, final PlannedEvent plannedEvent) {
        final List<DeliveryItem> matchingDeliveryItems = findMatchingByPlannedEventsDeliveryItems(plannedEvent, deliveryItems);
        updateDeliveryItemsEventStatusCodes(matchingDeliveryItems, plannedEvent);
        updateDeliveryItemsEventMatchKeys(matchingDeliveryItems, timelineEvent);
        final List<DeliveryItemDto> deliveryItemDtos = deliveryItemConverter.fromDeliveryItems(matchingDeliveryItems);
        timelineEvent.setDeliveryItems(deliveryItemDtos);
    }

    private void updateDeliveryItemsEventStatusCodes(final List<DeliveryItem> deliveryItems, final PlannedEvent plannedEvent) {
        deliveryItems.forEach(deliveryItem -> updateDeliveryItemsEventStatusCode(deliveryItem, plannedEvent));
    }

    private void updateDeliveryItemsEventMatchKeys(final List<DeliveryItem> deliveryItems, final TimelineEvent timelineEvent) {
        final String eventMatchKey = timelineEvent.getEventMatchKey();
        deliveryItems.forEach(deliveryItem -> deliveryItem.setEventMatchKey(eventMatchKey));
    }

    private void updateDeliveryItemsEventStatusCode(final DeliveryItem deliveryItem, final PlannedEvent plannedEvent) {
        final List<PlannedEvent> deliveryItemsPlannedEvents = deliveryItem.getPlannedEvents();
        final Optional<PlannedEvent> matchingPlannedEventOpt = findMatchingPlannedEventForDeliveryItem(deliveryItemsPlannedEvents, plannedEvent);
        matchingPlannedEventOpt.map(PlannedEvent::getEventStatusCode).ifPresent(deliveryItem::setEventStatusCode);
    }

    private Optional<PlannedEvent> findMatchingPlannedEventForDeliveryItem(
            final List<PlannedEvent> plannedEvents, final PlannedEvent timelinePlannedEvent) {
        return plannedEvents.stream()
                .filter(plannedEvent -> plannedEventsEqualForDeliveryItem(timelinePlannedEvent, plannedEvent))
                .findFirst();
    }

    private List<DeliveryItem> findMatchingByPlannedEventsDeliveryItems(
            final PlannedEvent plannedEvent, final List<DeliveryItem> deliveryItems) {
        return deliveryItems.stream()
                .filter(Objects::nonNull)
                .filter(deliveryItem -> deliveryItemHasEqualPlannedEvent(deliveryItem.getPlannedEvents(), plannedEvent))
                .collect(toList());
    }

    private boolean deliveryItemHasEqualPlannedEvent(final List<PlannedEvent> plannedEvents, final PlannedEvent timelinePlannedEvent) {
        return plannedEvents.stream()
                .anyMatch(plannedEvent -> plannedEventsEqualForDeliveryItem(timelinePlannedEvent, plannedEvent));
    }

    private boolean plannedEventsEqualForDeliveryItem(final PlannedEvent timelinePlannedEvent, final PlannedEvent plannedEvent) {
        return hasEqualPlannedEventTypes(timelinePlannedEvent, plannedEvent)
                && hasEqualEventMatchKeys(timelinePlannedEvent, plannedEvent);
    }

    private boolean plannedEventsEqualForFreightUnit(final PlannedEvent timelinePlannedEvent, final PlannedEvent plannedEvent) {
        return hasEqualPlannedEventTypes(timelinePlannedEvent, plannedEvent)
                && hasEqualLocationAltKeys(timelinePlannedEvent, plannedEvent);
    }

    private boolean hasEqualPlannedEventTypes(final PlannedEvent timelinePlannedEvent, final PlannedEvent plannedEvent) {
        final String timelinePlannedEventType = timelinePlannedEvent.getEventType();
        final String plannedEventType = plannedEvent.getEventType();
        return nonNull(timelinePlannedEventType) && timelinePlannedEventType.equals(plannedEventType);
    }

    private boolean hasEqualEventMatchKeys(final PlannedEvent timelinePlannedEvent, final PlannedEvent plannedEvent) {
        final String timelinePlannedEventMatchKey = timelinePlannedEvent.getEventMatchKey();
        final String plannedEventMatchKey = plannedEvent.getEventMatchKey();
        return nonNull(timelinePlannedEventMatchKey) && timelinePlannedEventMatchKey.equals(plannedEventMatchKey);
    }

    private boolean hasEqualLocationAltKeys(final PlannedEvent timelinePlannedEvent, final PlannedEvent plannedEvent) {
        final String timelinePlannedEventLocationAltKey = timelinePlannedEvent.getLocationAltKey();
        final String plannedEventLocationAltKey = plannedEvent.getLocationAltKey();
        return nonNull(timelinePlannedEventLocationAltKey) && timelinePlannedEventLocationAltKey.equals(plannedEventLocationAltKey);
    }

    private Optional<Shipment> getShipment(final String shipmentId) {
        final String uri = buildUriByShipmentId(shipmentId);
        return shipmentService.getByUri(uri);
    }

    private String buildUriByShipmentId(final String shipmentId) {
        final String deliveryItemsParam = format("%s/%s/%s/%s",
                DELIVERY_TPS_PARAM, DELIVERY_PARAM, DELIVERY_ITEMS_PARAM, PLANNED_EVENTS_PARAM);
        final String freightUnitItemsParam =
                format("%s/%s/%s/%s", FREIGHT_UNIT_TPS_PARAM, FREIGHT_UNIT_PARAM, FREIGHT_UNIT_ITEMS_PARAM, DELIVERY_ITEM_PARAM);
        final String freightUnitPlannedEventParam =
                format("%s/%s/%s", FREIGHT_UNIT_TPS_PARAM, FREIGHT_UNIT_PARAM, PLANNED_EVENTS_PARAM);
        final String expandParam =
                format("%s,%s,%s", deliveryItemsParam, freightUnitItemsParam, freightUnitPlannedEventParam);
        return UriComponentsBuilder
                .fromPath(format("%s(guid'%s')", SHIPMENT_ENDPOINT, shipmentId))
                .queryParam(EXPAND, expandParam)
                .build()
                .encode()
                .toUriString();
    }
}
