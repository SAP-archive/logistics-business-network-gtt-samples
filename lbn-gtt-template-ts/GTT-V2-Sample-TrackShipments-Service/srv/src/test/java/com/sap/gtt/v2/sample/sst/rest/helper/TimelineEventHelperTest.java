package com.sap.gtt.v2.sample.sst.rest.helper;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getStringFromResource;
import static java.util.Collections.singletonList;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.odata.helper.DeliveryItemHelper;
import com.sap.gtt.v2.sample.sst.odata.helper.FreightUnitHelper;
import com.sap.gtt.v2.sample.sst.odata.model.DeliveryItem;
import com.sap.gtt.v2.sample.sst.odata.model.FreightUnit;
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
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class TimelineEventHelperTest {

    @Mock
    private ShipmentService shipmentService;
    @Mock
    private DeliveryItemHelper deliveryItemHelper;
    @Mock
    private FreightUnitHelper freightUnitHelper;
    @Mock
    private DeliveryItemConverter deliveryItemConverter;
    @Mock
    private CurrentLocationService currentLocationService;
    @InjectMocks
    private TimelineEventHelper timelineEventHelper;

    @Test
    void fillDeliveryItems_givenTimelineEventsAndShipmentId_shouldSetDeliveryItems() {
        // given
        final String shipmentId = randomUUID().toString();
        final TimelineEvent timelineEvent = new TimelineEvent();
        final PlannedEvent plannedEvent = new PlannedEvent();
        plannedEvent.setEventStatusCode("TEST");
        plannedEvent.setEventType("com.lbngttsamples.gtt.app.sof.Shipment.Arrival");
        plannedEvent.setLocationAltKey("testAltKey");
        plannedEvent.setEventMatchKey("testEventMatchKey");
        timelineEvent.setPlannedEvent(plannedEvent);
        final String shipmentJson = getStringFromResource("/odata/shipment.json");
        final Shipment shipment = ODataUtils.readEntity(shipmentJson, Shipment.class);
        final String deliveryItemsJson = getStringFromResource("/odata/delivery-items.json");
        final List<DeliveryItem> deliveryItems = ODataUtils.readEntitySet(deliveryItemsJson, DeliveryItem.class).getResults();
        final String freightUnitsJson = getStringFromResource("/odata/freight-units.json");
        final List<FreightUnit> freightUnits = ODataUtils.readEntitySet(freightUnitsJson, FreightUnit.class).getResults();

        when(shipmentService.getByUri(contains("/Shipment"))).thenReturn(Optional.of(shipment));
        when(deliveryItemConverter.fromDeliveryItems(anyList())).thenReturn(singletonList(new DeliveryItemDto()));
        when(deliveryItemHelper.retrieveFromShipment(shipment)).thenReturn(deliveryItems);
        when(freightUnitHelper.retrieveFromShipment(shipment)).thenReturn(freightUnits);

        // when-then
        assertDoesNotThrow(() -> timelineEventHelper.fillDeliveryItems(singletonList(timelineEvent), shipmentId));
        assertThat(timelineEvent).extracting(TimelineEvent::getDeliveryItems).isNotNull();
    }

    @Test
    void fillETAData_givenTimelineEventsAndShipmentId_shouldSetEtaData() {
        // given
        final String shipmentId = randomUUID().toString();
        final TimelineEvent timelineEvent = new TimelineEvent();
        final PlannedEvent plannedEvent = new PlannedEvent();
        plannedEvent.setEventType("com.lbngttsamples.gtt.app.sof.Shipment.Arrival");
        plannedEvent.setEventMatchKey("TEST");
        timelineEvent.setPlannedEvent(plannedEvent);
        final CurrentLocation currentLocation = new CurrentLocation();
        final EstimatedArrival estimatedArrival = new EstimatedArrival();
        estimatedArrival.setStopId("TEST");
        currentLocation.setEstimatedArrival(singletonList(estimatedArrival));

        when(currentLocationService.getByTrackedProcessId(shipmentId)).thenReturn(Optional.of(currentLocation));

        // when-then
        assertDoesNotThrow(() -> timelineEventHelper.fillETAData(singletonList(timelineEvent), shipmentId));
        assertThat(timelineEvent).extracting(TimelineEvent::getEstimatedArrival).isNotNull();
    }
}
