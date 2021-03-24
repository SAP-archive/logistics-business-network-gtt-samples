package com.sap.gtt.v2.sample.pof.odata.handler;

import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.ArrivalTime;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.ODataUtils;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
public class ArrivalTimeODataHandlerTest {

    private static final UUID MOCK_DELIVERY_ID = UUID.randomUUID();

    private final ArrivalTimeODataHandler arrivalTimeODataHandler = new ArrivalTimeODataHandler();

    @Mock
    private GTTCoreServiceClient gttCoreServiceClient;


    @Before
    public void init() {
        ReflectionTestUtils.setField(arrivalTimeODataHandler, "gttCoreServiceClient", gttCoreServiceClient);
    }

    @Test
    public void getArrivalTimesByInboundDeliveryItemsCollection() {
        String jsonData = POFUtils.getStringFromResource("odata/planned-events_arrival_times.json");
        ODataResultList<ProcessEventDirectory> processEventDirectoryODataResultList = ODataUtils.readEntitySet(jsonData, ProcessEventDirectory.class);
        List<InboundDeliveryItem> mockInboundDeliveryItems = getMockInboundDeliveryItems();

        when(gttCoreServiceClient.readEntitySetAll(contains("/ProcessEventDirectory"), eq(ProcessEventDirectory.class)))
                .thenReturn(processEventDirectoryODataResultList);

        Map<UUID, List<ArrivalTime>> arrivalTimes4DeliveryItems = arrivalTimeODataHandler.getArrivalTimes4DeliveryItems(mockInboundDeliveryItems);

        List<ProcessEventDirectory> expected = processEventDirectoryODataResultList.getResults();
        List<ArrivalTime> actual = arrivalTimes4DeliveryItems.get(MOCK_DELIVERY_ID);

        assertEquals(mockInboundDeliveryItems.size(), arrivalTimes4DeliveryItems.size());
        assertEquals(expected.size(), actual.size());
        assertEquals(
                expected.stream().map(ped -> ped.getEvent().getActualBusinessTimestamp()).collect(Collectors.toSet()),
                actual.stream().map(ArrivalTime::getActualBizTs).collect(Collectors.toSet())
        );
        assertEquals(
                expected.stream().map(ped -> ped.getPlannedEvent().getPlannedBizTsEarliest()).collect(Collectors.toSet()),
                actual.stream().map(ArrivalTime::getPlannedBizTsEarliest).collect(Collectors.toSet())

        );
    }

    private List<InboundDeliveryItem> getMockInboundDeliveryItems() {
        InboundDeliveryItem inboundDeliveryItem = new InboundDeliveryItem();
        inboundDeliveryItem.setId(MOCK_DELIVERY_ID);
        inboundDeliveryItem.setPartyId(EMPTY);
        inboundDeliveryItem.setLogicalSystem(EMPTY);
        inboundDeliveryItem.setPlantLocationTypeCode(EMPTY);
        inboundDeliveryItem.setPlant(EMPTY);
        return Collections.singletonList(inboundDeliveryItem);
    }
}
