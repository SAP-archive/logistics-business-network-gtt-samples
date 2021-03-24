package com.sap.gtt.v2.sample.pof.odata.model;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.util.UUID;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.junit.Assert.assertEquals;


public class InboundDeliveryItemTest {

    private static final UUID MOCK_ID = UUID.randomUUID();

    private final Logger logger = LoggerFactory.getLogger(InboundDeliveryItemTest.class);

    @Test
    public void inboundDeliveryItemTest() {
        InboundDeliveryItem inboundDeliveryItem = new InboundDeliveryItem();

        inboundDeliveryItem.setId(MOCK_ID);
        inboundDeliveryItem.setAltKey(EMPTY);
        inboundDeliveryItem.setInboundDeliveryNo(EMPTY);
        inboundDeliveryItem.setItemNo(EMPTY);
        inboundDeliveryItem.setMaterialNumber(EMPTY);
        inboundDeliveryItem.setMaterialDescription(EMPTY);
        inboundDeliveryItem.setOrderQuantity(BigDecimal.ZERO);
        inboundDeliveryItem.setOrderQuantityUoM(EMPTY);
        inboundDeliveryItem.setSupplier(EMPTY);
        inboundDeliveryItem.setPlannedDeliveryDate(Long.MAX_VALUE);
        inboundDeliveryItem.setDocumentDate(Long.MAX_VALUE);
        inboundDeliveryItem.setGrossWeight(BigDecimal.ZERO);
        inboundDeliveryItem.setNetWeight(BigDecimal.ZERO);
        inboundDeliveryItem.setWeightUoM(EMPTY);
        inboundDeliveryItem.setVolume(BigDecimal.ZERO);
        inboundDeliveryItem.setVolumeUoM(EMPTY);
        inboundDeliveryItem.setWarehouseNo(EMPTY);
        inboundDeliveryItem.setWarehouseDescription(EMPTY);
        inboundDeliveryItem.setDoorForWarehouse(EMPTY);
        inboundDeliveryItem.setItemDescription(EMPTY);
        inboundDeliveryItem.setDoorText(EMPTY);
        inboundDeliveryItem.setBillOfLading(EMPTY);
        inboundDeliveryItem.setDangerousGoods(Boolean.FALSE);
        inboundDeliveryItem.setDestination(EMPTY);
        inboundDeliveryItem.setDepartureAddress(EMPTY);
        inboundDeliveryItem.setIncotermsVersion(EMPTY);
        inboundDeliveryItem.setIncotermsLocation(EMPTY);
        inboundDeliveryItem.setDeparture(EMPTY);
        inboundDeliveryItem.setDepartureAddress(EMPTY);
        inboundDeliveryItem.setDepartureEmail(EMPTY);
        inboundDeliveryItem.setDepartureTelephone(EMPTY);
        inboundDeliveryItem.setPlant(EMPTY);
        inboundDeliveryItem.setPlantLocationDescription(EMPTY);
        inboundDeliveryItem.setPurchaseOrderItemId(MOCK_ID);
        inboundDeliveryItem.setLogicalSystem(EMPTY);
        inboundDeliveryItem.setPartyId(EMPTY);
        inboundDeliveryItem.setTrackingIdType(EMPTY);
        inboundDeliveryItem.setProcessStatusCode(EMPTY);
        inboundDeliveryItem.setExecutionStatusCode(EMPTY);
        inboundDeliveryItem.setLastReportedEvent(EMPTY);
        inboundDeliveryItem.setPlannedArrivalTimestamp(Long.MAX_VALUE);
        inboundDeliveryItem.setIncoterms(new Incoterms());
        inboundDeliveryItem.setExecutionStatus(new ExecutionStatus());
        inboundDeliveryItem.setLastEventName(EMPTY);
        inboundDeliveryItem.setLastLocationAltKey(EMPTY);
        inboundDeliveryItem.setLastLocationDescription(EMPTY);
        inboundDeliveryItem.setLastVPLocationTypeCode(EMPTY);
        inboundDeliveryItem.setLastVPLocationType(new VPLocationType());
        inboundDeliveryItem.setInitialPlannedDeliveryDate(Long.MAX_VALUE);
        inboundDeliveryItem.setRevisedPlannedDeliveryDate(Long.MAX_VALUE);

        assertEquals(MOCK_ID, inboundDeliveryItem.getId());
        assertEquals(EMPTY, inboundDeliveryItem.getAltKey());
        assertEquals(EMPTY, inboundDeliveryItem.getInboundDeliveryNo());
        assertEquals(EMPTY, inboundDeliveryItem.getItemNo());
        assertEquals(EMPTY, inboundDeliveryItem.getMaterialNumber());
        assertEquals(EMPTY, inboundDeliveryItem.getMaterialDescription());
        assertEquals(BigDecimal.ZERO, inboundDeliveryItem.getOrderQuantity());
        assertEquals(EMPTY, inboundDeliveryItem.getOrderQuantityUoM());
        assertEquals(EMPTY, inboundDeliveryItem.getSupplier());
        assertEquals(Long.MAX_VALUE, inboundDeliveryItem.getPlannedDeliveryDate().longValue());
        assertEquals(Long.MAX_VALUE, inboundDeliveryItem.getDocumentDate().longValue());
        assertEquals(BigDecimal.ZERO, inboundDeliveryItem.getGrossWeight());
        assertEquals(BigDecimal.ZERO, inboundDeliveryItem.getNetWeight());
        assertEquals(EMPTY, inboundDeliveryItem.getWeightUoM());
        assertEquals(BigDecimal.ZERO, inboundDeliveryItem.getVolume());
        assertEquals(EMPTY, inboundDeliveryItem.getVolumeUoM());
        assertEquals(EMPTY, inboundDeliveryItem.getWarehouseNo());
        assertEquals(EMPTY, inboundDeliveryItem.getWarehouseDescription());
        assertEquals(EMPTY, inboundDeliveryItem.getDoorForWarehouse());
        assertEquals(EMPTY, inboundDeliveryItem.getItemDescription());
        assertEquals(EMPTY, inboundDeliveryItem.getDoorText());
        assertEquals(EMPTY, inboundDeliveryItem.getBillOfLading());
        assertEquals(Boolean.FALSE, inboundDeliveryItem.getDangerousGoods());
        assertEquals(EMPTY, inboundDeliveryItem.getDestination());
        assertEquals(EMPTY, inboundDeliveryItem.getDepartureAddress());
        assertEquals(EMPTY, inboundDeliveryItem.getIncotermsVersion());
        assertEquals(EMPTY, inboundDeliveryItem.getIncotermsLocation());
        assertEquals(EMPTY, inboundDeliveryItem.getDeparture());
        assertEquals(EMPTY, inboundDeliveryItem.getDepartureAddress());
        assertEquals(EMPTY, inboundDeliveryItem.getDepartureEmail());
        assertEquals(EMPTY, inboundDeliveryItem.getDepartureTelephone());
        assertEquals(EMPTY, inboundDeliveryItem.getPlant());
        assertEquals(EMPTY, inboundDeliveryItem.getPlantLocationDescription());
        assertEquals(MOCK_ID, inboundDeliveryItem.getPurchaseOrderItemId());
        assertEquals(EMPTY, inboundDeliveryItem.getLogicalSystem());
        assertEquals(EMPTY, inboundDeliveryItem.getPartyId());
        assertEquals(EMPTY, inboundDeliveryItem.getTrackingIdType());
        assertEquals(EMPTY, inboundDeliveryItem.getProcessStatusCode());
        assertEquals(EMPTY, inboundDeliveryItem.getExecutionStatusCode());
        assertEquals(EMPTY, inboundDeliveryItem.getLastReportedEvent());
        assertEquals(Long.MAX_VALUE, inboundDeliveryItem.getPlannedArrivalTimestamp().longValue());
        assertEquals(EMPTY, inboundDeliveryItem.getLastEventName());
        assertEquals(EMPTY, inboundDeliveryItem.getLastLocationAltKey());
        assertEquals(EMPTY, inboundDeliveryItem.getLastLocationDescription());
        assertEquals(EMPTY, inboundDeliveryItem.getLastVPLocationTypeCode());
        assertEquals(Long.MAX_VALUE, inboundDeliveryItem.getInitialPlannedDeliveryDate().longValue());
        assertEquals(Long.MAX_VALUE, inboundDeliveryItem.getRevisedPlannedDeliveryDate().longValue());

        logger.info("inboundDelivery info: {}", inboundDeliveryItem.toString());
    }
}
