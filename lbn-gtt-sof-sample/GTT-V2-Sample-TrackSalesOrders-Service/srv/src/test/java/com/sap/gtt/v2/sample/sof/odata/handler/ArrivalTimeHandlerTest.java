package com.sap.gtt.v2.sample.sof.odata.handler;

import com.sap.gtt.v2.sample.sof.domain.Event;
import com.sap.gtt.v2.sample.sof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.sof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sof.odata.model.ArrivalTime;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.commons.collections.CollectionUtils;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.modules.junit4.PowerMockRunner;

import java.util.*;
import java.util.stream.Collectors;

@RunWith(PowerMockRunner.class)
public class ArrivalTimeHandlerTest {
    private ArrivalTimeOdataHandler arrivalTimeOdataHandler = new ArrivalTimeOdataHandler();
    @Test
    public void testA() {
        arrivalTimeOdataHandler.getPlannedEvent4DeliveryItemUrl(UUID.randomUUID(),"locationAltKey");
    }
    @Test
    public void test() {
        List<ProcessEventDirectory> result = new ArrayList<>();
        List<UUID> uuids = new ArrayList<>();
        for(int i = 0; i<5; i++) {
            uuids.add(UUID.randomUUID());
        }
        List<Event> events = new ArrayList<>();
        for(int i = 0; i<15; i++) {
            Event event = new Event();
            event.setActualBusinessTimestamp(new Long(i));
            events.add(event);
        }

        for(int i = 0; i<15; i++) {
            ProcessEventDirectory processEventDirectory = new ProcessEventDirectory();
            processEventDirectory.setId(UUID.randomUUID());
            processEventDirectory.setPlannedEventId(uuids.get(i%5));
            PlannedEvent plannedEvent = new PlannedEvent();
            processEventDirectory.setPlannedEvent(plannedEvent);
            Event event = new Event();
            event.setActualBusinessTimestamp(new Long(i));
            processEventDirectory.setEvent(event);
            result.add(processEventDirectory);
        }
        List<ArrivalTime> arrivalTimes = arrivalTimeOdataHandler.getArrivalTimes(result);
        Set<Long> actualT = arrivalTimes.stream().map(ArrivalTime::getActualBizTs).collect(Collectors.toSet());
        for(int i =0; i<5; i++) {
            Assert.assertTrue(actualT.contains(new Long(i)));
        }
        Assert.assertTrue(actualT.size()==5);


    }
}
