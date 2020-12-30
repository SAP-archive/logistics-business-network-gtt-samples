package com.sap.gtt.v2.sample.pof.odata.handler;

import com.sap.gtt.v2.sample.pof.domain.Location;
import com.sap.gtt.v2.sample.pof.odata.model.LocationDTO;
import com.sap.gtt.v2.sample.pof.service.MapService;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;

@RunWith(PowerMockRunner.class)
@PrepareForTest(POFUtils.class)
public class LocationDTOHandlerTest {
    @Mock
    private GTTCoreServiceClient gttCoreServiceClient;

    @InjectMocks
    private POFLocationODataHandler locationDTOOdataHandler;
    @Mock
    private MapService mapService;

    @Before
    public void setup() {
        ReflectionTestUtils.setField(locationDTOOdataHandler, "gttCoreServiceClient", gttCoreServiceClient);
    }

        @Test
    public void getLocationDTO() {
        String locationAltKey = "";
        Mockito.when(gttCoreServiceClient.getLocation(locationAltKey)).thenReturn(new Location());
        Mockito.when(mapService.getLocationDetail(any())).thenReturn(new LocationDTO());
        locationDTOOdataHandler.getLocation(locationAltKey);
    }

    @Test
    public void getLocationDTOs() {
        Set<String> locationAltKeys = new HashSet<>();
        List<Location> list = new LinkedList<>();
        for(int i =0; i<5; i++) {
            locationAltKeys.add(i+"");
            Location location = new Location();
            location.setLocationAltKey(i+"");
            list.add(location);
            LocationDTO locationDTO = new LocationDTO();
            locationDTO.setLocationAltKey(i+"");
            Mockito.when(mapService.getLocationDetail(location)).thenReturn(locationDTO);
        }
        Mockito.when(gttCoreServiceClient.getLocations(locationAltKeys)).thenReturn(list);

        Map<String,LocationDTO> map = locationDTOOdataHandler.getLocations(locationAltKeys);
        for(int  i = 0; i<5; i++) {
            Assert.assertTrue(map.containsKey(i+""));
            Assert.assertEquals(i+"",map.get(i+"").getLocationAltKey());
        }
    }
}
