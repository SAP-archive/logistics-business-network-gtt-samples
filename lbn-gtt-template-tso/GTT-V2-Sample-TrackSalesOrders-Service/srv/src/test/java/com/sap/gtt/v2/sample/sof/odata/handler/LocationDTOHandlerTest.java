package com.sap.gtt.v2.sample.sof.odata.handler;

import com.sap.gtt.v2.sample.sof.App;
import com.sap.gtt.v2.sample.sof.domain.Location;
import com.sap.gtt.v2.sample.sof.odata.model.LocationDTO;
import com.sap.gtt.v2.sample.sof.rest.controller.service.MapService;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;

@RunWith(PowerMockRunner.class)
public class LocationDTOHandlerTest {
    @Mock
    private GTTCoreServiceClient client;
    @InjectMocks
    private LocationDTOOdataHandler locationDTOOdataHandler;
    @Mock
    private MapService mapService;
    @Test
    public void getLocationDTO() {
        String locationAltKey = "";
        Mockito.when(client.getLocation(locationAltKey)).thenReturn(new Location());
        Mockito.when(mapService.getLocationDetail(any())).thenReturn(new LocationDTO());
        locationDTOOdataHandler.getLocationDTO(locationAltKey);
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
        Mockito.when(client.getLocations(locationAltKeys)).thenReturn(list);

        Map<String,LocationDTO> map = locationDTOOdataHandler.getLocationDTOs(locationAltKeys);
        for(int  i = 0; i<5; i++) {
            Assert.assertTrue(map.containsKey(i+""));
            Assert.assertEquals(i+"",map.get(i+"").getLocationAltKey());
        }
    }
}
