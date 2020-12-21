package com.sap.gtt.v2.sample.pof.utils;

import com.sap.gtt.v2.sample.pof.domain.Location;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

public class LocationUtilsTest {

    @Test
    public void test() {
        List<Location> locations = Arrays.asList(
                new Location() {{
                    setLocationAltKey("location1");
                }},
                new Location() {{
                    setLocationAltKey("location2");
                }});
        Optional<Location> location1 = LocationUtils.retrieveLocationByAltKey("location1", locations);

        Assert.assertTrue(location1.isPresent());
        Assert.assertEquals("location1", location1.get().getLocationAltKey());
    }
}
