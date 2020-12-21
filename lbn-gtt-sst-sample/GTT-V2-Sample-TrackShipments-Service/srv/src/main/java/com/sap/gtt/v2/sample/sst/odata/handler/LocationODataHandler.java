package com.sap.gtt.v2.sample.sst.odata.handler;

import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.service.LocationService;
import java.util.Map;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * {@link LocationODataHandler} is a handler which processes HTTP OData requests for {@link Location} entity.
 *
 * @author Aliaksandr Miron
 */
@Component
public class LocationODataHandler extends SSTDefaultODataHandler {

    @Autowired
    private LocationService locationService;

    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(
            GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        final String uri = SSTUtils.getNormalizedUri(oDataContext);
        final ODataResultList<Location> locationResultList = locationService.getAll(uri);
        return convertResults(locationResultList);
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        final String uri = SSTUtils.getNormalizedUri(oDataContext);
        final Location location = locationService.get(uri);
        return ODataUtils.toMap(location);
    }
}
