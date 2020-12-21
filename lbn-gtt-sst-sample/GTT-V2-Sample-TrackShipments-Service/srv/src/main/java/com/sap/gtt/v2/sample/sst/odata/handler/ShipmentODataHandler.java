package com.sap.gtt.v2.sample.sst.odata.handler;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getNormalizedUri;

import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import com.sap.gtt.v2.sample.sst.odata.service.ShipmentService;
import java.util.Map;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * {@link ShipmentODataHandler} is a handler which processes HTTP OData requests for {@link Shipment} entity.
 *
 * @author Aliaksandr Miron
 */
@Component
public class ShipmentODataHandler extends SSTDefaultODataHandler {

    @Autowired
    private ShipmentService shipmentService;

    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(
            GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        final String uri = getNormalizedUri(oDataContext);
        final ODataResultList<Shipment> shipmentResultList = shipmentService.getAllByUri(uri);
        return convertResults(shipmentResultList);
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        final String uri = getNormalizedUri(oDataContext);
        final Shipment shipment = shipmentService.getByUri(uri).orElse(null);
        return ODataUtils.toMap(shipment);
    }
}
