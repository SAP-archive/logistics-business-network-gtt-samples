package com.sap.gtt.v2.sample.sst.odata.handler;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getNormalizedUri;

import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.FreightUnit;
import com.sap.gtt.v2.sample.sst.odata.service.FreightUnitService;
import java.util.Map;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * {@link FreightUnitODataHandler} is a handler which processes HTTP OData requests for {@link FreightUnit} entity.
 *
 * @author Aliaksandr Miron
 */
@Component
public class FreightUnitODataHandler extends SSTDefaultODataHandler {

    @Autowired
    private FreightUnitService freightUnitService;

    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(
            GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        final String uri = getNormalizedUri(oDataContext);
        final ODataResultList<FreightUnit> freightUnitResultList = freightUnitService.getAllByUri(uri);
        return convertResults(freightUnitResultList);
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        final String uri = getNormalizedUri(oDataContext);
        final FreightUnit freightUnit = freightUnitService.getByUri(uri).orElse(null);
        return ODataUtils.toMap(freightUnit);
    }
}
