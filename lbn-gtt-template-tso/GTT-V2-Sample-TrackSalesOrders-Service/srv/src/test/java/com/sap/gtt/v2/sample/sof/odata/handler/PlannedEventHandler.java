package com.sap.gtt.v2.sample.sof.odata.handler;

import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sof.odata.model.TrackedProcess;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
@Profile("test")
public class PlannedEventHandler extends SOFDefaultODataHandler {

    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        String json = SOFUtils.getStringFromResource("/odata/tracked-process-single.json");
        TrackedProcess tp = ODataUtils.readEntity(json, TrackedProcess.class);
        ODataResultList<PlannedEvent> res = new ODataResultList<>();
        res.setResults(tp.getPlannedEvents());
        return convertResults(res);
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        return null;
    }
}
