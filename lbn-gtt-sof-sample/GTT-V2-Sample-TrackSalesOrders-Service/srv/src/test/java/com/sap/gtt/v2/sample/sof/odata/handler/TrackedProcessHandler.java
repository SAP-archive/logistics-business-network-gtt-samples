package com.sap.gtt.v2.sample.sof.odata.handler;

import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.TrackedProcess;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetCountUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
@Profile("test")
public class TrackedProcessHandler extends SOFDefaultODataHandler {
    private static final Logger logger = LoggerFactory.getLogger(TrackedProcessHandler.class);

    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        // retrieve json string from gtt read service
        String json = SOFUtils.getStringFromResource("/odata/tracked-processes.json");
        ODataResultList<TrackedProcess> res = ODataUtils.readEntitySet(json, TrackedProcess.class);
        logger.info("tp list deserialized, size: {}", res.getResults().size());

        return convertResults(res);
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        // retrieve json string from gtt read service
        String json = SOFUtils.getStringFromResource("/odata/tracked-process-single.json");

        TrackedProcess tp = ODataUtils.readEntity(json, TrackedProcess.class);

        logger.info("tp deserialized: {}", tp.getId());

        return ODataUtils.toMap(tp);
    }

    @Override
    public Integer handleCountEntitySet(GetEntitySetCountUriInfo uriInfo, ODataContext oDataContext) {
        String json = SOFUtils.getStringFromResource("/odata/tracked-processes.json");

        ODataResultList<TrackedProcess> res = ODataUtils.readEntitySet(json, TrackedProcess.class);
        return res.getResults().size();
    }
}
