package com.sap.gtt.v2.sample.pof.odata.callback;

import com.sap.gtt.v2.sample.pof.exception.POFServiceException;
import org.apache.olingo.odata2.api.ODataCallback;
import org.apache.olingo.odata2.api.edm.EdmException;
import org.apache.olingo.odata2.api.edm.EdmNavigationProperty;
import org.apache.olingo.odata2.api.ep.EntityProviderWriteProperties;
import org.apache.olingo.odata2.api.ep.callback.OnWriteFeedContent;
import org.apache.olingo.odata2.api.ep.callback.WriteFeedCallbackContext;
import org.apache.olingo.odata2.api.ep.callback.WriteFeedCallbackResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ExpandEntitySetCallback implements OnWriteFeedContent {

    private static final Logger logger = LoggerFactory.getLogger(ExpandEntitySetCallback.class);

    private URI serviceRoot;

    public ExpandEntitySetCallback() {
        super();
    }

    public void setServiceRoot(URI serviceRoot) {
        this.serviceRoot = serviceRoot;
    }

    @Override
    public WriteFeedCallbackResult retrieveFeedResult(WriteFeedCallbackContext context) {
        WriteFeedCallbackResult result = new WriteFeedCallbackResult();
        Map<String, ODataCallback> callbacks = context.getCurrentWriteProperties().getCallbacks();

        EdmNavigationProperty property = context.getNavigationProperty();
        String propertyName = null;

        try {
            propertyName = property.getName();

            List<Map<String, Object>> propertyValues = new ArrayList<>();
            Map<String, Object> contextEntryData = context.getEntryData();
            if (contextEntryData != null
                    && !contextEntryData.isEmpty()
                    && contextEntryData.containsKey(propertyName)) {
                propertyValues.addAll((List<Map<String, Object>>) contextEntryData.get(propertyName));
            }
            result.setFeedData(propertyValues);
        } catch (EdmException e) {
            logger.error("Retrieving feed failed", e);
            throw new POFServiceException(POFServiceException.MESSAGE_CODE_ERROR_FEED_RESULT);
        }

        EntityProviderWriteProperties inlineProperties = EntityProviderWriteProperties.serviceRoot(serviceRoot)
                .expandSelectTree(context.getCurrentExpandSelectTreeNode())
                .selfLink(context.getSelfLink())
                .callbacks(callbacks)
                .build();
        result.setInlineProperties(inlineProperties);

        return result;
    }

}
