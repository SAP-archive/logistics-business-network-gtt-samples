package com.sap.gtt.v2.sample.sst.odata.callback;

import com.sap.gtt.v2.sample.sst.common.exception.SSTServiceException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.apache.olingo.odata2.api.ODataCallback;
import org.apache.olingo.odata2.api.edm.EdmException;
import org.apache.olingo.odata2.api.edm.EdmNavigationProperty;
import org.apache.olingo.odata2.api.ep.EntityProviderWriteProperties;
import org.apache.olingo.odata2.api.ep.callback.OnWriteFeedContent;
import org.apache.olingo.odata2.api.ep.callback.WriteFeedCallbackContext;
import org.apache.olingo.odata2.api.ep.callback.WriteFeedCallbackResult;

/**
 * @author Min Li
 */
public class ExpandEntitySetCallback implements OnWriteFeedContent {

    private URI serviceRoot;

    public ExpandEntitySetCallback() {
        super();
    }

    @Override
    public WriteFeedCallbackResult retrieveFeedResult(WriteFeedCallbackContext context) {
        WriteFeedCallbackResult result = new WriteFeedCallbackResult();
        Map<String, ODataCallback> callbacks = context.getCurrentWriteProperties().getCallbacks();

        EdmNavigationProperty property = context.getNavigationProperty();
        String propertyName;

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
            throw new SSTServiceException(SSTServiceException.MESSAGE_CODE_ERROR_FEED_RESULT);
        }

        EntityProviderWriteProperties inlineProperties = EntityProviderWriteProperties.serviceRoot(serviceRoot)
                .expandSelectTree(context.getCurrentExpandSelectTreeNode())
                .selfLink(context.getSelfLink())
                .callbacks(callbacks)
                .build();
        result.setInlineProperties(inlineProperties);

        return result;
    }

    public void setServiceRoot(URI serviceRoot) {
        this.serviceRoot = serviceRoot;
    }
}
