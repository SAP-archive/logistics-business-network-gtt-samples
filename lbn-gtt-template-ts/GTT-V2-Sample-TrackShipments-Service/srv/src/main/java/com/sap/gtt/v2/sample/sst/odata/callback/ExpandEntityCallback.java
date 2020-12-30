package com.sap.gtt.v2.sample.sst.odata.callback;

import com.sap.gtt.v2.sample.sst.common.exception.SSTServiceException;
import java.net.URI;
import java.util.Collections;
import java.util.Map;
import org.apache.olingo.odata2.api.ODataCallback;
import org.apache.olingo.odata2.api.edm.EdmException;
import org.apache.olingo.odata2.api.edm.EdmNavigationProperty;
import org.apache.olingo.odata2.api.ep.EntityProviderWriteProperties;
import org.apache.olingo.odata2.api.ep.callback.OnWriteEntryContent;
import org.apache.olingo.odata2.api.ep.callback.WriteEntryCallbackContext;
import org.apache.olingo.odata2.api.ep.callback.WriteEntryCallbackResult;

/**
 * @author Min Li
 */
public class ExpandEntityCallback implements OnWriteEntryContent {

    private URI serviceRoot;

    public ExpandEntityCallback() {
        super();
    }

    @Override
    public WriteEntryCallbackResult retrieveEntryResult(WriteEntryCallbackContext context) {
        WriteEntryCallbackResult result = new WriteEntryCallbackResult();

        Map<String, ODataCallback> callbacks = context.getCurrentWriteProperties().getCallbacks();
        EdmNavigationProperty property = context.getNavigationProperty();
        String propertyName;
        Map<String, Object> entryData = context.getEntryData();

        try {
            propertyName = property.getName();
            if (entryData.get(propertyName) != null) {
                Map<String, Object> naviData;
                naviData = (Map<String, Object>) entryData.get(propertyName);
                result.setEntryData(naviData);
            } else {
                result.setEntryData(Collections.emptyMap());
            }
        } catch (EdmException e) {
            throw new SSTServiceException(SSTServiceException.MESSAGE_CODE_ERROR_FEED_RESULT);
        }

        EntityProviderWriteProperties inlineProperties = EntityProviderWriteProperties.serviceRoot(this.serviceRoot)
                .expandSelectTree(context.getCurrentExpandSelectTreeNode())
                .callbacks(callbacks)
                .build();

        result.setInlineProperties(inlineProperties);
        return result;
    }

    public void setServiceRoot(URI serviceRoot) {
        this.serviceRoot = serviceRoot;
    }
}
