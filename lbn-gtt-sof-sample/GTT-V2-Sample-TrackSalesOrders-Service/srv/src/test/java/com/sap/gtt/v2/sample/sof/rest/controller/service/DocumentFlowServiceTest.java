package com.sap.gtt.v2.sample.sof.rest.controller.service;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import com.sap.gtt.v2.sample.sof.constant.DocumentFlowGeneralStatusEnum;
import com.sap.gtt.v2.sample.sof.constant.DocumentFlowGroupEnum;
import com.sap.gtt.v2.sample.sof.odata.model.SalesOrder;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.DocumentFlow;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.Group;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.Line;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.Node;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.util.List;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;

@RunWith(PowerMockRunner.class)
@PrepareForTest(SOFUtils.class)
public class DocumentFlowServiceTest {

    @Mock
    private GTTCoreServiceClient client;

    @InjectMocks
    private DocumentFlowService documentFlowService;

    @Test
    public void testGenerateDocumentFlow() {
        String json = SOFUtils.getStringFromResource("/odata/document-flow.json");
        Mockito.when(client.readEntity(contains("/SalesOrder(guid'73ab07bb-ab6a-5c7f-9da0-4c975a0656e9')"), eq(SalesOrder.class))).thenReturn(ODataUtils.readEntity(json, SalesOrder.class));
        DocumentFlow flow = documentFlowService.generateDocumentFlow(UUID.fromString("73ab07bb-ab6a-5c7f-9da0-4c975a0656e9"));

        List<Node> nodes = flow.getNodes();
        Node node = nodes.get(0);
        Assert.assertEquals(12, nodes.size());
        Assert.assertEquals(1, node.getKey().intValue());
        Assert.assertEquals(DocumentFlowGeneralStatusEnum.ERROR.getStatus(), node.getStatus());
        Assert.assertEquals(3, node.getAttributes().size());

        List<Line> lines = flow.getLines();
        Assert.assertEquals(12, lines.size());

        List<Group> groups = flow.getGroups();
        Group firstGroup = groups.get(0);
        Group lastGroup = groups.get(groups.size() - 1);
        Assert.assertEquals(6, groups.size());
        Assert.assertEquals(DocumentFlowGroupEnum.SALES_ORDER.getGroupTitle(), firstGroup.getTitle());
        Assert.assertEquals(DocumentFlowGeneralStatusEnum.ERROR.getStatus(), firstGroup.getStatus());
        Assert.assertEquals(DocumentFlowGroupEnum.RESOURCE.getGroupTitle(), lastGroup.getTitle());
        Assert.assertEquals(DocumentFlowGeneralStatusEnum.ERROR.getStatus(), lastGroup.getStatus());
        Assert.assertEquals(Constants.SAP_ICON_LEAD, lastGroup.getIcon());

    }

}
