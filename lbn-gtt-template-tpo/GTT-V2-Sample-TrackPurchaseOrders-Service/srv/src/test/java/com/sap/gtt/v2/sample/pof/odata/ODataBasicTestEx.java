package com.sap.gtt.v2.sample.pof.odata;

import com.sap.gtt.v2.sample.pof.App;
import org.assertj.core.api.Assertions;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = App.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@ActiveProfiles("test")
public class ODataBasicTestEx {
    @Autowired
    private TestRestTemplate restTemplate;
    
    @Test
    public void testDeliveryItemQuery() {
        String query = "/sap/logistics/gtt/sample/pof/odata/v1/InboundDeliveryItem";

        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);

        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
    }

    @Test
    public void testPurchaseOrderQuery() {
        String query = "/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrder";

        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);

        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
    }

    @Test
    public void testPurchaseOrderItemQuery() {
        String query = "/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrderItem";

        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);

        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
    }
}
