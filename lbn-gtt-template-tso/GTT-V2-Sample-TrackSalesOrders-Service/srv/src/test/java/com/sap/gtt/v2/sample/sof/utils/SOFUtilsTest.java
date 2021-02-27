package com.sap.gtt.v2.sample.sof.utils;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import com.sap.gtt.v2.sample.sof.domain.OrderBy;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterCondition;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.assertj.core.api.Assertions;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

public class SOFUtilsTest {

    @Test
    public void testGetNormalizedUri() {
        String requestUri = "https://gtt-sample-track-salesorders-service-int.cfapps.sap.hana.ondemand.com/sap/logistics/gtt/sample/sof/odata/v1/SalesOrder(guid%2773ca77e6-85b1-5523-b05c-726ec9ef3027%27)?$format=json";
        String serviceRoot = "https://gtt-sample-track-salesorders-service-int.cfapps.sap.hana.ondemand.com:443/sap/logistics/gtt/sample/sof/odata/v1/";

        String normalizedUri = SOFUtils.getNormalizedUri(requestUri, serviceRoot);
        Assertions.assertThat(normalizedUri).doesNotContain("443");
        Assertions.assertThat(normalizedUri).isNotEqualTo("/SalesOrder(guid'73ca77e6-85b1-5523-b05c-726ec9ef3027')?$format=json");

        requestUri = "https://gtt-sample-track-salesorders-service-int.cfapps.sap.hana.ondemand.com/sap/logistics/gtt/sample/sof/odata/v1/SalesOrder(guid%2773ca77e6-85b1-5523-b05c-726ec9ef3027%27)?$format=json";
        serviceRoot = "https://gtt-sample-track-salesorders-service-int.cfapps.sap.hana.ondemand.com:80/sap/logistics/gtt/sample/sof/odata/v1/";

        normalizedUri = SOFUtils.getNormalizedUri(requestUri, serviceRoot);
        Assertions.assertThat(normalizedUri).doesNotContain("80");
        Assertions.assertThat(normalizedUri).isNotEqualTo("/SalesOrder(guid'73ca77e6-85b1-5523-b05c-726ec9ef3027')?$format=json");

        requestUri = "http://localhost:9098/sap/logistics/gtt/sample/sof/odata/v1/SalesOrder(guid%2773ca77e6-85b1-5523-b05c-726ec9ef3027%27)?$format=json";
        serviceRoot = "http://localhost:9098/sap/logistics/gtt/sample/sof/odata/v1/";
        normalizedUri = SOFUtils.getNormalizedUri(requestUri, serviceRoot);
        Assertions.assertThat(normalizedUri).isNotEqualTo("/SalesOrder(guid'73ca77e6-85b1-5523-b05c-726ec9ef3027')?$format=json");
    }

    @Test
    public void testGenerateLocationAltKey() {
        String locationAltKey = "xri://sap.com/id:LBN#10013165:INT_TEST_CORE_ENGINE:Location:Customer:{{EXTERNALID_COREENGINE}}";
        String generated = SOFUtils.generateLocationAltKey("LBN#10013165", "INT_TEST_CORE_ENGINE",
                "Customer", "{{EXTERNALID_COREENGINE}}");
        Assertions.assertThat(generated).isEqualTo(locationAltKey);
    }
    private String deliveryItemId;
    private List<FilterCondition> filterConditions;
    private List<OrderBy> orderByList;
    private String filter;
    private List<String> expand;
    private String targetEntityName;

    @Before
    public void setUp() {
        deliveryItemId = "41a26e89-cf88-517b-9a67-962d75459369";
        filterConditions = new ArrayList<>();
        FilterCondition propertyCondition = null;
        propertyCondition = new FilterCondition("process_id", FilterCondition.EDM_TYPE_GUID, deliveryItemId, BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        orderByList = new ArrayList<>();
        orderByList.add(new OrderBy("event/altKey",""));
        orderByList.add(new OrderBy("event/actualBusinessTimestamp",""));
        targetEntityName = Constants.PROCESS_EVENT_DIRECTORY_ENTITY_NAME;
        expand = new ArrayList<>();
        expand.add(Constants.EVENT_EXPAND);
        filter = "null";
    }

    @Test
    public void testGenerateUrl() {
        String url = SOFUtils.generateUrl(targetEntityName,filterConditions, BinaryOperator.AND,true,false,expand,orderByList);
        String expected = "/ProcessEventDirectory?$filter=%20(process_id%20eq%20guid'41a26e89-cf88-517b-9a67-962d75459369')%20and%20(%20correlationType_code%20eq%20'EARLY_REPORTED'%20or%20correlationType_code%20eq%20'REPORTED'%20or%20correlationType_code%20eq%20'LATE_REPORTED'%20or%20correlationType_code%20eq%20'UNPLANNED'%20or%20correlationType_code%20eq%20'UNPLANNED_DELAYED'%20or%20correlationType_code%20eq%20'UNPLANNED_ONTIME'%20)%20&$expand=event%20&$orderby=event/altKey%20,event/actualBusinessTimestamp%20";
        Assert.assertEquals(expected,url);
    }
    @Test
    public void testGenerateUrlWithTrue() {
        String url = SOFUtils.generateUrl(targetEntityName,filterConditions, BinaryOperator.AND,true,true,expand,orderByList);
        String expected = "/ProcessEventDirectory?$filter=%20(process_id%20eq%20guid'41a26e89-cf88-517b-9a67-962d75459369')%20and%20(%20correlationType_code%20eq%20'EARLY_REPORTED'%20or%20correlationType_code%20eq%20'REPORTED'%20or%20correlationType_code%20eq%20'LATE_REPORTED'%20or%20correlationType_code%20eq%20'UNPLANNED'%20or%20correlationType_code%20eq%20'UNPLANNED_DELAYED'%20or%20correlationType_code%20eq%20'UNPLANNED_ONTIME'%20or%20correlationType_code%20eq%20'EARLY_REPORTED'%20or%20correlationType_code%20eq%20'REPORTED'%20or%20correlationType_code%20eq%20'LATE_REPORTED'%20or%20correlationType_code%20eq%20'UNPLANNED'%20)%20&$expand=event%20&$orderby=event/altKey%20,event/actualBusinessTimestamp%20";
        Assert.assertEquals(expected,url);
    }
    @Test
    public void testGenerateUrl1() {
        String url = SOFUtils.generateUrl(targetEntityName,filter,true,true,expand,orderByList);
        String expected = "/ProcessEventDirectory?$filter=null%20(%20correlationType_code%20eq%20'EARLY_REPORTED'%20or%20correlationType_code%20eq%20'REPORTED'%20or%20correlationType_code%20eq%20'LATE_REPORTED'%20or%20correlationType_code%20eq%20'UNPLANNED'%20or%20correlationType_code%20eq%20'UNPLANNED_DELAYED'%20or%20correlationType_code%20eq%20'UNPLANNED_ONTIME'%20or%20correlationType_code%20eq%20'EARLY_REPORTED'%20or%20correlationType_code%20eq%20'REPORTED'%20or%20correlationType_code%20eq%20'LATE_REPORTED'%20or%20correlationType_code%20eq%20'UNPLANNED'%20)%20&$expand=event%20&$orderby=event/altKey%20,event/actualBusinessTimestamp%20";
        Assert.assertEquals(expected,url);
    }

    @Test
    public void testGenerateUrl2() {
        String url = SOFUtils.generateUrl(targetEntityName,filter,expand,orderByList);
        String expected = "/ProcessEventDirectory?$filter=null&$expand=event%20&$orderby=event/altKey%20,event/actualBusinessTimestamp%20";
        Assert.assertEquals(expected,url);
    }

    @Test
    public void testGenerateUrl3() {
        String url = SOFUtils.generateUrl(targetEntityName,filterConditions, BinaryOperator.AND,expand,orderByList);
        String expected = "/ProcessEventDirectory?$filter=%20(process_id%20eq%20guid'41a26e89-cf88-517b-9a67-962d75459369')%20&$expand=event%20&$orderby=event/altKey%20,event/actualBusinessTimestamp%20";
        Assert.assertEquals(expected,url);
    }

    @Test
    public void testGenerateUrl4() {
        String url = SOFUtils.generateUrl(targetEntityName,"",filterConditions, BinaryOperator.AND,false,false,expand,orderByList);
        String expected = "/ProcessEventDirectory?$filter=%20(process_id%20eq%20guid'41a26e89-cf88-517b-9a67-962d75459369')%20&$expand=event%20&$orderby=event/altKey%20,event/actualBusinessTimestamp%20";
        Assert.assertEquals(expected,url);
    }

    @Test
    public void testGenerateUrlWithCorrelationType() {
        SOFUtils.generateUrlWithCorrelationType(targetEntityName,filterConditions,BinaryOperator.AND,expand,orderByList);
    }

    @Test
    public void testGenerateUrlWithCorrelationTypeAndNoGeo() {
        SOFUtils.generateUrlWithCorrelationTypeAndNoGeo(targetEntityName,filterConditions,BinaryOperator.AND,expand,orderByList);
    }

    @Test
    public void getTimeStr() {
        SOFUtils.getTimeStr();
    }

    @Test
    public void removeArrivalTimesInUrl() {
        String url = "?$expand=arrivalTimes";
        String actual = SOFUtils.removeFieldInUrl(url,"arrivalTimes");
        Assert.assertEquals("?",actual);
    }

    @Test
    public void removeDestinationLocationInUrl() {
        String url = "?$expand=destinationLocation";
        String actual = SOFUtils.removeFieldInUrl(url,"destinationLocation");
        Assert.assertEquals("?",actual);

        url = "?$expand=incoterms%2cshipToPartyLocation";
        actual = SOFUtils.removeFieldInUrl(url,"shipToPartyLocation");
        Assert.assertEquals("?$expand=incoterms",actual);

        url = "?$expand=incoterms%2cshipToPartyLocation%2clocationXXX";
        actual = SOFUtils.removeFieldInUrl(url,"shipToPartyLocation");
        Assert.assertEquals("?$expand=incoterms%2clocationXXX",actual);

        url = "?$expand=shipToPartyLocation%2clocationXXX";
        actual = SOFUtils.removeFieldInUrl(url,"shipToPartyLocation");
        Assert.assertEquals("?$expand=locationXXX",actual);
    }
}