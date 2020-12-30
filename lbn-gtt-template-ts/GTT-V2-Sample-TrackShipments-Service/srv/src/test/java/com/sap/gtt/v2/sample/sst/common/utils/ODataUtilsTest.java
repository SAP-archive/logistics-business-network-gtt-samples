package com.sap.gtt.v2.sample.sst.common.utils;

import com.google.gson.GsonBuilder;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.TrackedProcess;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

class ODataUtilsTest {

    @Test
    void testReadEntitySet() {
        String json = SSTUtils.getStringFromResource("/odata/tracked-processes.json");
        ODataResultList<TrackedProcess> res = ODataUtils.readEntitySet(json, TrackedProcess.class);
        System.out.println(res);
        Assertions.assertThat(res.getCount()).isEqualTo(10);
        List<TrackedProcess> tpList = res.getResults();
        Assertions.assertThat(tpList).hasSize(2);

        Assertions.assertThat(tpList.get(0).getId()).isEqualTo(
                UUID.fromString("996f8dd3-b2e0-582e-ae1b-9d7d851c030d"));
        Assertions.assertThat(tpList.get(0).getPlannedEvents()).hasSize(4);
        Assertions.assertThat(tpList.get(0).getProcessStatus()).isNotNull();

        Assertions.assertThat(tpList.get(1).getId()).isEqualTo(
                UUID.fromString("7cb00662-6f1a-52d4-95fd-4ace0678a48f"));
        Assertions.assertThat(tpList.get(1).getPlannedEvents()).hasSize(2);
        Assertions.assertThat(tpList.get(1).getProcessStatus()).isNotNull();

        String converted = (new GsonBuilder().setPrettyPrinting()).create().toJson(res.getResults());
        System.out.println(converted);

        List<Map<String, Object>> mapList = new ArrayList<>();
        for (TrackedProcess tp : tpList) {
            Map<String, Object> map = ODataUtils.toMap(tp);
            System.out.println(map);
            mapList.add(map);
        }

        Assertions.assertThat(mapList).hasSize(2);
        Assertions.assertThat((List<Map<String, Object>>) mapList.get(0).get("plannedEvents")).hasSize(4);
        Assertions.assertThat(mapList.get(0).get("processStatus")).isInstanceOf(Map.class);
    }

    @Test
    void testEmptyResult() {
        String json = "{\n" +
                "    \"d\": {\n" +
                "        \"__count\": \"0\",\n" +
                "        \"results\": []\n" +
                "    }\n" +
                "}";

        ODataResultList<TrackedProcess> res = ODataUtils.readEntitySet(json, TrackedProcess.class);
        Assertions.assertThat(res).isNotNull();
        Assertions.assertThat(res.getCount()).isEqualTo(0);
        Assertions.assertThat(res.getResults()).isNotNull();
        Assertions.assertThat(res.getResults()).isEmpty();

        Map<String, Object> map = ODataUtils.toMap(res.getResults());
        Assertions.assertThat(map).isNotNull();
        Assertions.assertThat(map).isEmpty();
    }

    @Test
    void testErrorResult() {
        String json = "{\n" +
                "    \"error\": {\n" +
                "        \"code\": \"com.sap.gtt.v2.exception.MetadataException.NotFound\",\n" +
                "        \"message\": \"com.lbngttsamples.gtt.app.sof is not found\",\n" +
                "        \"details\": []\n" +
                "    }\n" +
                "}";

        ODataResultList<TrackedProcess> res = ODataUtils.readEntitySet(json, TrackedProcess.class);
        Assertions.assertThat(res).isNotNull();
        Assertions.assertThat(res.getCount()).isNull();
        Assertions.assertThat(res.getResults()).isNotNull();
        Assertions.assertThat(res.getResults()).isEmpty();

        Map<String, Object> map = ODataUtils.toMap(res.getResults());
        Assertions.assertThat(map).isNotNull();
        Assertions.assertThat(map).isEmpty();
    }
}
