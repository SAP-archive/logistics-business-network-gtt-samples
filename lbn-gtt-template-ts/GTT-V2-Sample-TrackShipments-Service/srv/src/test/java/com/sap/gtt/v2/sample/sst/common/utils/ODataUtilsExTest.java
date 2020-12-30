package com.sap.gtt.v2.sample.sst.common.utils;

import com.google.gson.GsonBuilder;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.TrackedProcess;
import java.util.List;
import java.util.UUID;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

class ODataUtilsExTest {

    @Test
    void testReadEntitySetPojo() {
        String json = SSTUtils.getStringFromResource("/odata/tracked-processes.json");
        ODataResultList<TrackedProcess> res = ODataUtils.readEntitySet(json, TrackedProcess.class);
        System.out.println(res);
        Assertions.assertThat(res.getCount()).isEqualTo(10);
        List<TrackedProcess> tpList = res.getResults();
        Assertions.assertThat(tpList).hasSize(2);

        Assertions.assertThat(tpList.get(0).getId()).isEqualTo(
                UUID.fromString("996f8dd3-b2e0-582e-ae1b-9d7d851c030d"));

        Assertions.assertThat(tpList.get(1).getId()).isEqualTo(
                UUID.fromString("7cb00662-6f1a-52d4-95fd-4ace0678a48f"));

        String converted = (new GsonBuilder().setPrettyPrinting()).create().toJson(res.getResults());
        System.out.println(converted);
    }
}
