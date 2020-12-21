package com.sap.gtt.v2.sample.pof.utils;

import com.google.gson.GsonBuilder;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.TrackedProcess;
import org.apache.commons.io.IOUtils;
import org.assertj.core.api.Assertions;
import org.junit.Test;
import org.springframework.core.io.ClassPathResource;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

public class ODataUtilsExTest {

    @Test
    public void testReadEntitySetPojo() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/tracked-processes.json").getInputStream());
        ODataResultList<TrackedProcess> res = ODataUtils.readEntitySet(json, TrackedProcess.class);
        System.out.println(res);
        Assertions.assertThat(res.getCount()).isEqualTo(10);
        List<TrackedProcess> tpList = res.getResults();
        Assertions.assertThat(tpList).hasSize(2);

        Assertions.assertThat(tpList.get(0).getId()).isEqualTo(
                UUID.fromString("996f8dd3-b2e0-582e-ae1b-9d7d851c030d"));
//        Assertions.assertThat(tpList.get(0).getPlannedEvents()).hasSize(4);
//        Assertions.assertThat(tpList.get(0).getProcessStatus()).isNotNull();

        Assertions.assertThat(tpList.get(1).getId()).isEqualTo(
                UUID.fromString("7cb00662-6f1a-52d4-95fd-4ace0678a48f"));
//        Assertions.assertThat(tpList.get(1).getPlannedEvents()).hasSize(2);
//        Assertions.assertThat(tpList.get(1).getProcessStatus()).isNotNull();

        String converted = (new GsonBuilder().setPrettyPrinting()).create().toJson(res.getResults());
        System.out.println(converted);


    }
}