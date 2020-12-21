package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static org.assertj.core.api.Assertions.assertThat;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.rest.model.EventHistory;
import org.junit.jupiter.api.Test;

class EventHistoryConverterTest {

    private final EventHistoryConverter eventHistoryConverter = new EventHistoryConverter();

    @Test
    void fromProcessEventDirectory_givenProcessEventDirectory_shouldConvertToEventHistory() {
        // given
        final String processEventDirectoriesJson = SSTUtils.getStringFromResource("/odata/process-event-directories.json");
        final ProcessEventDirectory processEventDirectory =
                ODataUtils.readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults().get(0);

        // when
        final EventHistory eventHistory = eventHistoryConverter.fromProcessEventDirectory(processEventDirectory);

        // then
        assertThat(eventHistory).isNotNull();
    }
}
