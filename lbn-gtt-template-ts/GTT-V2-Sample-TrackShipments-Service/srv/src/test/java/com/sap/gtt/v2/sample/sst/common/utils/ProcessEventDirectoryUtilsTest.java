package com.sap.gtt.v2.sample.sst.common.utils;

import static com.sap.gtt.v2.sample.sst.common.utils.ODataUtils.readEntitySet;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getStringFromResource;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;

class ProcessEventDirectoryUtilsTest {

    @Test
    void filterByWhitelistForTimelineEvents_givenProcessEventDirectories_shouldReturnFiltered() {
        // given
        final String processEventDirectoriesJson = getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();

        // when
        final List<ProcessEventDirectory> filteredProcessEventDirectories =
                ProcessEventDirectoryUtils.filterByWhitelistForTimelineEvents(processEventDirectories);

        // then
        assertThat(filteredProcessEventDirectories).isNotEmpty();
    }

    @Test
    void filterByWhitelistForRoutes_givenProcessEventDirectories_shouldReturnFiltered() {
        // given
        final String processEventDirectoriesJson = getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();

        // when
        final List<ProcessEventDirectory> filteredProcessEventDirectories =
                ProcessEventDirectoryUtils.filterByWhitelistForRoutes(processEventDirectories);

        // then
        assertThat(filteredProcessEventDirectories).isNotEmpty();
    }

    @Test
    void getMaxByActualBusinessDateTime_givenProcessEventDirectories_shouldReturnMax() {
        // given
        final String processEventDirectoriesJson = getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();

        // when
        final Optional<ProcessEventDirectory> processEventDirectoryOpt =
                ProcessEventDirectoryUtils.getMaxByActualBusinessDateTime(processEventDirectories);

        // then
        assertThat(processEventDirectoryOpt).isPresent();
    }

    @Test
    void sortByActualBusinessDateTimeAscending_givenProcessEventDirectories_shouldReturnSorted() {
        // given
        final String processEventDirectoriesJson = getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();

        // when
        final List<ProcessEventDirectory> sortedProcessEventDirectories =
                ProcessEventDirectoryUtils.sortByActualBusinessDateTimeAscending(processEventDirectories);

        // then
        assertThat(sortedProcessEventDirectories).isNotEmpty();

        final ProcessEventDirectory firstElement = sortedProcessEventDirectories.get(0);
        final ProcessEventDirectory lastElement = sortedProcessEventDirectories.get(sortedProcessEventDirectories.size() - 1);

        assertThat(firstElement.getEvent().getActualBusinessTimestamp())
                .isLessThanOrEqualTo(lastElement.getEvent().getActualBusinessTimestamp());
    }

    @Test
    void sortByActualBusinessDateTimeDescending_givenProcessEventDirectories_shouldReturnSorted() {
        // given
        final String processEventDirectoriesJson = getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();

        // when
        final List<ProcessEventDirectory> sortedProcessEventDirectories =
                ProcessEventDirectoryUtils.sortByActualBusinessDateTimeDescending(processEventDirectories);

        // then
        assertThat(sortedProcessEventDirectories).isNotEmpty();

        final ProcessEventDirectory firstElement = sortedProcessEventDirectories.get(0);
        final ProcessEventDirectory lastElement = sortedProcessEventDirectories.get(sortedProcessEventDirectories.size() - 1);

        assertThat(firstElement.getEvent().getActualBusinessTimestamp())
                .isGreaterThanOrEqualTo(lastElement.getEvent().getActualBusinessTimestamp());
    }

    @Test
    void getMaxComparator_shouldReturnComparator() {
        // when-then
        assertDoesNotThrow(ProcessEventDirectoryUtils::getMaxComparator);
    }

    @Test
    void getDescendingComparator_shouldReturnComparator() {
        // when-then
        assertDoesNotThrow(ProcessEventDirectoryUtils::getDescendingComparator);
    }

    @Test
    void getAscendingComparator_shouldReturnComparator() {
        // when-then
        assertDoesNotThrow(ProcessEventDirectoryUtils::getAscendingComparator);
    }
}
