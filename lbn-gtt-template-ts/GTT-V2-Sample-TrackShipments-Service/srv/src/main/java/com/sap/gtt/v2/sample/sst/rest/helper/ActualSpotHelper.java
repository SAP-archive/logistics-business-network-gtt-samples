package com.sap.gtt.v2.sample.sst.rest.helper;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.utils.ProcessEventDirectoryUtils;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.converter.ActualSpotConverter;
import java.util.List;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link ActualSpotHelper} is a helper class which operates on {@link ActualSpot} entities.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class ActualSpotHelper {

    @Autowired
    private ActualSpotConverter actualSpotConverter;

    /**
     * Retrieves list of {@link ActualSpot} from provided {@link ProcessEventDirectory} entities.
     *
     * @param processEventDirectories - provided list of {@link ProcessEventDirectory} entities
     * @return list of {@link ActualSpot} entities
     */
    public List<ActualSpot> getAllAscending(@NotNull final List<ProcessEventDirectory> processEventDirectories) {
        final List<ProcessEventDirectory> sortedActualEvents =
                ProcessEventDirectoryUtils.sortByActualBusinessDateTimeAscending(processEventDirectories);
        return actualSpotConverter.fromProcessEventDirectories(sortedActualEvents);
    }
}
