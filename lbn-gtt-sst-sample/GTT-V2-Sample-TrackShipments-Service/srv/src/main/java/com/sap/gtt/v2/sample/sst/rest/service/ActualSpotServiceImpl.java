package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.utils.ProcessEventDirectoryUtils;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.converter.ActualSpotConverter;
import java.util.List;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * @author Aliaksandr Miron
 */
@Validated
@Service
public class ActualSpotServiceImpl implements ActualSpotService {

    @Autowired
    private ActualSpotConverter actualSpotConverter;

    @Override
    public List<ActualSpot> getAllAscending(@NotNull final List<ProcessEventDirectory> processEventDirectories) {
        final List<ProcessEventDirectory> sortedActualEvents =
                ProcessEventDirectoryUtils.sortByActualBusinessDateTimeAscending(processEventDirectories);
        return actualSpotConverter.fromProcessEventDirectories(sortedActualEvents);
    }
}
