package com.sap.gtt.v2.sample.sof.odata.handler;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.LocationDTO;
import com.sap.gtt.v2.sample.sof.odata.model.SalesOrder;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

@Component
public class SalesOrderODataHandler extends SOFDefaultODataHandler {

    public static final String SOF_DEFAULT_O_DATA_HANDLER = "SOFDefaultODataHandler";
    @Autowired
    private LocationDTOOdataHandler locationDTOOdataHandler;
    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        String uri = SOFUtils.getNormalizedUri(oDataContext);
        boolean containsLocation = uri.contains(Constants.SHIP_TO_PARTY_LOCATION);
        if(containsLocation) {
            uri = SOFUtils.removeFieldInUrl(uri, Constants.SHIP_TO_PARTY_LOCATION);
        }
        ODataResultList<SalesOrder> entityList = gttCoreServiceClient.readEntitySet(uri, SalesOrder.class);
        if(containsLocation) {
            setLocations(entityList);
        }
        return convertResults(entityList);
    }

    private void setLocations(ODataResultList<SalesOrder> entityList) {
        Set<String> locationAltKeys = new HashSet<>();
        entityList.getResults().forEach(salesOrder -> locationAltKeys.add(SOFUtils.generateLocationAltKey(salesOrder.getPartyId(),salesOrder.getLogicalSystem(),"Customer",salesOrder.getShipToPartyId())));
        Map<String, LocationDTO> map = locationDTOOdataHandler.getLocationDTOs(locationAltKeys);
        entityList.getResults().forEach(salesOrder -> {
            String locationAltKey = SOFUtils.generateLocationAltKey(salesOrder.getPartyId(),salesOrder.getLogicalSystem(),"Customer",salesOrder.getShipToPartyId());
            salesOrder.setLocationDTO(map.get(locationAltKey));
        });
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        String uri = SOFUtils.getNormalizedUri(oDataContext);
        boolean containsLocation = uri.contains(Constants.SHIP_TO_PARTY_LOCATION);
        if(containsLocation) {
            uri = SOFUtils.removeFieldInUrl(uri, Constants.SHIP_TO_PARTY_LOCATION);
        }
        SalesOrder salesOrder = gttCoreServiceClient.readEntity(uri, SalesOrder.class);
        if(containsLocation) {
            setLocation(salesOrder);
        }
        return ODataUtils.toMap(salesOrder);
    }

    private void setLocation(SalesOrder salesOrder) {
        String locationAltKey = SOFUtils.generateLocationAltKey(salesOrder.getPartyId(),salesOrder.getLogicalSystem(),"Customer",salesOrder.getShipToPartyId());
        salesOrder.setLocationDTO(locationDTOOdataHandler.getLocationDTO(locationAltKey));
    }


}
