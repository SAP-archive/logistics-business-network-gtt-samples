package com.sap.gtt.v2.sample.pof.service;

import com.sap.gtt.v2.sample.pof.odata.handler.POFLocationODataHandler;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.LocationDTO;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

@Service
public class LocationService {

    @Autowired
    private POFLocationODataHandler pofLocationODataHandler;

    @Autowired
    private MapService mapService;

    public Map<String, LocationDTO> getLocationsForPurchaseOrderItems(List<PurchaseOrderItem> purchaseOrderItems) {
        Set<String> locationAltKeys = new HashSet<>();
        purchaseOrderItems.forEach(purchaseOrderItem -> {
            if (purchaseOrderItem.getReceivingLocationType() != null && purchaseOrderItem.getReceivingLocationId() != null) {
                locationAltKeys.add(POFUtils.generateLocationAltKey(purchaseOrderItem.getPartyId(), purchaseOrderItem.getLogicalSystem(),
                        purchaseOrderItem.getReceivingLocationType().getCode(), purchaseOrderItem.getReceivingLocationId()));
            }

            if (purchaseOrderItem.getSupplierLocationType() != null && purchaseOrderItem.getSupplierId() != null) {
                locationAltKeys.add(POFUtils.generateLocationAltKey(purchaseOrderItem.getPartyId(), purchaseOrderItem.getLogicalSystem(),
                        purchaseOrderItem.getSupplierLocationType().getCode(), purchaseOrderItem.getSupplierId()));
            }
        });

        return pofLocationODataHandler.getLocations(locationAltKeys);
    }

    public Map<String, LocationDTO> getLocationsForPurchaseOrderTP(List<PurchaseOrderItem> purchaseOrders) {
        Set<String> locationAltKeys = new HashSet<>();
        purchaseOrders.stream()
                .filter(Objects::nonNull)
                .forEach(purchaseOrderItem -> {
                    if (purchaseOrderItem.getReceivingLocationType() != null && purchaseOrderItem.getReceivingLocationId() != null) {
                        locationAltKeys.add(POFUtils.generateLocationAltKey(purchaseOrderItem.getPartyId(), purchaseOrderItem.getLogicalSystem(),
                                purchaseOrderItem.getReceivingLocationType().getCode(), purchaseOrderItem.getReceivingLocationId()));
                    }

                    if (purchaseOrderItem.getSupplierLocationType() != null && purchaseOrderItem.getSupplierId() != null) {
                        locationAltKeys.add(POFUtils.generateLocationAltKey(purchaseOrderItem.getPartyId(), purchaseOrderItem.getLogicalSystem(),
                                purchaseOrderItem.getSupplierLocationType().getCode(), purchaseOrderItem.getSupplierId()));
                    }
                });

        return pofLocationODataHandler.getLocations(locationAltKeys);
    }

    public Map<String, LocationDTO> getLocationsForPurchaseOrders(List<PurchaseOrder> purchaseOrders) {
        Set<String> locationAltKeys = new HashSet<>();
        purchaseOrders.forEach(purchaseOrder -> {
            if (purchaseOrder.getReceivingLocationType() != null && purchaseOrder.getReceivingLocationId() != null) {
                locationAltKeys.add(POFUtils.generateLocationAltKey(purchaseOrder.getPartyId(), purchaseOrder.getLogicalSystem(),
                        purchaseOrder.getReceivingLocationType().getCode(), purchaseOrder.getReceivingLocationId()));
            }

            if (purchaseOrder.getSupplierLocationType() != null && purchaseOrder.getSupplierId() != null) {
                locationAltKeys.add(POFUtils.generateLocationAltKey(purchaseOrder.getPartyId(), purchaseOrder.getLogicalSystem(),
                        purchaseOrder.getSupplierLocationType().getCode(), purchaseOrder.getSupplierId()));
            }
        });

        return pofLocationODataHandler.getLocations(locationAltKeys);
    }

    public Map<String, LocationDTO> getLocationsForPurchaseOrderItemInboundDeliveryItem(List<InboundDeliveryItem> inboundDeliveryItemsTP) {
        Set<String> locationAltKeys = new HashSet<>();
        inboundDeliveryItemsTP.stream()
                .filter(Objects::nonNull)
                .forEach(inboundDeliveryItem -> {
                    if (inboundDeliveryItem.getPlantLocationType() != null && inboundDeliveryItem.getPlant() != null) {
                        locationAltKeys.add(POFUtils.generateLocationAltKey(inboundDeliveryItem.getPartyId(), inboundDeliveryItem.getLogicalSystem(),
                                inboundDeliveryItem.getPlantLocationType().getCode(), inboundDeliveryItem.getPlant()));
                    }

                    if (inboundDeliveryItem.getSupplierLocationType() != null && inboundDeliveryItem.getSupplier() != null) {
                        locationAltKeys.add(POFUtils.generateLocationAltKey(inboundDeliveryItem.getPartyId(), inboundDeliveryItem.getLogicalSystem(),
                                inboundDeliveryItem.getSupplierLocationType().getCode(), inboundDeliveryItem.getSupplier()));
                    }
                });

        return pofLocationODataHandler.getLocations(locationAltKeys);
    }

    public Map<String, LocationDTO> getLocationsForInboundDeliveryItem(List<InboundDeliveryItem> inboundDeliveryItems) {
        Set<String> locationAltKeys = new HashSet<>();
        inboundDeliveryItems.forEach(inboundDeliveryItem -> {
            if (inboundDeliveryItem.getPlantLocationType() != null && inboundDeliveryItem.getPlant() != null) {
                locationAltKeys.add(POFUtils.generateLocationAltKey(inboundDeliveryItem.getPartyId(), inboundDeliveryItem.getLogicalSystem(),
                        inboundDeliveryItem.getPlantLocationType().getCode(), inboundDeliveryItem.getPlant()));
            }

            if (inboundDeliveryItem.getSupplierLocationType() != null && inboundDeliveryItem.getSupplier() != null) {
                locationAltKeys.add(POFUtils.generateLocationAltKey(inboundDeliveryItem.getPartyId(), inboundDeliveryItem.getLogicalSystem(),
                        inboundDeliveryItem.getSupplierLocationType().getCode(), inboundDeliveryItem.getSupplier()));
            }
        });

        return pofLocationODataHandler.getLocations(locationAltKeys);
    }


    public void setLocationsForPurchaseOrder(PurchaseOrder purchaseOrder, Map<String, LocationDTO> map) {
        if (purchaseOrder.getReceivingLocationType() != null && purchaseOrder.getReceivingLocationId() != null) {
            String locationAltKey = POFUtils.generateLocationAltKey(purchaseOrder.getPartyId(),purchaseOrder.getLogicalSystem(),
                    purchaseOrder.getReceivingLocationType().getCode(),purchaseOrder.getReceivingLocationId());
            purchaseOrder.setReceivingLocation(map.get(locationAltKey));
        }

        if (purchaseOrder.getSupplierLocationType() != null && purchaseOrder.getSupplierId() != null) {
            String locationAltKey = POFUtils.generateLocationAltKey(purchaseOrder.getPartyId(),purchaseOrder.getLogicalSystem(),
                    purchaseOrder.getSupplierLocationType().getCode(),purchaseOrder.getSupplierId());
            purchaseOrder.setSupplierLocation(map.get(locationAltKey));
        }
    }

    public void setLocationsForPurchaseOrderItem(PurchaseOrderItem purchaseOrderItem, Map<String, LocationDTO> map) {
        if (purchaseOrderItem.getReceivingLocationType() != null && purchaseOrderItem.getReceivingLocationId() != null) {
            String locationAltKey = POFUtils.generateLocationAltKey(purchaseOrderItem.getPartyId(),purchaseOrderItem.getLogicalSystem(),
                    purchaseOrderItem.getReceivingLocationType().getCode(),purchaseOrderItem.getReceivingLocationId());
            purchaseOrderItem.setReceivingLocation(map.get(locationAltKey));
        }

        if (purchaseOrderItem.getSupplierLocationType() != null && purchaseOrderItem.getSupplierId() != null) {
            String locationAltKey = POFUtils.generateLocationAltKey(purchaseOrderItem.getPartyId(),purchaseOrderItem.getLogicalSystem(),
                    purchaseOrderItem.getSupplierLocationType().getCode(),purchaseOrderItem.getSupplierId());
            purchaseOrderItem.setSupplierLocation(map.get(locationAltKey));
        }
    }

    public void setLocationsForInboundDelivery(InboundDeliveryItem inboundDeliveryItem, Map<String, LocationDTO> map) {
        if (inboundDeliveryItem.getPlant() != null && inboundDeliveryItem.getPlantLocationType() != null) {
            String locationAltKey = POFUtils.generateLocationAltKey(inboundDeliveryItem.getPartyId(), inboundDeliveryItem.getLogicalSystem(),
                    inboundDeliveryItem.getPlantLocationType().getCode(), inboundDeliveryItem.getPlant());
            inboundDeliveryItem.setPlantLocation(map.get(locationAltKey));
        }
        if (inboundDeliveryItem.getSupplier() != null && inboundDeliveryItem.getSupplierLocationType() != null) {
            String locationAltKey = POFUtils.generateLocationAltKey(inboundDeliveryItem.getPartyId(), inboundDeliveryItem.getLogicalSystem(),
                    inboundDeliveryItem.getSupplierLocationType().getCode(), inboundDeliveryItem.getSupplier());
            inboundDeliveryItem.setSupplierLocation(map.get(locationAltKey));
        }
    }

    public void setPlantLocation(InboundDeliveryItem inboundDeliveryItem) {
        if (inboundDeliveryItem.getPlantLocationType() != null && inboundDeliveryItem.getPlant() != null) {
            String locationAltKey = POFUtils.generateLocationAltKey(inboundDeliveryItem.getPartyId(),inboundDeliveryItem.getLogicalSystem(),
                    inboundDeliveryItem.getPlantLocationType().getCode(), inboundDeliveryItem.getPlant());
            inboundDeliveryItem.setPlantLocation(mapService.getLocationDetail(pofLocationODataHandler.getLocation(locationAltKey)));
        }
    }

    public void setSupplierLocation(InboundDeliveryItem inboundDeliveryItem) {
        if (inboundDeliveryItem.getSupplierLocationType() != null && inboundDeliveryItem.getSupplier() != null) {
            String locationAltKey = POFUtils.generateLocationAltKey(inboundDeliveryItem.getPartyId(),inboundDeliveryItem.getLogicalSystem(),
                    inboundDeliveryItem.getSupplierLocationType().getCode(), inboundDeliveryItem.getSupplier());
            inboundDeliveryItem.setSupplierLocation(mapService.getLocationDetail(pofLocationODataHandler.getLocation(locationAltKey)));
        }
    }

    public void setReceivingLocation(PurchaseOrderItem purchaseOrderItem) {
        if (purchaseOrderItem.getReceivingLocationType() != null && purchaseOrderItem.getReceivingLocationId() != null) {
            String locationAltKey = POFUtils.generateLocationAltKey(purchaseOrderItem.getPartyId(), purchaseOrderItem.getLogicalSystem(),
                    purchaseOrderItem.getReceivingLocationType().getCode(), purchaseOrderItem.getReceivingLocationId());
            purchaseOrderItem.setReceivingLocation(mapService.getLocationDetail(pofLocationODataHandler.getLocation(locationAltKey)));
        }
    }

    public void setSupplierLocation(PurchaseOrderItem purchaseOrderItem) {
        if (purchaseOrderItem.getSupplierLocationType() != null && purchaseOrderItem.getSupplierId() != null) {
            String locationAltKey = POFUtils.generateLocationAltKey(purchaseOrderItem.getPartyId(),purchaseOrderItem.getLogicalSystem(),
                    purchaseOrderItem.getSupplierLocationType().getCode(), purchaseOrderItem.getSupplierId());
            purchaseOrderItem.setSupplierLocation(mapService.getLocationDetail(pofLocationODataHandler.getLocation(locationAltKey)));
        }
    }

    public void setReceivingLocation(PurchaseOrder purchaseOrder) {
        if (purchaseOrder.getReceivingLocationType() != null && purchaseOrder.getReceivingLocationId() != null) {
            String locationAltKey = POFUtils.generateLocationAltKey(purchaseOrder.getPartyId(),purchaseOrder.getLogicalSystem(),
                    purchaseOrder.getReceivingLocationType().getCode(), purchaseOrder.getReceivingLocationId());
            purchaseOrder.setReceivingLocation(mapService.getLocationDetail(pofLocationODataHandler.getLocation(locationAltKey)));
        }
    }

    public void setSupplierLocation(PurchaseOrder purchaseOrder) {
        if (purchaseOrder.getSupplierLocationType() != null && purchaseOrder.getSupplierId() != null) {
            String locationAltKey = POFUtils.generateLocationAltKey(purchaseOrder.getPartyId(),purchaseOrder.getLogicalSystem(),
                    purchaseOrder.getSupplierLocationType().getCode(), purchaseOrder.getSupplierId());
            purchaseOrder.setSupplierLocation(mapService.getLocationDetail(pofLocationODataHandler.getLocation(locationAltKey)));
        }
    }
}
