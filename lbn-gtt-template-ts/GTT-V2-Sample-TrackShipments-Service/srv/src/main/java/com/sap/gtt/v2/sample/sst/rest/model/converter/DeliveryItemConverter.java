package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.sst.odata.model.DeliveryItem;
import com.sap.gtt.v2.sample.sst.odata.model.FreightUnitItem;
import com.sap.gtt.v2.sample.sst.rest.model.dto.DeliveryItemDto;
import java.util.List;
import java.util.Optional;
import javax.validation.constraints.NotNull;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link DeliveryItemConverter} is a converter which converts {@link DeliveryItem} entities to required type.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class DeliveryItemConverter {

    /**
     * Converts provided {@link DeliveryItem} entity to {@link DeliveryItemDto}.
     *
     * @param deliveryItem - {@link DeliveryItem} entity
     * @return {@link DeliveryItemDto} entity
     */
    public DeliveryItemDto fromDeliveryItem(@NotNull final DeliveryItem deliveryItem) {
        final DeliveryItemDto deliveryItemDto = new DeliveryItemDto();
        deliveryItemDto.setId(deliveryItem.getId().toString());
        deliveryItemDto.setItemNo(deliveryItem.getItemNo());
        deliveryItemDto.setDeliveryNo(deliveryItem.getDeliveryNo());
        deliveryItemDto.setDeliveryItemNo(deliveryItem.getItemNo());
        deliveryItemDto.setOrderQuantity(deliveryItem.getOrderQuantity());
        deliveryItemDto.setQuantityUoM(deliveryItem.getQuantityUoM());
        deliveryItemDto.setMaterialNo(deliveryItem.getMaterialNo());
        deliveryItemDto.setMaterialDescription(deliveryItem.getMaterialDescription());
        deliveryItemDto.setEventStatusCode(deliveryItem.getEventStatusCode());
        deliveryItemDto.setAltKey(deliveryItem.getAltKey());
        deliveryItemDto.setEventMatchKey(deliveryItem.getEventMatchKey());
        deliveryItemDto.setInFreightUnit(FALSE);
        return deliveryItemDto;
    }

    /**
     * Converts provided {@link DeliveryItem} entities to {@link DeliveryItemDto} list.
     *
     * @param deliveryItems - list of {@link DeliveryItem} entities
     * @return {@link DeliveryItemDto} entities
     */
    public List<DeliveryItemDto> fromDeliveryItems(@NotNull final List<DeliveryItem> deliveryItems) {
        return deliveryItems.stream()
                .map(this::fromDeliveryItem)
                .collect(toList());
    }

    /**
     * Converts provided {@link FreightUnitItem} entity to {@link DeliveryItemDto}.
     *
     * @param freightUnitItem - {@link FreightUnitItem} entity
     * @return {@link DeliveryItemDto} entity
     */
    public DeliveryItemDto fromFreightUnitItem(@NotNull final FreightUnitItem freightUnitItem) {
        final DeliveryItemDto deliveryItemDto = new DeliveryItemDto();
        final Optional<DeliveryItem> deliveryItemOpt = Optional.ofNullable(freightUnitItem.getDeliveryItem());
        deliveryItemDto.setId(freightUnitItem.getFreightUnitId().toString());
        deliveryItemDto.setItemNo(freightUnitItem.getItemNo());
        deliveryItemDto.setDeliveryNo(freightUnitItem.getDeliveryNo());
        deliveryItemDto.setDeliveryItemNo(freightUnitItem.getDeliveryItemNo());
        deliveryItemDto.setOrderQuantity(freightUnitItem.getQuantity());
        deliveryItemDto.setQuantityUoM(freightUnitItem.getQuantityUoM());
        deliveryItemDto.setMaterialNo(freightUnitItem.getMaterialNo());
        deliveryItemDto.setMaterialDescription(freightUnitItem.getMaterialDescription());
        deliveryItemDto.setEventStatusCode(freightUnitItem.getEventStatusCode());
        deliveryItemDto.setInFreightUnit(TRUE);
        deliveryItemDto.setEventMatchKey(freightUnitItem.getEventMatchKey());
        deliveryItemOpt.ifPresent(deliveryItem -> deliveryItemDto.setAltKey(deliveryItem.getAltKey()));
        return deliveryItemDto;
    }

    /**
     * Converts provided {@link FreightUnitItem} entities to {@link DeliveryItemDto} list.
     *
     * @param freightUnitItems - list of {@link FreightUnitItem} entities
     * @return {@link DeliveryItemDto} entities
     */
    public List<DeliveryItemDto> fromFreightUnitItems(@NotNull final List<FreightUnitItem> freightUnitItems) {
        return freightUnitItems.stream()
                .map(this::fromFreightUnitItem)
                .collect(toList());
    }
}
