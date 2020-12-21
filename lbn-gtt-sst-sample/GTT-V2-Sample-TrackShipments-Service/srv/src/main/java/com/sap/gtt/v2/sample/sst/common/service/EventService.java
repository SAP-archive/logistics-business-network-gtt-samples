package com.sap.gtt.v2.sample.sst.common.service;

import com.sap.gtt.v2.sample.sst.common.model.CodeListValue;
import com.sap.gtt.v2.sample.sst.common.model.Event;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.UUID;
import javax.validation.constraints.NotNull;

/**
 * {@link EventService} is a service which operates on {@link Event} entities.
 *
 * @author Aliaksandr Miron
 */
public interface EventService {

    /**
     * Retrieves {@link Event} entity.
     *
     * @param id        - UUID of {@link Event} entity
     * @param eventType - type name of {@link Event} entity
     * @return {@link Event} entity wrapped in {@link Optional}
     */
    Optional<Event> getById(@NotNull final String id, @NotNull final String eventType);


    /**
     * Retrieves {@link Event} entities.
     *
     * @param eventType - type name of {@link Event} entity
     * @param ids       - UUIDs of {@link Event} entities
     * @return list of {@link Event} entities
     */
    List<Event> getByEventType(@NotNull final String eventType, @NotNull final List<UUID> ids);

    /**
     * Creates {@link Event} entity.
     *
     * @param eventJson - JSON body of {@link Event} entity
     * @param eventType - type name of {@link Event} entity
     */
    void create(@NotNull final String eventJson, @NotNull final String eventType);

    /**
     * Retrieves metadata for event type.
     *
     * @param eventType - type name of {@link Event} entity
     * @return metadata of provided event type
     */
    String getEventTypesMetadata(@NotNull final String eventType);

    /**
     * Retrieves {@link CodeListValue} entities.
     *
     * @param codeListName - name of code list
     * @param locale       - {@link Locale} entity
     * @return list of {@link CodeListValue} entities
     */
    List<CodeListValue> getCodeList(@NotNull final String codeListName, @NotNull final Locale locale);
}
