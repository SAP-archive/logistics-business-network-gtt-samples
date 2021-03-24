package com.sap.gtt.v2.sample.sst.common.utils;

import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;

import static java.time.ZoneOffset.UTC;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class SSTUtilsTest {

    private static final String dateTimeRegex = "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z";

    @Test
    void getNormalizedUri_givenRequestUri_shouldReturnNormalizedUri() {
        // given
        final String requestUri = "https://dummy/sap/logistics/gtt/sample/sst/odata/v1/Shipment(guid%2773ca77e6-85b1-5523-b05c-726ec9ef3027%27)?$format=json";
        final String serviceRoot = "https://dummy:443/sap/logistics/gtt/sample/sst/odata/v1/";

        // when
        final String normalizedUri = SSTUtils.getNormalizedUri(requestUri, serviceRoot);

        // then
        assertThat(normalizedUri)
                .doesNotContain("443")
                .isEqualTo("/Shipment(guid%2773ca77e6-85b1-5523-b05c-726ec9ef3027%27)?$format=json");
    }

    @Test
    void getDateTimeString_givenTimeAsLong_shouldReturnFormattedDateTime() {
        // given
        final Long dateTime = LocalDateTime.now().toInstant(UTC).toEpochMilli();

        // when
        final String formattedDateTime = SSTUtils.getDateTimeString(dateTime);

        // then
        assertNotNull(formattedDateTime);
        assertTrue(formattedDateTime.matches(dateTimeRegex));
    }

    @Test
    void getDateTimeLong_givenDateTimeString_shouldReturnLongValue() {
        // given
        final String dateTime = "2020-01-01T00:00:00Z";

        // when
        final Long dateTimeLong = SSTUtils.getDateTimeLong(dateTime);

        // then
        assertNotNull(dateTimeLong);
    }

    @Test
    void removeParametersFromUrl_givenUriWithParameters_shouldRemoveParameters() {
        // given
        final String arrivalLocationParam = "arrivalLocation";
        final String departureLocationParam = "departureLocation";
        final String uri = "http://localhost:8080/sap/logistics/gtt/sample/sst/odata/v1/Shipment?$expand=arrivalLocation,departureLocation";

        // when
        final String uriWithoutParameters = SSTUtils.removeParametersFromUrl(uri, arrivalLocationParam, departureLocationParam);

        // then
        assertThat(uriWithoutParameters).doesNotContain(arrivalLocationParam, departureLocationParam);
    }

    @Test
    void removeParameterFromUri_givenUriWithParameter_shouldRemoveParameter() {
        // given
        final String arrivalLocationParam = "arrivalLocation";
        final String uri = "http://localhost:8080/sap/logistics/gtt/sample/sst/odata/v1/Shipment?$expand=arrivalLocation";

        // when
        final String uriWithoutParameter = SSTUtils.removeParameterFromUri(uri, arrivalLocationParam);

        // then
        assertThat(uriWithoutParameter).doesNotContain(arrivalLocationParam);
    }

    @Test
    void getEventTypeShortName_givenFullEventTypeName_shouldReturnShortName() {
        // given
        final String eventType = "TestEvent";
        final String eventTypeFullName = "com.test." + eventType;

        // when
        final String eventTypeShortName = SSTUtils.getEventTypeShortName(eventTypeFullName);

        // then
        assertEquals(eventType, eventTypeShortName);
    }
}
