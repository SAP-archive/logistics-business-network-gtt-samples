package com.sap.gtt.v2.sample.sst.common.validator;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;

class CoordinatesValidatorTest {

    private final CoordinatesValidator coordinatesValidator = new CoordinatesValidator();

    @Test
    void isValid_givenValidCoordinates_shouldPassValidation() {
        // given
        final BigDecimal longitude = BigDecimal.valueOf(100);
        final BigDecimal latitude = BigDecimal.valueOf(80);

        // when-then
        assertTrue(coordinatesValidator.isValid(longitude, latitude));
    }

    @Test
    void isValid_givenInvalidCoordinates_shouldFailValidation() {
        // given
        final BigDecimal longitude = BigDecimal.valueOf(200);
        final BigDecimal latitude = BigDecimal.valueOf(-100);

        // when-then
        assertFalse(coordinatesValidator.isValid(longitude, latitude));
    }
}
