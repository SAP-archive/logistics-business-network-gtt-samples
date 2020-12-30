package com.sap.gtt.v2.sample.sst.common.validator;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.MAX_LATITUDE;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.MAX_LONGITUDE;
import static java.util.Objects.nonNull;

import java.math.BigDecimal;
import javax.annotation.Nullable;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link CoordinatesValidator} is a validator which verifies that provided coordinates are valid.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class CoordinatesValidator {

    /**
     * Verifies that coordinates are valid.
     *
     * @param longitude - value of longitude
     * @param latitude  - value of latitude
     * @return true if coordinates are valid, otherwise - false
     */
    public boolean isValid(@Nullable final BigDecimal longitude, @Nullable final BigDecimal latitude) {
        return isEachValuePresent(longitude, latitude)
                && (isLongitudeValid(longitude) && isLatitudeValid(latitude));
    }

    private boolean isEachValuePresent(final BigDecimal longitude, final BigDecimal latitude) {
        return nonNull(longitude) && nonNull(latitude);
    }

    private boolean isLongitudeValid(final BigDecimal longitude) {
        return MAX_LONGITUDE.compareTo(longitude) >= 0
                && MAX_LONGITUDE.negate().compareTo(longitude) <= 0;
    }

    private boolean isLatitudeValid(final BigDecimal latitude) {
        return MAX_LATITUDE.compareTo(latitude) >= 0
                && MAX_LATITUDE.negate().compareTo(latitude) <= 0;
    }
}
