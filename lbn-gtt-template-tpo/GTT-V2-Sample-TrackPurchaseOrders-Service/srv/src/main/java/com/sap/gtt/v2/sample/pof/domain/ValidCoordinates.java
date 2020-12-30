package com.sap.gtt.v2.sample.pof.domain;

import java.io.Serializable;

public class ValidCoordinates implements Serializable {
    private boolean invalid = false;
    private boolean missing = false;
    private boolean valid = true;

    public boolean isInvalid() {
        return invalid;
    }

    public void setInvalid(boolean invalid) {
        this.invalid = invalid;
    }

    public boolean isMissing() {
        return missing;
    }

    public void setMissing(boolean missing) {
        this.missing = missing;
    }

    public boolean isValid() {
        return valid;
    }

    public void setValid(boolean valid) {
        this.valid = valid;
    }
}
