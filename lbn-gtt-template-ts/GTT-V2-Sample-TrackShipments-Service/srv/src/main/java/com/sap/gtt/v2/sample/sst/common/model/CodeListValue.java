package com.sap.gtt.v2.sample.sst.common.model;

/**
 * @author Aliaksandr Miron
 */
public class CodeListValue {

    private String code;

    private String name;

    private CodeListTranslation localized;

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public CodeListTranslation getLocalized() {
        return localized;
    }

    public void setLocalized(CodeListTranslation localized) {
        this.localized = localized;
    }
}
