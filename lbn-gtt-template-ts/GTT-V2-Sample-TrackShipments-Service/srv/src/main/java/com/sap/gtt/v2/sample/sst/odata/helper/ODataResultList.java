package com.sap.gtt.v2.sample.sst.odata.helper;

import java.util.ArrayList;
import java.util.List;

/**
 * {@link ODataResultList} is used for wrapping required OData requests in convenient way.
 *
 * @author Min Li
 */
public class ODataResultList<T> {

    private Integer count;

    private List<T> results = new ArrayList<>();

    public Integer getCount() {
        return count;
    }

    public void setCount(Integer count) {
        this.count = count;
    }

    public List<T> getResults() {
        return results;
    }

    public void setResults(List<T> results) {
        this.results = results;
    }

    @Override
    public String toString() {
        return "ODataResultList{" +
                "count=" + count +
                ", results=" + results +
                '}';
    }
}
