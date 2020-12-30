package com.sap.gtt.v2.sample.sof.odata.filter;

import org.apache.olingo.odata2.api.edm.EdmSimpleTypeKind;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;

/**
 * FilterCondition is the class to help the expression build
 * @propertyName property in expression
 * @edmType value type
 * @literalValue value
 * @binaryOperator operator
 * **/

public class FilterCondition {

    public static final String EDM_TYPE_STRING = EdmSimpleTypeKind.String.getFullQualifiedName().toString(); //"Edm.String";
    public static final String EDM_TYPE_DATE_TIME_OFFSET = EdmSimpleTypeKind.DateTimeOffset.getFullQualifiedName().toString(); //"Edm.DateTimeOffset";
    public static final String EDM_TYPE_GUID = EdmSimpleTypeKind.Guid.getFullQualifiedName().toString(); //"Edm.Guid";
    public static final String EDM_TYPE_BOOLEAN = EdmSimpleTypeKind.Boolean.getFullQualifiedName().toString(); //"Edm.Boolean";
    public static final String EDM_TYPE_DOUBLE = EdmSimpleTypeKind.Double.getFullQualifiedName().toString(); // "Edm.Double";
    public static final String EDM_TYPE_INT32 = EdmSimpleTypeKind.Int32.getFullQualifiedName().toString(); // "Edm.Int32";
    public static final String EDM_TYPE_INT64 = EdmSimpleTypeKind.Int64.getFullQualifiedName().toString(); // "Edm.Int64";

    private String propertyName;
    private String edmType;
    private String literalValue;
    private BinaryOperator binaryOperator;

    public FilterCondition(String propertyName, String edmType, String literalValue,
                           BinaryOperator binaryOperator) {
        super();
        this.propertyName = propertyName;
        this.edmType = edmType;
        this.literalValue = literalValue;
        this.binaryOperator = binaryOperator;
    }

    public String getPropertyName() {
        return propertyName;
    }

    public String getEdmType() {
        return edmType;
    }

    public String getLiteralValue() {
        return literalValue;
    }

    public BinaryOperator getBinaryOperator() {
        return binaryOperator;
    }
}
