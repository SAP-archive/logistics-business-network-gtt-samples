package com.sap.gtt.v2.sample.pof.odata.filter;

import org.apache.olingo.odata2.api.edm.EdmType;
import org.apache.olingo.odata2.api.edm.EdmTyped;
import org.apache.olingo.odata2.api.exception.ODataApplicationException;
import org.apache.olingo.odata2.api.uri.expression.*;

/**
 * PropertyExpressionImpl is represent the property in expression
 * @name the property name
 * **/

public class PropertyExpressionImpl implements PropertyExpression {

    private String name;

    public PropertyExpressionImpl(String name) {
        this.name = name;
    }

    @Override
    public ExpressionKind getKind() {
        return ExpressionKind.PROPERTY;
    }

    @Override
    public EdmType getEdmType() {
        throw new UnsupportedOperationException();
    }

    @Override
    public CommonExpression setEdmType(EdmType edmType) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getUriLiteral() {
        return name;
    }

    @Override
    public Object accept(ExpressionVisitor visitor) throws ExceptionVisitExpression, ODataApplicationException {
        Object ret = visitor.visitProperty(this, getUriLiteral(), null);
        return ret;
    }

    @Override
    public String getPropertyName() {
        return name;
    }

    @Override
    public EdmTyped getEdmProperty() {
        throw new UnsupportedOperationException();
    }

}
