package com.sap.gtt.v2.sample.sst.odata.filter;

import org.apache.olingo.odata2.api.edm.EdmType;
import org.apache.olingo.odata2.api.edm.EdmTyped;
import org.apache.olingo.odata2.api.exception.ODataApplicationException;
import org.apache.olingo.odata2.api.uri.expression.CommonExpression;
import org.apache.olingo.odata2.api.uri.expression.ExceptionVisitExpression;
import org.apache.olingo.odata2.api.uri.expression.ExpressionKind;
import org.apache.olingo.odata2.api.uri.expression.ExpressionVisitor;
import org.apache.olingo.odata2.api.uri.expression.PropertyExpression;

/**
 * {@link PropertyExpressionImpl} represents the property in expression.
 *
 * @author Min Li
 */
public class PropertyExpressionImpl implements PropertyExpression {

    private final String name;

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
        return visitor.visitProperty(this, getUriLiteral(), null);
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
