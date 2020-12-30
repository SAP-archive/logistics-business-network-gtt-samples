package com.sap.gtt.v2.sample.pof.odata.filter;

import org.apache.olingo.odata2.api.edm.EdmType;
import org.apache.olingo.odata2.api.exception.ODataApplicationException;
import org.apache.olingo.odata2.api.uri.expression.BinaryExpression;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.expression.CommonExpression;
import org.apache.olingo.odata2.api.uri.expression.ExceptionVisitExpression;
import org.apache.olingo.odata2.api.uri.expression.ExpressionKind;
import org.apache.olingo.odata2.api.uri.expression.ExpressionVisitor;

public class BinaryExpressionImpl implements BinaryExpression {

    CommonExpression left;
    CommonExpression right;
    BinaryOperator operator;

    public BinaryExpressionImpl(CommonExpression left, CommonExpression right, BinaryOperator operator) {
        this.left = left;
        this.right = right;
        this.operator = operator;
    }

    @Override
    public ExpressionKind getKind() {
        return ExpressionKind.BINARY;
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
        return getStringLiteral(left) + " " + operator.toUriLiteral() + " " + getStringLiteral(right);
    }

    private String getStringLiteral(CommonExpression expression) {
        if (expression instanceof PropertyExpressionImpl || expression instanceof LiteralExpressionImpl)
            return expression.getUriLiteral();
        else
            return  "(" + expression.getUriLiteral() + ")";
    }

    @Override
    public Object accept(final ExpressionVisitor visitor) throws ExceptionVisitExpression, ODataApplicationException {
        Object retLeftSide = left.accept(visitor);
        Object retRightSide = right.accept(visitor);

        return visitor.visitBinary(this, operator, retLeftSide, retRightSide);
    }

    @Override
    public BinaryOperator getOperator() {
        return this.operator;
    }

    @Override
    public CommonExpression getLeftOperand() {
        return this.left;
    }

    @Override
    public CommonExpression getRightOperand() {
        return this.right;
    }

}
