package com.sap.gtt.v2.sample.sof.odata.filter;

import org.apache.olingo.odata2.api.edm.EdmType;
import org.apache.olingo.odata2.api.exception.ODataApplicationException;
import org.apache.olingo.odata2.api.uri.expression.*;

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
        String left;
        String right;
        if (this.left instanceof PropertyExpressionImpl || this.left instanceof LiteralExpressionImpl)
            left=this.left.getUriLiteral();
        else
            left="("+this.left.getUriLiteral()+")";

        if (this.right instanceof PropertyExpressionImpl || this.right instanceof LiteralExpressionImpl)
            right=this.right.getUriLiteral();
        else
            right="("+this.right.getUriLiteral()+")";

        return left + " " + this.operator.toUriLiteral() + " " + right;
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
