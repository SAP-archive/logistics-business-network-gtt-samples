package com.sap.gtt.v2.sample.pof.odata.filter;

import org.apache.olingo.odata2.api.uri.expression.*;
import org.apache.olingo.odata2.core.uri.expression.FilterExpressionImpl;

import java.util.List;

public class FilterExpressionBuilder {
    private FilterExpressionBuilder() {
    }

    public static FilterExpression createFilterExpression(List<FilterCondition> conditions, BinaryOperator andOr) {
        CommonExpression lastExpression = null;
        for (FilterCondition condition : conditions) {

            BinaryExpression binaryExpression = getBinaryExpression(condition);
            if (lastExpression != null) {
                lastExpression = new BinaryExpressionImpl(lastExpression, binaryExpression, andOr);
            } else {
                lastExpression = binaryExpression;
            }
        }
        if (lastExpression == null) return null;
        return new FilterExpressionImpl(lastExpression.getUriLiteral(), lastExpression);
    }

    private static BinaryExpression getBinaryExpression(FilterCondition condition) {
        PropertyExpression propertyExpression = new PropertyExpressionImpl(condition.getPropertyName());
        LiteralExpression literalExpression = new LiteralExpressionImpl(condition.getEdmType(), condition.getLiteralValue());
        return new BinaryExpressionImpl(propertyExpression, literalExpression, condition.getBinaryOperator());

    }

    public static FilterExpression createFilterExpression(FilterExpression filterExpression, FilterCondition filterCondition, BinaryOperator andOr) {
        BinaryExpression createdExpression = getBinaryExpression(filterCondition);
        BinaryExpression newExpression = new BinaryExpressionImpl(filterExpression, createdExpression, andOr);
        return new FilterExpressionImpl(newExpression.getUriLiteral(), newExpression);
    }

    public static FilterExpression createFilterExpression(FilterExpression filterExpression1, FilterExpression filterExpression2, BinaryOperator andOr) {
        BinaryExpression newExpression = new BinaryExpressionImpl(filterExpression1, filterExpression2, andOr);
        return new FilterExpressionImpl(newExpression.getUriLiteral(), newExpression);
    }

    public static FilterExpression createFilterExpression(CommonExpression expression) {
        return new FilterExpressionImpl(expression.getUriLiteral(), expression);
    }

}
