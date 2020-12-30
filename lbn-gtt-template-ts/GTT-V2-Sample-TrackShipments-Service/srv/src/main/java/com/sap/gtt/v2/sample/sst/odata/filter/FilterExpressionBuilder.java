package com.sap.gtt.v2.sample.sst.odata.filter;

import static java.util.Objects.isNull;
import static java.util.Objects.nonNull;

import java.util.List;
import org.apache.olingo.odata2.api.uri.expression.BinaryExpression;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.expression.CommonExpression;
import org.apache.olingo.odata2.api.uri.expression.FilterExpression;
import org.apache.olingo.odata2.api.uri.expression.LiteralExpression;
import org.apache.olingo.odata2.api.uri.expression.PropertyExpression;
import org.apache.olingo.odata2.core.uri.expression.FilterExpressionImpl;

/**
 * {@link FilterExpressionBuilder} builder which helps to create {@link FilterExpression}.
 *
 * @author Min Li
 */
public class FilterExpressionBuilder {

    private FilterExpressionBuilder() {
    }

    public static FilterExpression createFilterExpression(List<FilterCondition> conditions, BinaryOperator andOr) {
        CommonExpression lastExpression = null;
        for (FilterCondition condition : conditions) {
            BinaryExpression binaryExpression = getBinaryExpression(condition);
            lastExpression = nonNull(lastExpression)
                    ? new BinaryExpressionImpl(lastExpression, binaryExpression, andOr)
                    : binaryExpression;
        }
        if (isNull(lastExpression)) {
            return null;
        } else {
            return new FilterExpressionImpl(lastExpression.getUriLiteral(), lastExpression);
        }
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
