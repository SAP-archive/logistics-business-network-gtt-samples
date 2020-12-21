package com.sap.gtt.v2.sample.pof.odata.filter;

import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.expression.FilterExpression;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

public class FilterBuilderTest {
    @Before
    public void setUp() {
        System.out.println("Start  " + this.getClass().getSimpleName());
    }

    @After
    public void tearDown() {
        System.out.println("Finish " + this.getClass().getSimpleName());
    }

    @Test
    public void testFilterBuilder() {
        List<FilterCondition> propertyConditions = new ArrayList<>();
        FilterCondition propertyCondition = null;

        propertyCondition = new FilterCondition("STRING", FilterCondition.EDM_TYPE_STRING, "stringvalue", BinaryOperator.EQ);
        propertyConditions.add(propertyCondition);

        propertyCondition = new FilterCondition("STRING", FilterCondition.EDM_TYPE_STRING, "stringvalue", BinaryOperator.NE);
        propertyConditions.add(propertyCondition);

        propertyCondition = new FilterCondition("INT32", FilterCondition.EDM_TYPE_INT32, "123", BinaryOperator.EQ);
        propertyConditions.add(propertyCondition);

        propertyCondition = new FilterCondition("DOUBLE", FilterCondition.EDM_TYPE_DOUBLE, "123.123", BinaryOperator.EQ);
        propertyConditions.add(propertyCondition);

        propertyCondition = new FilterCondition("OFFSET", FilterCondition.EDM_TYPE_DATE_TIME_OFFSET, "2018-06-05T16:00:00.000Z", BinaryOperator.EQ);
        propertyConditions.add(propertyCondition);

        propertyCondition = new FilterCondition("BOOLEAN", FilterCondition.EDM_TYPE_BOOLEAN, "false", BinaryOperator.EQ);
        propertyConditions.add(propertyCondition);

        FilterExpression filterExpressionAnd = FilterExpressionBuilder.createFilterExpression(propertyConditions, BinaryOperator.AND);
        System.out.println(filterExpressionAnd.getExpressionString());
        String expect = "(((((STRING eq 'stringvalue') and (STRING ne 'stringvalue')) and (INT32 eq 123)) and (DOUBLE eq 123.123)) and (OFFSET eq datetimeoffset'2018-06-05T16:00:00.000Z')) and (BOOLEAN eq false)";
        Assert.assertEquals(expect, filterExpressionAnd.getExpressionString());

        FilterExpression filterExpressionOr = FilterExpressionBuilder.createFilterExpression(propertyConditions, BinaryOperator.OR);

        expect = "(((((STRING eq 'stringvalue') or (STRING ne 'stringvalue')) or (INT32 eq 123)) or (DOUBLE eq 123.123)) or (OFFSET eq datetimeoffset'2018-06-05T16:00:00.000Z')) or (BOOLEAN eq false)";
        System.out.println(filterExpressionOr.getExpressionString());
        Assert.assertEquals(expect, filterExpressionOr.getExpressionString());

        FilterExpression filterExpressionCombine = FilterExpressionBuilder.createFilterExpression(filterExpressionAnd, filterExpressionOr, BinaryOperator.OR);
        expect = "((((((STRING eq 'stringvalue') and (STRING ne 'stringvalue')) and (INT32 eq 123)) and (DOUBLE eq 123.123)) and (OFFSET eq datetimeoffset'2018-06-05T16:00:00.000Z')) and (BOOLEAN eq false)) or ((((((STRING eq 'stringvalue') or (STRING ne 'stringvalue')) or (INT32 eq 123)) or (DOUBLE eq 123.123)) or (OFFSET eq datetimeoffset'2018-06-05T16:00:00.000Z')) or (BOOLEAN eq false))";
        System.out.println(filterExpressionCombine.getExpressionString());
        Assert.assertEquals(expect, filterExpressionCombine.getExpressionString());

        FilterExpression filterExpressionCombine1 = FilterExpressionBuilder.createFilterExpression(filterExpressionAnd, propertyConditions.get(0), BinaryOperator.OR);
        System.out.println(filterExpressionCombine1.getExpressionString());
        expect = "((((((STRING eq 'stringvalue') and (STRING ne 'stringvalue')) and (INT32 eq 123)) and (DOUBLE eq 123.123)) and (OFFSET eq datetimeoffset'2018-06-05T16:00:00.000Z')) and (BOOLEAN eq false)) or (STRING eq 'stringvalue')";
        Assert.assertEquals(expect, filterExpressionCombine1.getExpressionString());

        FilterExpression filterExpression = FilterExpressionBuilder.createFilterExpression(filterExpressionCombine1);
        Assert.assertEquals(expect, filterExpression.getExpressionString());

    }
}
