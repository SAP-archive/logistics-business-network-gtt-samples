package com.sap.gtt.v2.sample.sst.odata.filter;

import static com.sap.gtt.v2.sample.sst.odata.filter.FilterCondition.EDM_TYPE_DATE_TIME_OFFSET;
import static com.sap.gtt.v2.sample.sst.odata.filter.FilterCondition.EDM_TYPE_GUID;
import static com.sap.gtt.v2.sample.sst.odata.filter.FilterCondition.EDM_TYPE_INT64;
import static com.sap.gtt.v2.sample.sst.odata.filter.FilterCondition.EDM_TYPE_STRING;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.apache.olingo.odata2.api.edm.EdmType;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class LiteralExpressionImplTest {

    private final LiteralExpressionImpl literalExpression = new LiteralExpressionImpl("test", "test");

    @Test
    void getEdmLiteral_shouldReturnStringEdmType() {
        // when_then
        assertDoesNotThrow(() -> LiteralExpressionImpl.getEdmLiteral(EDM_TYPE_STRING));
    }

    @Test
    void getEdmLiteral_shouldReturnDateTimeEdmType() {
        // when_then
        assertDoesNotThrow(() -> LiteralExpressionImpl.getEdmLiteral(EDM_TYPE_DATE_TIME_OFFSET));
    }

    @Test
    void getEdmLiteral_shouldReturnGuidEdmType() {
        // when_then
        assertDoesNotThrow(() -> LiteralExpressionImpl.getEdmLiteral(EDM_TYPE_GUID));
    }

    @Test
    void getKind_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(literalExpression::getKind);
    }

    @Test
    void getEdmType_shouldThrowUnsupportedOperationException() {
        // when-then
        assertThrows(UnsupportedOperationException.class, literalExpression::getEdmType);
    }

    @Test
    void setEdmType_shouldThrowUnsupportedOperationException() {
        final EdmType emdType = Mockito.mock(EdmType.class);

        // when-then
        assertThrows(UnsupportedOperationException.class, () -> literalExpression.setEdmType(emdType));
    }

    @Test
    void getUriLiteral_givenStringEdmType_shouldNotThrowException() {
        // given
        literalExpression.setType(EDM_TYPE_STRING);

        // then
        assertDoesNotThrow(literalExpression::getUriLiteral);
    }

    @Test
    void getUriLiteral_givenDateTimeEdmType_shouldNotThrowException() {
        // given
        literalExpression.setType(EDM_TYPE_DATE_TIME_OFFSET);

        // then
        assertDoesNotThrow(literalExpression::getUriLiteral);
    }

    @Test
    void getUriLiteral_givenIntegerEdmType_shouldNotThrowException() {
        // given
        literalExpression.setType(EDM_TYPE_INT64);

        // then
        assertDoesNotThrow(literalExpression::getUriLiteral);
    }

    @Test
    void getType_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(literalExpression::getType);
    }

    @Test
    void setType_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(() -> literalExpression.setType("test"));
    }

    @Test
    void getValue_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(literalExpression::getValue);
    }

    @Test
    void setValue_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(() -> literalExpression.setValue("test"));
    }
}
