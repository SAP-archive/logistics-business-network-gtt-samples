package com.sap.gtt.v2.sample.sst.odata.filter;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.apache.olingo.odata2.api.edm.EdmType;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class BinaryExpressionImplTest {

    private final BinaryExpressionImpl binaryExpression = new BinaryExpressionImpl(null, null, null);

    @Test
    void getKind_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(binaryExpression::getKind);
    }

    @Test
    void getEdmType_shouldThrowUnsupportedOperationException() {
        // when-then
        assertThrows(UnsupportedOperationException.class, binaryExpression::getEdmType);
    }

    @Test
    void setEdmType_shouldThrowUnsupportedOperationException() {
        final EdmType emdType = Mockito.mock(EdmType.class);

        // when-then
        assertThrows(UnsupportedOperationException.class, () -> binaryExpression.setEdmType(emdType));
    }

    @Test
    void getOperator_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(binaryExpression::getOperator);
    }

    @Test
    void getLeftOperand_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(binaryExpression::getLeftOperand);
    }

    @Test
    void getRightOperand_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(binaryExpression::getRightOperand);
    }
}
