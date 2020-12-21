package com.sap.gtt.v2.sample.sst.odata.filter;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.apache.olingo.odata2.api.edm.EdmType;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class PropertyExpressionImplTest {

    private final PropertyExpressionImpl propertyExpression = new PropertyExpressionImpl("test");

    @Test
    void getKind_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(propertyExpression::getKind);
    }

    @Test
    void getEdmType_shouldThrowUnsupportedOperationException() {
        // when-then
        assertThrows(UnsupportedOperationException.class, propertyExpression::getEdmType);
    }

    @Test
    void setEdmType_shouldThrowUnsupportedOperationException() {
        final EdmType edmType = Mockito.mock(EdmType.class);

        // when-then
        assertThrows(UnsupportedOperationException.class, () -> propertyExpression.setEdmType(edmType));
    }

    @Test
    void getUriLiteral_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(propertyExpression::getUriLiteral);
    }

    @Test
    void getPropertyName_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(propertyExpression::getPropertyName);
    }

    @Test
    void getEdmProperty_shouldThrowUnsupportedOperationException() {
        // when-then
        assertThrows(UnsupportedOperationException.class, propertyExpression::getEdmProperty);
    }
}
