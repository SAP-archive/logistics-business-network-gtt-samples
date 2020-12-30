package com.sap.gtt.v2.sample.sof.odata.filter;

import org.apache.commons.lang3.StringUtils;
import org.apache.olingo.odata2.api.edm.EdmLiteral;
import org.apache.olingo.odata2.api.edm.EdmSimpleType;
import org.apache.olingo.odata2.api.edm.EdmType;
import org.apache.olingo.odata2.api.uri.expression.CommonExpression;
import org.apache.olingo.odata2.api.uri.expression.ExpressionKind;
import org.apache.olingo.odata2.api.uri.expression.ExpressionVisitor;
import org.apache.olingo.odata2.api.uri.expression.LiteralExpression;
import org.apache.olingo.odata2.core.edm.EdmDateTimeOffset;
import org.apache.olingo.odata2.core.edm.EdmGuid;
import org.apache.olingo.odata2.core.edm.EdmString;

import static com.sap.gtt.v2.sample.sof.odata.filter.FilterCondition.*;

/**
 * LiteralExpressionImpl is represent the value in expression
 *
 * @type type of the value
 * @value value
 **/
public class LiteralExpressionImpl implements LiteralExpression {
    private String type;
    private String value;

    public LiteralExpressionImpl(String type, String value) {
        this.type = type;
        this.value = value;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    @Override
    public ExpressionKind getKind() {
        return ExpressionKind.LITERAL;
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
        String result = this.value;
        if (StringUtils.equals(this.type, EDM_TYPE_STRING)) {
            result = "'" + result + "'";
        } else if (StringUtils.equals(this.type, EDM_TYPE_DATE_TIME_OFFSET)) {
            result = "datetimeoffset'" + result + "'";
        } else if (StringUtils.equals(this.type, FilterCondition.EDM_TYPE_GUID)) {
            if (StringUtils.isBlank(result)) {
                result = "00000000-0000-0000-0000-000000000000";
            }
            result = "guid'" + result + "'";
        } else if (StringUtils.equals(this.type, FilterCondition.EDM_TYPE_INT64)) {
            result += "L";
        }
        return result;
    }

    @Override
    public Object accept(final ExpressionVisitor visitor) {
        Object ret = visitor.visitLiteral(this,
                new EdmLiteral(getEdmLiteral(type), value));
        return ret;
    }

    public static EdmSimpleType getEdmLiteral(String type) {
        if (type.equals(EDM_TYPE_DATE_TIME_OFFSET)) {
            return EdmDateTimeOffset.getInstance();
        } else if (type.equals(EDM_TYPE_STRING)) {
            return EdmString.getInstance();
        } else if (type.equals(EDM_TYPE_GUID)) {
            return EdmGuid.getInstance();
        } else {
            throw new UnsupportedOperationException("If needed, other conversions can be added.");
        }
    }
}
