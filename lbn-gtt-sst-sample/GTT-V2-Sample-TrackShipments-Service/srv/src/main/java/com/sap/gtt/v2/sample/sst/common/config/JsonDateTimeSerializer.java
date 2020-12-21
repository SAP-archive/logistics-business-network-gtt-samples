package com.sap.gtt.v2.sample.sst.common.config;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.DATE_TIME_PATTERN;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import org.springframework.stereotype.Component;

/**
 * {@link JsonDateTimeSerializer} is serializer which overrides the behavior on {@link Date} in JSON format.
 *
 * @author Aliaksandr Miron
 */
@Component
public class JsonDateTimeSerializer extends JsonSerializer<Long> {

    @Override
    public void serialize(Long value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
        final SimpleDateFormat formatter = new SimpleDateFormat(DATE_TIME_PATTERN);
        final Date date = new Date(value);
        final String formattedDateTime = formatter.format(date);
        gen.writeString(formattedDateTime);
    }
}
