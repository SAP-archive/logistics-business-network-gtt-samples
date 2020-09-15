package com.sap.gtt.v2.sample.sof.utils;


import com.google.gson.*;
import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.exception.SOFServiceException;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntityType;
import org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty;
import org.apache.olingo.odata2.api.annotation.edm.EdmProperty;
import org.apache.olingo.odata2.api.annotation.edm.EdmType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.*;

import static com.sap.gtt.v2.sample.sof.exception.SOFServiceException.MESSAGE_CODE_INVALID_JSON;
import static com.sap.gtt.v2.sample.sof.utils.SOFUtils.getGson;

public class ODataUtils {

    public static final String D = "d";
    public static final String COUNT = "__count";
    public static final String RESULTS = "results";

    private static final Logger logger = LoggerFactory.getLogger(ODataUtils.class);

    private ODataUtils() {

    }

    public static <T> ODataResultList<T> readEntitySet(String json, Class<T> classOfT) {
        JsonObject jsonObject = SOFUtils.getGson().fromJson(json, JsonObject.class);
        return readEntitySet(jsonObject, classOfT);
    }

    public static <T> ODataResultList<T> readEntitySet(JsonObject jsonObject, Class<T> classOfT) {
        ODataResultList<T> res = new ODataResultList<>();

        if (jsonObject.has(D)) {
            jsonObject = jsonObject.getAsJsonObject(D);
            if (jsonObject.has(COUNT) && !jsonObject.get(COUNT).isJsonNull()) {
                res.setCount(jsonObject.getAsJsonPrimitive(COUNT).getAsInt());
            }
        }

        if (jsonObject.has(RESULTS)) {
            JsonArray results = jsonObject.getAsJsonArray(RESULTS);
            for (JsonElement jsonElement : results) {
                res.getResults().add(readEntity(jsonElement.getAsJsonObject(), classOfT));
            }
        }

        return res;
    }

    public static <T> T readEntity(String json, Class<T> classOfT) {
        return readEntity(getGson().fromJson(json, JsonObject.class), classOfT);
    }

    public static <T> T readEntity(JsonObject jsonObject, Class<T> classOfT) {
        if (jsonObject.has(D)) {
            jsonObject = jsonObject.getAsJsonObject(D);
        }
        EdmEntityType declaredAnnotation = classOfT.getDeclaredAnnotation(EdmEntityType.class);
        if (declaredAnnotation != null) {
            return readEntityEdm(jsonObject, classOfT);
        } else {
            return readEntityPojo(jsonObject, classOfT);
        }
    }

    private static <T> T readEntityPojo(JsonObject jsonObject, Class<T> classOfT) {
        T t;
        try {
            t = classOfT.newInstance();
            Field[] fields = FieldUtils.getAllFields(classOfT);
            for (Field field : fields) {
                String propName;
                propName = getPropertyNameOfPojo(field);
                if (!jsonObject.has(propName)) {
                    continue;
                }
                ReflectionUtils.makeAccessible(field);
                if (isIterable(field)) {
                    field.set(t, readEntitySet(jsonObject.getAsJsonObject(propName), getActualType(field)).getResults());
                } else if (!isSimpleType(field)) {
                    if (!jsonObject.get(propName).isJsonNull()) {
                        field.set(t, readEntity(jsonObject.getAsJsonObject(propName), field.getType()));
                    }
                } else {
                    field.set(t, getPropertyValue(getAsJsonPrimitive(jsonObject, propName), field));
                }
            }

        } catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
            logger.warn("create instance of {} failed", classOfT, e);
            return null;
        } catch (JsonParseException e) {
            logger.warn("json parsing of {} failed", classOfT, e);
            return null;
        }
        return t;
    }

    private static String getPropertyNameOfPojo(Field field) {
        String propName;
        SerializedName serializedName = field.getDeclaredAnnotation(SerializedName.class);
        if (serializedName == null) {
            propName = field.getName();
        } else {
            propName = serializedName.value();
        }
        return propName;
    }

    private static boolean isSimpleType(Field field) {
        Class<?> type = field.getType();
        return type.isPrimitive()
                || type.equals(Boolean.class) || type.equals(Character.class)
                || type.equals(Byte.class) || type.equals(Short.class)
                || type.equals(Integer.class) || type.equals(Long.class)
                || type.equals(Float.class) || type.equals(Double.class)
                || type.equals(BigDecimal.class) || type.equals(BigInteger.class)
                || type.equals(Calendar.class) || type.equals(ZonedDateTime.class)
                || type.equals(Date.class) || type.equals(Instant.class)
                || type.equals(LocalDateTime.class) || type.equals(UUID.class)
                || type.equals(String.class);
    }

    private static boolean isIterable(Field field) {
        return Iterable.class.isAssignableFrom(field.getType());
    }

    private static Class<?> getActualType(Field field) throws ClassNotFoundException {
        ParameterizedType parameterizedType = (ParameterizedType) field.getGenericType();
        Type actualType = parameterizedType.getActualTypeArguments()[0];
        return Class.forName(actualType.getTypeName());
    }

    private static <T> T readEntityEdm(JsonObject jsonObject, Class<T> classOfT) {
        T t;
        try {
            t = classOfT.newInstance();
            fillNormalFields(jsonObject, classOfT, t);
            fillNavigationFields(jsonObject, classOfT, t);
        } catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
            logger.error("create instance of {} failed", classOfT, e);
            return null;
        } catch (JsonParseException e) {
            throw new SOFServiceException(MESSAGE_CODE_INVALID_JSON);
        }
        return t;
    }

    private static <T> void fillNavigationFields(JsonObject jsonObject, Class<T> classOfT, T t) throws ClassNotFoundException, IllegalAccessException {
        List<Field> navigationFields = FieldUtils.getFieldsListWithAnnotation(classOfT, EdmNavigationProperty.class);
        for (Field field : navigationFields) {
            ReflectionUtils.makeAccessible(field);
            EdmNavigationProperty declaredAnnotation = field.getDeclaredAnnotation(EdmNavigationProperty.class);
            String propName = getPropertyName(field, declaredAnnotation);
            if (!jsonObject.has(propName)) {
                continue;
            }
            fillNavigationFieldsInternal(jsonObject, t, field, declaredAnnotation, propName);
        }
    }

    private static <T> void fillNavigationFieldsInternal(JsonObject jsonObject, T t, Field field, EdmNavigationProperty declaredAnnotation, String propName) throws ClassNotFoundException, IllegalAccessException {
        Class<?> clazz = declaredAnnotation.toType();
        if (declaredAnnotation.toMultiplicity().equals(EdmNavigationProperty.Multiplicity.MANY)) {
            if (clazz == null) {
                clazz = getActualType(field);
            }
            field.set(t, readEntitySet(jsonObject.getAsJsonObject(propName), clazz).getResults());
        } else {
            if (clazz == null) {
                clazz = field.getType();
            }
            if (jsonObject.get(propName) == null || jsonObject.get(propName).isJsonNull()) {
                field.set(t, null);
            } else {
                field.set(t, readEntityEdm(jsonObject.getAsJsonObject(propName), clazz));
            }
        }
    }

    private static <T> void fillNormalFields(JsonObject jsonObject, Class<T> classOfT, T t) throws IllegalAccessException {
        List<Field> fields = FieldUtils.getFieldsListWithAnnotation(classOfT, EdmProperty.class);
        for (Field field : fields) {
            ReflectionUtils.makeAccessible(field);
            String propName = getPropertyName(field);
            if (!jsonObject.has(propName)) {
                continue;
            }
            field.set(t, getPropertyValue(getAsJsonPrimitive(jsonObject, propName), field));
        }
    }

    private static JsonPrimitive getAsJsonPrimitive(JsonObject jsonObject, String propName) {
        if (jsonObject.get(propName).isJsonNull()) {
            return null;
        }
        return jsonObject.getAsJsonPrimitive(propName);
    }

    private static String getPropertyName(Field field) {
        String propName = field.getDeclaredAnnotation(EdmProperty.class).name();
        if (StringUtils.isEmpty(propName)) {
            propName = field.getName();
        }
        return propName;
    }

    private static String getPropertyName(Field field, EdmNavigationProperty declaredAnnotation) {
        String propName = declaredAnnotation.name();
        if (StringUtils.isEmpty(propName)) {
            propName = field.getName();
        }
        return propName;
    }

    private static Object getPropertyValue(JsonPrimitive jsonPrimitive, Field field) {
        if (jsonPrimitive == null || jsonPrimitive.isJsonNull()) {
            return null;
        }
        Object obj = getPropertyValuePrimitive(jsonPrimitive, field);
        if (obj == null) {
            obj = getPropertyValueOther(jsonPrimitive, field);
        }
        return obj;
    }

    private static Object getPropertyValuePrimitive(JsonPrimitive jsonPrimitive, Field field) {
        Object obj = null;
        Class<?> type = field.getType();
        if (isInt(type)) {
            obj = jsonPrimitive.getAsInt();
        } else if (isLong(type)) {
            obj = getLongValue(jsonPrimitive, field);
        } else if (isDouble(type)) {
            obj = jsonPrimitive.getAsDouble();
        } else if (isFloat(type)) {
            obj = jsonPrimitive.getAsFloat();
        } else if (isShort(type)) {
            obj = jsonPrimitive.getAsShort();
        } else if (isByte(type)) {
            obj = jsonPrimitive.getAsByte();
        } else if (isBoolean(type)) {
            obj = jsonPrimitive.getAsBoolean();
        } else if (isChar(type)) {
            obj = jsonPrimitive.getAsCharacter();
        } else if (isNumber(type)) {
            obj = jsonPrimitive.getAsNumber();
        }
        return obj;
    }

    private static boolean isNumber(Class<?> type) {
        return type.equals(Number.class);
    }

    private static boolean isChar(Class<?> type) {
        return type.equals(Character.class) || type.equals(char.class);
    }

    private static boolean isBoolean(Class<?> type) {
        return type.equals(Boolean.class) || type.equals(boolean.class);
    }

    private static boolean isByte(Class<?> type) {
        return type.equals(Byte.class) || type.equals(byte.class);
    }

    private static boolean isShort(Class<?> type) {
        return type.equals(Short.class) || type.equals(short.class);
    }

    private static boolean isFloat(Class<?> type) {
        return type.equals(Float.class) || type.equals(float.class);
    }

    private static boolean isDouble(Class<?> type) {
        return type.equals(Double.class) || type.equals(double.class);
    }

    private static boolean isLong(Class<?> type) {
        return type.equals(Long.class) || type.equals(long.class);
    }

    private static boolean isInt(Class<?> type) {
        return type.equals(Integer.class) || type.equals(int.class);
    }

    private static Object getPropertyValueOther(JsonPrimitive jsonPrimitive, Field field) {
        Object obj = null;
        Class<?> type = field.getType();
        if (type.equals(BigDecimal.class)) {
            obj = jsonPrimitive.getAsBigDecimal();
        } else if (type.equals(BigInteger.class)) {
            obj = jsonPrimitive.getAsBigInteger();
        } else if (type.equals(Date.class)) {
            obj = new Date(normalizeTimestamp(jsonPrimitive.getAsString()));
        } else if (type.equals(Calendar.class)) {
            obj = Calendar.getInstance();
            ((Calendar) obj).setTimeInMillis(normalizeTimestamp(jsonPrimitive.getAsString()));
        } else if (type.equals(ZonedDateTime.class)) {
            obj = ZonedDateTime.ofInstant(Instant.ofEpochMilli(normalizeTimestamp(jsonPrimitive.getAsString())), ZoneId.systemDefault());
        } else if (type.equals(LocalDateTime.class)) {
            obj = LocalDateTime.ofInstant(Instant.ofEpochMilli(normalizeTimestamp(jsonPrimitive.getAsString())), ZoneId.systemDefault());
        } else if (type.equals(UUID.class)) {
            obj = UUID.fromString(jsonPrimitive.getAsString());
        } else if (type.isEnum()) {
            // obj = Enum.valueOf(field.getType(), jsonPrimitive.getAsString());
        } else {
            obj = jsonPrimitive.getAsString();
        }
        return obj;
    }

    private static Object getLongValue(JsonPrimitive jsonPrimitive, Field field) {
        Object obj;
        EdmType edmType = getEdmType(field);
        if (Objects.equals(edmType, EdmType.DATE_TIME_OFFSET) ||
                Objects.equals(edmType, EdmType.DATE_TIME) || jsonPrimitive.isString()) {
            obj = normalizeTimestamp(jsonPrimitive.getAsString());
        } else {
            obj = jsonPrimitive.getAsLong();
        }
        return obj;
    }

    private static EdmType getEdmType(Field field) {
        if (field.getDeclaredAnnotation(EdmProperty.class) != null) {
            return field.getDeclaredAnnotation(EdmProperty.class).type();
        }
        return null;
    }

    public static long normalizeTimestamp(String timestamp) {
        timestamp = timestamp.replaceAll(".*/Date\\(([\\d\\+\\-]+)\\)/.*", "$1");
        return Long.parseLong(timestamp);
    }

    public static Map<String, Object> toMap(Object obj) {
        Map<String, Object> map = new HashMap<>();
        if (obj == null) {
            return map;
        }
        try {
            List<Field> fields = FieldUtils.getFieldsListWithAnnotation(obj.getClass(), EdmProperty.class);
            for (Field field : fields) {
                ReflectionUtils.makeAccessible(field);
                map.put(getPropertyName(field), field.get(obj));
            }

            List<Field> navigationFields = FieldUtils.getFieldsListWithAnnotation(obj.getClass(), EdmNavigationProperty.class);
            for (Field field : navigationFields) {
                ReflectionUtils.makeAccessible(field);
                EdmNavigationProperty declaredAnnotation = field.getDeclaredAnnotation(EdmNavigationProperty.class);
                String propName = getPropertyName(field, declaredAnnotation);
                if (declaredAnnotation.toMultiplicity().equals(EdmNavigationProperty.Multiplicity.MANY)) {
                    Iterable values = (Iterable) field.get(obj);
                    List<Map<String, Object>> mapList = new ArrayList<>();
                    if (values == null) {
                        map.put(propName, mapList);
                        continue;
                    }
                    for (Object val : values) {
                        mapList.add(toMap(val));
                    }
                    map.put(propName, mapList);
                } else {
                    map.put(propName, toMap(field.get(obj)));
                }
            }
        } catch (IllegalAccessException e) {
            logger.error("get field value of {} failed", obj, e);
            return null;
        }

        return map;
    }
}
