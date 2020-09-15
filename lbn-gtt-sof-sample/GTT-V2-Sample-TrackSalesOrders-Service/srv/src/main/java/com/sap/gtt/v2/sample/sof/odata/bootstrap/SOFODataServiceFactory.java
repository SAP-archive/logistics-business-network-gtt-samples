package com.sap.gtt.v2.sample.sof.odata.bootstrap;

import com.sap.gtt.v2.sample.sof.exception.SOFServiceException;
import com.sap.gtt.v2.sample.sof.odata.SOFODataSingleProcessor;
import com.sap.gtt.v2.sample.sof.odata.model.SalesOrder;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import com.sap.gtt.v2.sample.sof.utils.SpringContextUtils;
import org.apache.olingo.odata2.annotation.processor.core.edm.AnnotationEdmProvider;
import org.apache.olingo.odata2.annotation.processor.core.util.ClassHelper;
import org.apache.olingo.odata2.api.ODataCallback;
import org.apache.olingo.odata2.api.ODataService;
import org.apache.olingo.odata2.api.ODataServiceFactory;
import org.apache.olingo.odata2.api.ep.EntityProvider;
import org.apache.olingo.odata2.api.exception.ODataException;
import org.apache.olingo.odata2.api.processor.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.core.type.classreading.CachingMetadataReaderFactory;
import org.springframework.core.type.classreading.MetadataReader;
import org.springframework.core.type.classreading.MetadataReaderFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.SystemPropertyUtils;

import java.io.IOException;
import java.util.*;

import static com.sap.gtt.v2.sample.sof.exception.SOFServiceException.MESSAGE_CODE_ERROR_ODATA_INIT_FAILED;
import static com.sap.gtt.v2.sample.sof.exception.SOFServiceException.MESSAGE_CODE_ERROR_PACKAGE_SCAN;

@Component
public class SOFODataServiceFactory extends ODataServiceFactory {
    private static final Logger logger = LoggerFactory.getLogger(SOFODataServiceFactory.class);
    private static final AnnotationEdmProvider edmProvider;

    static {

        try {
            List<Class<?>> annotatedClassesFinal = new ArrayList<>();
            String packageToScan = SalesOrder.class.getPackage().getName();
            Collection<Class<?>> annotatedClasses = scanPackage(packageToScan, null);

            annotatedClassesFinal.addAll(annotatedClasses);

            edmProvider = new AnnotationEdmProvider(annotatedClassesFinal);
        } catch (ODataException e) {
            throw new SOFServiceException(MESSAGE_CODE_ERROR_ODATA_INIT_FAILED);
        }

    }

    @Override
    public ODataService createService(ODataContext odataContext) {
        ODataSingleProcessor singleProcessor = SpringContextUtils.getBean(SOFODataSingleProcessor.class);
        return createODataSingleProcessorService(edmProvider, singleProcessor);
    }

    public static Set<Class<?>> scanPackage(String basePackage, ClassHelper.ClassValidator validator) {
        Set<Class<?>> classes = new HashSet<>();
        try {
            String packageSearchPath = ResourcePatternResolver.CLASSPATH_ALL_URL_PREFIX
                    + org.springframework.util.ClassUtils.convertClassNameToResourcePath(
                    SystemPropertyUtils.resolvePlaceholders(basePackage))
                    + "/**/*.class";
            ResourcePatternResolver resourcePatternResolver = new PathMatchingResourcePatternResolver();
            MetadataReaderFactory metadataReaderFactory = new CachingMetadataReaderFactory(resourcePatternResolver);

            Resource[] resources = resourcePatternResolver.getResources(packageSearchPath);

            for (int i = 0; i < resources.length; i++) {
                Resource resource = resources[i];
                classes.addAll(scanResource(resource, metadataReaderFactory, validator));
            }
        } catch (IOException e) {
            logger.error("package scan failed", e);
            throw new SOFServiceException(MESSAGE_CODE_ERROR_PACKAGE_SCAN);
        }
        return classes;
    }

    protected static Set<Class<?>> scanResource(Resource resource, MetadataReaderFactory metadataReaderFactory,
                                                ClassHelper.ClassValidator validator) {
        Set<Class<?>> classes = new HashSet<>();
        try {
            if (resource.isReadable()) {
                MetadataReader metadataReader = metadataReaderFactory.getMetadataReader(resource);

                Class<?> clazz = Class.forName(metadataReader.getClassMetadata().getClassName());
                boolean valid = true;
                if (validator != null) {
                    valid = validator.isClassValid(clazz);
                }
                if (valid) {
                    classes.add(clazz);
                }

            }
        } catch (ClassNotFoundException | IOException e) {
            throw new SOFServiceException(e);
        }
        return classes;
    }


    @SuppressWarnings("unchecked")
    @Override
    public <T extends ODataCallback> T getCallback(final Class<T> callbackInterface) {
        T result;
        if (callbackInterface.isAssignableFrom(GlobalErrorCallback.class)) {
            result = (T) new GlobalErrorCallback();
        } else {
            result = super.getCallback(callbackInterface);
        }

        return result;
    }

    public static class GlobalErrorCallback implements ODataErrorCallback {
        private static final Logger logger = LoggerFactory.getLogger(GlobalErrorCallback.class);

        @Override
        public ODataResponse handleError(ODataErrorContext context) {
            String json = SOFUtils.getGson().toJson(context);
            logger.error("{}", json, context.getException());

            return EntityProvider.writeErrorDocument(context);
        }

    }
}
