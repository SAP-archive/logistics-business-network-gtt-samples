package com.sap.gtt.v2.sample.sst.odata.bootstrap;

import com.google.gson.Gson;
import com.sap.gtt.v2.sample.sst.common.exception.SSTServiceException;
import com.sap.gtt.v2.sample.sst.common.utils.SpringContextUtils;
import com.sap.gtt.v2.sample.sst.odata.SSTODataSingleProcessor;
import com.sap.gtt.v2.sample.sst.odata.model.ProcessStatus;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import org.apache.olingo.odata2.annotation.processor.core.edm.AnnotationEdmProvider;
import org.apache.olingo.odata2.annotation.processor.core.util.ClassHelper;
import org.apache.olingo.odata2.api.ODataCallback;
import org.apache.olingo.odata2.api.ODataService;
import org.apache.olingo.odata2.api.ODataServiceFactory;
import org.apache.olingo.odata2.api.ep.EntityProvider;
import org.apache.olingo.odata2.api.exception.ODataException;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.processor.ODataErrorCallback;
import org.apache.olingo.odata2.api.processor.ODataErrorContext;
import org.apache.olingo.odata2.api.processor.ODataResponse;
import org.apache.olingo.odata2.api.processor.ODataSingleProcessor;
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

/**
 * @author Min Li
 */
@Component
public class SSTODataServiceFactory extends ODataServiceFactory {

    private static final Logger logger = LoggerFactory.getLogger(SSTODataServiceFactory.class);
    private static final AnnotationEdmProvider edmProvider;

    static {
        try {
            String packageToScan = ProcessStatus.class.getPackage().getName();
            Collection<Class<?>> annotatedClasses = scanPackage(packageToScan, null);
            edmProvider = new AnnotationEdmProvider(annotatedClasses);
        } catch (ODataException e) {
            throw new SSTServiceException(SSTServiceException.MESSAGE_CODE_ERROR_ODATA_INIT_FAILED);
        }
    }

    @Override
    public ODataService createService(ODataContext odataContext) {
        ODataSingleProcessor singleProcessor = SpringContextUtils.getBean(SSTODataSingleProcessor.class);
        return createODataSingleProcessorService(edmProvider, singleProcessor);
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T extends ODataCallback> T getCallback(final Class<T> callbackInterface) {
        return callbackInterface.isAssignableFrom(GlobalErrorCallback.class)
                ? (T) new GlobalErrorCallback()
                : super.getCallback(callbackInterface);
    }

    public static class GlobalErrorCallback implements ODataErrorCallback {

        private static final Logger logger = LoggerFactory.getLogger(GlobalErrorCallback.class);

        @Override
        public ODataResponse handleError(ODataErrorContext context) {
            String json = new Gson().toJson(context);
            logger.error("{}", json, context.getException());
            return EntityProvider.writeErrorDocument(context);
        }
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

            for (Resource resource : resources) {
                classes.addAll(scanResource(resource, metadataReaderFactory, validator));
            }
        } catch (IOException e) {
            logger.error("package scan failed", e);
            throw new SSTServiceException(SSTServiceException.MESSAGE_CODE_ERROR_PACKAGE_SCAN);
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
            throw new SSTServiceException(e);
        }
        return classes;
    }
}
