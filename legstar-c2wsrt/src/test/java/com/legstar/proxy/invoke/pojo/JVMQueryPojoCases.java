package com.legstar.proxy.invoke.pojo;

import java.util.HashMap;
import java.util.Map;

import com.legstar.proxy.invoke.DirectOperationProxy;

/**
 * Centralize data for JVMQuery test case.
 *
 */
public final class JVMQueryPojoCases {

    /**
     * Utility class.
     */
    private JVMQueryPojoCases() {
        
    }
    
    /**
     * @return a valid proxy configuration
     */
    private static Map < String, String > getConfig() {
        Map < String, String > config = new HashMap < String, String >();
        config.put(PojoInvoker.POJO_CLASS_NAME_PROPERTY,
                "com.legstar.xsdc.test.cases.jvmquery.JVMQuery");
        config.put(PojoInvoker.POJO_METHOD_NAME_PROPERTY, "queryJvm");
        return config;
    }

    /**
     * @return a complete configuration that result in direct proxying.
     */
    public static Map < String, String > getDirectConfig() {
        Map < String, String > config = getConfig();
        config.put(DirectOperationProxy.REQUEST_TRANSFORMERS_CLASS_NAME_PROPERTY,
        "com.legstar.test.coxb.jvmquery.bind.JVMQueryRequestTransformers");
        config.put(DirectOperationProxy.RESPONSE_TRANSFORMERS_CLASS_NAME_PROPERTY,
        "com.legstar.test.coxb.jvmquery.bind.JVMQueryReplyTransformers");
        return config;
    }

    /**
     * @return a complete configuration that result in reflection proxying.
     */
    public static Map < String, String > getReflectConfig() {
        Map < String, String > config = getConfig();
        return config;
    }
}
