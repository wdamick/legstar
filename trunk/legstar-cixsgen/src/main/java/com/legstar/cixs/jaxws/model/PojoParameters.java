package com.legstar.cixs.jaxws.model;

import java.util.Map;

import com.legstar.codegen.CodeGenMakeException;

/**
 * Set of parameters describing a POJO.
 */
public class PojoParameters {

    /* ====================================================================== */
    /* = Constants section                                                  = */
    /* ====================================================================== */
    /** POJO Class name. */ 
    public static final String POJO_CLASS_NAME_PROPERTY = "pojoClassName";

    /** POJO method name. */ 
    public static final String POJO_METHOD_NAME_PROPERTY = "pojoMethodName";

    /* ====================================================================== */
    /* = Properties section                                                 = */
    /* ====================================================================== */
    /** The target POJO fully qualified class name. */
    private String mClassName;

    /** The target POJO method name. */
    private String mMethodName;

    /**
     * POJO parameters are expected by templates to come from a parameters map.
     * @param parameters a parameters map to which POJO parameters must be added
     */
    public void add(final Map < String, Object > parameters) {
        parameters.put(POJO_CLASS_NAME_PROPERTY, getClassName());
        parameters.put(POJO_METHOD_NAME_PROPERTY, getMethodName());
    }

    /**
     * When target is a POJO, check that corresponding parameters are set correctly.
     * @throws CodeGenMakeException if parameters are missing or wrong
     */
    public void check() throws CodeGenMakeException {
        if (getClassName() == null || getClassName().length() == 0) {
            throw new CodeGenMakeException(
            "Missing target POJO implementation class name");
        }
        if (getMethodName() == null || getMethodName().length() == 0) {
            throw new CodeGenMakeException(
            "Missing target POJO method name");
        }
    }

    /**
     * @return target POJO fully qualified class name
     */
    public String getClassName() {
        return mClassName;
    }

    /**
     * @param className target POJO fully qualified class name
     */
    public void setClassName(final String className) {
        mClassName = className;
    }

    /**
     * @return target POJO method name
     */
    public String getMethodName() {
        return mMethodName;
    }

    /**
     * @param methodName target POJO method name
     */
    public void setMethodName(final String methodName) {
        mMethodName = methodName;
    }
}
