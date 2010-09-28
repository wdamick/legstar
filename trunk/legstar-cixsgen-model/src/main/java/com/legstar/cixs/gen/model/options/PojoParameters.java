/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cixs.gen.model.options;

import java.util.Map;
import java.util.Properties;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.AbstractPropertiesModel;

/**
 * Set of parameters describing a POJO.
 */
public class PojoParameters extends AbstractPropertiesModel {

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */
    /** POJO Class name. */
    public static final String POJO_CLASS_NAME_PROPERTY = "pojoClassName";

    /** POJO method name. */
    public static final String POJO_METHOD_NAME_PROPERTY = "pojoMethodName";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */
    /** The target POJO fully qualified class name. */
    private String mClassName;

    /** The target POJO method name. */
    private String mMethodName;

    /**
     * A no-Arg constructor.
     */
    public PojoParameters() {
        super();
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public PojoParameters(final Properties props) {
        super(props);
        setClassName(getString(props, POJO_CLASS_NAME_PROPERTY, null));
        setMethodName(getString(props, POJO_METHOD_NAME_PROPERTY, null));
    }

    /**
     * POJO parameters are expected by templates to come from a parameters map.
     * 
     * @param parameters a parameters map to which POJO parameters must be added
     */
    public void add(final Map < String, Object > parameters) {
        parameters.put(POJO_CLASS_NAME_PROPERTY, getClassName());
        parameters.put(POJO_METHOD_NAME_PROPERTY, getMethodName());
    }

    /**
     * When target is a POJO, check that corresponding parameters are set
     * correctly.
     * 
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

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        putString(props, POJO_CLASS_NAME_PROPERTY, getClassName());
        putString(props, POJO_METHOD_NAME_PROPERTY, getMethodName());
        return props;
    }
}
