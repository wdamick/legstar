/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cixs.gen.model;

import java.util.Properties;

import com.legstar.coxb.util.NameUtil;

/**
 * This class describes a Service which maps a set of mainframe programs
 * (each program maps with an operation).
 * This model is usable for both inbound (Mainframe calling Web Service) and
 * outbound (Web Service calling Mainframe).
 * Not all properties are useful for both inbound and outbound but it is simpler
 * to have a single model for both.
 */
public abstract class AbstractCixsService extends CixsMappingModel {

    /** Default suffix for class implementation name. */
    private static final String DEFAULT_IMPL_SUFFIX = "Impl";

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */

    /** Service package name. */
    public static final String SERVICE_PACKAGE_NAME = "servicePackageName";

    /** Namespace used for JAXB objects derived from service. */
    public static final String SERVICE_NAMESPACE = "serviceNamespace";

    /** Service interface class name. */
    public static final String SERVICE_INTERFACE_CLASSNAME = "serviceInterfaceClassName";

    /** Service implementation class name. */
    public static final String SERVICE_IMPLEMENTATION_CLASSNAME = "serviceImplementationClassName";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /** Service package name. */
    private String _servicePackageName;

    /** Namespace used for JAXB objects derived from service. */
    private String _serviceNamespace;

    /** Service interface class name. */
    private String _serviceInterfaceClassName;

    /** Service implementation class name. */
    private String _serviceImplementationClassName;

    /**
     * Construct an empty model.
     */
    public AbstractCixsService() {
        super();
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public AbstractCixsService(final Properties props) {
        super(props);
        setPackageName(getString(props, SERVICE_PACKAGE_NAME, null));
        setNamespace(getString(props, SERVICE_NAMESPACE, null));
        setInterfaceClassName(getString(props, SERVICE_INTERFACE_CLASSNAME,
                null));
        setImplementationClassName(getString(props,
                SERVICE_IMPLEMENTATION_CLASSNAME, null));
    }

    /**
     * @return the Service package name
     */
    public String getPackageName() {
        return _servicePackageName;
    }

    /**
     * @param packageName the Service package name to set
     */
    public void setPackageName(final String packageName) {
        _servicePackageName = packageName;
    }

    /**
     * @return the namespace used for JAXB objects derived from service
     */
    public String getNamespace() {
        return _serviceNamespace;
    }

    /**
     * @param namespace used for JAXB objects derived from service
     */
    public void setNamespace(final String namespace) {
        _serviceNamespace = namespace;
    }

    /**
     * @return the Service interface class name
     */
    public String getInterfaceClassName() {
        if (_serviceInterfaceClassName == null
                || _serviceInterfaceClassName.length() == 0) {
            return NameUtil.toClassName(getName());
        }
        return _serviceInterfaceClassName;
    }

    /**
     * @param interfaceClassName the Service interface class name to set
     */
    public void setInterfaceClassName(final String interfaceClassName) {
        _serviceInterfaceClassName = interfaceClassName;
    }

    /**
     * @return the Service implementation class name
     */
    public String getImplementationClassName() {
        if (_serviceImplementationClassName == null
                || _serviceImplementationClassName.length() == 0) {
            if (getName() == null) {
                return DEFAULT_IMPL_SUFFIX;
            }
            return NameUtil.toClassName(getName() + DEFAULT_IMPL_SUFFIX);
        }
        return _serviceImplementationClassName;
    }

    /**
     * @param implementationClassName the Service implementation class name to
     *            set
     */
    public void setImplementationClassName(
            final String implementationClassName) {
        _serviceImplementationClassName = implementationClassName;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        putString(props, SERVICE_PACKAGE_NAME, getPackageName());
        putString(props, SERVICE_NAMESPACE, getNamespace());
        putString(props, SERVICE_INTERFACE_CLASSNAME, getInterfaceClassName());
        putString(props, SERVICE_IMPLEMENTATION_CLASSNAME,
                getImplementationClassName());
        return props;
    }
}
