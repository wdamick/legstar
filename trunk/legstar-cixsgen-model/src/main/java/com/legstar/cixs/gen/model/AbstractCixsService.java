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
package com.legstar.cixs.gen.model;

import com.legstar.coxb.util.Utils;

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

    /** Service package name. */
    private String mPackageName;

    /** Namespace used for JAXB objects derived from service. */
    private String mNamespace;

    /** Service interface class name. */
    private String mInterfaceClassName;

    /** Service implementation class name. */
    private String mImplementationClassName;

    /**
     * @return the Service package name
     */
    public final String getPackageName() {
        return mPackageName;
    }

    /**
     * @param packageName the Service package name to set
     */
    public final void setPackageName(final String packageName) {
        mPackageName = packageName;
    }

    /**
     * @return the namespace used for JAXB objects derived from service
     */
    public final String getNamespace() {
        return mNamespace;
    }

    /**
     * @param namespace used for JAXB objects derived from service
     */
    public final void setNamespace(final String namespace) {
        mNamespace = namespace;
    }

    /**
     * @return the Service interface class name
     */
    public final String getInterfaceClassName() {
        if (mInterfaceClassName == null || mInterfaceClassName.length() == 0) {
            return Utils.toClassName(getName());
        }
        return mInterfaceClassName;
    }

    /**
     * @param interfaceClassName the Service interface class name to set
     */
    public final void setInterfaceClassName(final String interfaceClassName) {
        mInterfaceClassName = interfaceClassName;
    }

    /**
     * @return the Service implementation class name
     */
    public final String getImplementationClassName() {
        if (mImplementationClassName == null 
                || mImplementationClassName.length() == 0) {
            if (getName() == null) {
                return DEFAULT_IMPL_SUFFIX;
            }
            return Utils.toClassName(getName() + DEFAULT_IMPL_SUFFIX);
        }
        return mImplementationClassName;
    }

    /**
     * @param implementationClassName the Service implementation class name to
     *  set
     */
    public final void setImplementationClassName(
            final String implementationClassName) {
        mImplementationClassName = implementationClassName;
    }

}
