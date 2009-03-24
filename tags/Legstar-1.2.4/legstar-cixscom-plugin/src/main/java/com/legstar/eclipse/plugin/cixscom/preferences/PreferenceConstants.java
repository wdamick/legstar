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
package com.legstar.eclipse.plugin.cixscom.preferences;

/**
 * Constant definitions for plug-in preferences.
 */
public final class PreferenceConstants {

    /** Generated Java classes package name prefix. */
    public static final String DEFAULT_CIXS_PACKAGE_NAME_PREFIX =
        "com.legstar.eclipse.plugin.cixscom.DefaultPackageNamePrefix";

    /** Folder where property files should be generated. */
    public static final String CIXS_TARGET_PROP_FOLDER =
        "com.legstar.eclipse.plugin.cixscom.PropFolder";

    /** Last generated java classes package name. */
    public static final String LAST_CIXS_PACKAGE_NAME =
        "com.legstar.eclipse.plugin.cixscom.LastJavaClassesPackageName";

    /** Last generated project name. */
    public static final String LAST_CIXS_PROJECT_NAME =
        "com.legstar.eclipse.plugin.cixscom.LastProjectName";

    /** Prefix of property storing a group button selection status in preferences. */
    public static final String GROUP_BUTTON_SELECTION =
        "com.legstar.eclipse.plugin.cixscom.GroupButton";

    /** Last pojo class name used for a proxy. */
    public static final String PROXY_LAST_POJO_CLASS_NAME =
        "com.legstar.eclipse.plugin.cixscom.proxy.LastPojoClassName";
    
    /** Last pojo method name used for a proxy. */
    public static final String PROXY_LAST_POJO_METHOD_NAME =
        "com.legstar.eclipse.plugin.cixscom.proxy.LastPojoMethodName";

    /** Last web service target namespace used for a proxy. */
    public static final String PROXY_LAST_WS_WSDL_TARGET_NAMESPACE =
        "com.legstar.eclipse.plugin.cixscom.proxy.LastWebServiceWsdlTargetNamespace";

    /** Last web service port name used for a proxy. */
    public static final String PROXY_LAST_WS_WSDL_PORT_NAME =
        "com.legstar.eclipse.plugin.cixscom.proxy.LastWebServiceWsdlPortName";

    /** Last web service service name used for a proxy. */
    public static final String PROXY_LAST_WS_WSDL_SERVICE_NAME =
        "com.legstar.eclipse.plugin.cixscom.proxy.LastWebServiceWsdlServiceName";

    /** Last web service target namespace used for a proxy. */
    public static final String PROXY_LAST_WS_WSDL_URL =
        "com.legstar.eclipse.plugin.cixscom.proxy.LastWebServiceWsdlUrl";

    /** Utility class. */
    private PreferenceConstants() {
        
    }
}
