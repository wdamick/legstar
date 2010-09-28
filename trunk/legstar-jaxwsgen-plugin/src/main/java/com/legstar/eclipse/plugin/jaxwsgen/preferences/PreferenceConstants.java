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
package com.legstar.eclipse.plugin.jaxwsgen.preferences;

/**
 * Constant definitions for plug-in preferences.
 */
public final class PreferenceConstants {

    /** J2ee folder where web deployment files should be generated. */
    public static final String DEFAULT_J2EE_WDD_FOLDER =
            "com.legstar.eclipse.plugin.jaxwsgen.DefaultJ2eeWDDFolder";

    /** J2ee folder where war files will be deployed. */
    public static final String DEFAULT_J2EE_WAR_FOLDER =
            "com.legstar.eclipse.plugin.jaxwsgen.DefaultTargetWarDir";

    /** Folder where sample COBOL programs are generated. */
    public static final String DEFAULT_COBOL_SAMPLE_FOLDER =
            "com.legstar.eclipse.plugin.jaxwsgen.cobolSampleFolder";

    /** Generated Web services target namespace prefix. */
    public static final String ADAPTER_WSDL_TARGET_NAMESPACE_PREFIX =
            "com.legstar.eclipse.plugin.jaxwsgen.adapter.wsdlTargetNamespacePrefix";

    /** Appended to project name to form a wsdl service name. */
    public static final String ADAPTER_WSDL_SERVICE_NAME_SUFFIX =
            "com.legstar.eclipse.plugin.jaxwsgen.adapter.wsdlServiceNameSuffix";

    /** Appended to project name to form a wsdl port name. */
    public static final String ADAPTER_WSDL_PORT_NAME_SUFFIX =
            "com.legstar.eclipse.plugin.jaxwsgen.adapter.wsdlPortNameSuffix";

    /** The default http scheme proxies will be listening on. */
    public static final String PROXY_DEFAULT_HTTP_SCHEME =
            "com.legstar.eclipse.plugin.jaxwsgen.proxy.DefaultHttpScheme";

    /** The default http host proxies will be listening on. */
    public static final String PROXY_DEFAULT_HTTP_HOST =
            "com.legstar.eclipse.plugin.jaxwsgen.proxy.DefaultHttpHost";

    /** The default http port proxies will be listening on. */
    public static final String PROXY_DEFAULT_HTTP_PORT =
            "com.legstar.eclipse.plugin.jaxwsgen.proxy.DefaultHttpPort";

    /** A template used to build a unique path to a proxy over http. */
    public static final String PROXY_HTTP_PATH_TEMPLATE =
            "com.legstar.eclipse.plugin.jaxwsgen.proxy.HttpPathTemplate";

    /** Utility class. */
    private PreferenceConstants() {

    }

}
