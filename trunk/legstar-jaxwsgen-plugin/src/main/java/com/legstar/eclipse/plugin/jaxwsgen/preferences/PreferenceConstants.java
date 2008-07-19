/*******************************************************************************
 * Copyright (c) 2008 LegSem.
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
public class PreferenceConstants {

    /** J2ee folder where war files should be deployed. */
    public static final String J2EE_WAR_FOLDER =
        "com.legstar.eclipse.plugin.jaxwsgen.j2eeWarFolder";

    /** J2ee folder where web deployment files should be generated. */
    public static final String J2EE_WDD_FOLDER =
        "com.legstar.eclipse.plugin.jaxwsgen.j2eeWDDFolder";

    /** Generated Web services target namespace prefix. */
    public static final String WS_TARGET_NAMESPACE_PREFIX =
        "com.legstar.eclipse.plugin.jaxwsgen.wsTargetNamespacePrefix";

    /** A template to be used for URIs exposed by generated proxy services. */
    public static final String PROXY_URI_TEMPLATE =
        "com.legstar.eclipse.plugin.jaxwsgen.proxyURITemplate";

    /** Folder where sample COBOL programs are generated. */
    public static final String COBOL_SAMPLE_FOLDER =
        "com.legstar.eclipse.plugin.jaxwsgen.cobolSampleFolder";

}
