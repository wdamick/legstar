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
package com.legstar.eclipse.plugin.schemagen.preferences;

/**
 * Constant definitions for plug-in preferences.
 */
public final class PreferenceConstants {

    /** A prefix to be used for new XML namespaces. */
    public static final String XSD_NAMESPACE_PREFIX =
        "com.legstar.eclipse.plugin.schemagen.xsdNamespacePrefix";

    /** Prefix to be used for new jaxb package names. */
    public static final String JAXB_PACKAGE_NAME_PREFIX =
        "com.legstar.eclipse.plugin.schemagen.jaxbPackageNamePrefix";

    /** The last URL that was queried. */
    public static final String LAST_URL_KEY = "lastUrl";
    
    /** Utility class.*/
    private PreferenceConstants() {
        
    }
    
}
