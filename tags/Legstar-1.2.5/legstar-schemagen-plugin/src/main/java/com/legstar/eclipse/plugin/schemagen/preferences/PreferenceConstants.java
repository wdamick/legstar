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

    /** Last eclipse container used. */
    public static final String LAST_TARGET_CONTAINER =
        "com.legstar.eclipse.plugin.schemagen.last.targetcontainer";

    /** Last target XSD file name used. */
    public static final String LAST_TARGET_XSD_FILE_NAME =
        "com.legstar.eclipse.plugin.schemagen.last.targetxsdfilename";

    /** Last JAXB type suffix used. */
    public static final String LAST_JAXB_TYPE_SUFFIX =
        "com.legstar.eclipse.plugin.schemagen.last.jaxbtypesuffix";

    /** Utility class.*/
    private PreferenceConstants() {
        
    }
    
}
