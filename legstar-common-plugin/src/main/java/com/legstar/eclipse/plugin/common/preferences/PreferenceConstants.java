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
package com.legstar.eclipse.plugin.common.preferences;

/**
 * Constant definitions for plug-in preferences.
 */
public class PreferenceConstants {

    /** Folder where ant scripts should be generated. */
    public static final String ANT_SCRIPTS_FOLDER =
        "com.legstar.eclipse.plugin.common.antScriptsFolder";

    /** Mainframe character set. @see java.nio.charset.Charset */
    public static final String HOST_CHARSET =
        "com.legstar.eclipse.plugin.common.hostCharset";

    /** Storage key prefix for history of wsdl URLs. */
    public static final String URL_HISTORY_STORE_KEY_PFX = "com.legstar.eclipse.plugin.common.historyUrl";

    /** Utility class. */
    private PreferenceConstants() {
        
    }
}
