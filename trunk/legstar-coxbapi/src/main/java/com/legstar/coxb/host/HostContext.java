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
package com.legstar.coxb.host;

/**
 * A class representing a host context, typically the host code page in use
 * for instance.
 * This class is meant to be a generalization for lamguage specific contexts
 * 
 * @author Fady Moussallam
 * 
 */
public class HostContext {

    /** Default host charset is default US EBCDIC charset with Euro support. */
    private static final String DEFAULT_CHARSET_NAME = "IBM01140";

    /** Current host character set. */
    private String mhostCharsetName = getDefaultHostCharsetName();

    /**
     * @return Returns the current charsetName.
     */
    public final String getHostCharsetName() {
        return mhostCharsetName;
    }

    /**
     * This will generate an UnsupportedCharsetException if user tries to
     * specify a charset that is not supported by the current JVM.
     * 
     * @param name
     *            The charsetName to set.
     */
    public final void setHostCharsetName(final String name) {
        // We create a charset to check the validity of the charsetName
        mhostCharsetName = name;
    }

    /**
     * Provides a default host character set.
     * @return the default character set
     */
    public static String getDefaultHostCharsetName() {
        return DEFAULT_CHARSET_NAME;
    }

}
