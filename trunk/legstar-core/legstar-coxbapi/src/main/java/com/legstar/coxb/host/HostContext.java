/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.host;

import java.io.UnsupportedEncodingException;

/**
 * A class representing a host context, typically the host code page in use for
 * instance. This class is meant to be a generalization for lamguage specific
 * contexts
 * 
 * @author Fady Moussallam
 * 
 */
public class HostContext {

    /** Default host charset is default US EBCDIC charset with Euro support. */
    private static final String DEFAULT_CHARSET_NAME = "IBM01140";

    /**
     * All numeric characters that might appear in a string representation of an
     * integer.
     */
    private static final String INTEGER_SIGNS = "0123456789\0 +-";

    /** Current host character set. */
    private String mhostCharsetName = getDefaultHostCharsetName();

    /**
     * Numeric characters that might appear in a string representation of an
     * integer converted to the target host charset.
     */
    private byte[] hostIntegerSigns;

    public HostContext() {
        hostIntegerSigns = getHostIntegerSigns(getDefaultHostCharsetName());
    }

    /**
     * @return Returns the current charsetName.
     */
    public String getHostCharsetName() {
        return mhostCharsetName;
    }

    /**
     * This will generate an UnsupportedCharsetException if user tries to
     * specify a charset that is not supported by the current JVM.
     * 
     * @param charsetName The charsetName to set.
     */
    public void setHostCharsetName(final String charsetName) {
        if (charsetName == null || charsetName.trim().length() == 0) {
            throw new IllegalArgumentException("Invalid host character set");
        }
        mhostCharsetName = charsetName;
        hostIntegerSigns = getHostIntegerSigns(charsetName);
    }

    /**
     * Provides a default host character set.
     * 
     * @return the default character set
     */
    public static String getDefaultHostCharsetName() {
        return DEFAULT_CHARSET_NAME;
    }

    /**
     * Returns the numeric characters that might appear in a string
     * representation of an integer converted to the target host charset.
     * 
     * @return the numeric characters that might appear in a string
     *         representation of an integer converted to the target host charset
     */
    public byte[] getHostIntegerSigns() {
        return hostIntegerSigns;
    }

    /**
     * @param charsetName the host character set
     * @return the numeric characters that might appear in a string
     *         representation of an integer converted to the target host charset
     */
    public static byte[] getHostIntegerSigns(String charsetName) {
        try {
            return INTEGER_SIGNS.getBytes(charsetName);
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }

    }

    /**
     * @return the characters that might appear in an integer
     */
    public static String getIntegerSigns() {
        return INTEGER_SIGNS;
    }

}
