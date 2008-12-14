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
package com.legstar.util;

/** Test utility class. */
public final class Util {


    /** Cannot be instanciated. */
    private Util() {

    }

    /**
     * Utility method to pretty print a byte array content.
     * @param hostBytes byte array to print
     * @return a pretty string
     */
    public static String toHexString(final byte[] hostBytes) {

        if (hostBytes == null) {
            return null;
        }

        StringBuffer hexString = new StringBuffer("");
        for (int i = 0; i < hostBytes.length; i++) {
            hexString.append(
                    Integer.toHexString(
                            hostBytes[i] & 0xFF | 0x100).substring(1, 3));
        }

        return hexString.toString();
    }

    /**
     * Another pretty printing method but with a limit to the number of
     * bytes printed. This method will print an even number of starting
     * and ending bytes.
     * @param hostBytes byte array to print
     * @param maxBytes maximum number of bytes to process
     * @return a pretty string
     */
    public static String toHexString(
            final byte[] hostBytes, final int maxBytes) {

        if (hostBytes == null) {
            return null;
        }

        if (hostBytes.length <= maxBytes) {
            return toHexString(hostBytes);
        }

        int gap = maxBytes / 2;
        StringBuffer hexString = new StringBuffer("");
        for (int i = 0; i < gap; i++) {
            hexString.append(
                    Integer.toHexString(
                            hostBytes[i] & 0xFF | 0x100).substring(1, 3));
        }
        hexString.append("....");
        for (int i = (hostBytes.length - gap); i < hostBytes.length; i++) {
            hexString.append(
                    Integer.toHexString(
                            hostBytes[i] & 0xFF | 0x100).substring(1, 3));
        }

        return hexString.toString();
    }

    /**
     * Takes a string of characters representing hex data and converts it
     * to a byte array.
     * @param string the hex string
     * @return the byte array
     */
    public static byte[] toByteArray(final String string) {
        if (string == null) {
            return new byte[0];
        }
        byte[] hostBytes = new byte[string.length() / 2];
        for (int i = 0; i < string.length(); i += 2) {
            hostBytes[i / 2] = 
                (byte) Integer.parseInt(string.substring(i, i + 2), 16);
        }
        return hostBytes;
    }

}
