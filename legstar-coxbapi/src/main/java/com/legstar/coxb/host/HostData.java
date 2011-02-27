/*******************************************************************************
 * Copyright (c) 2010 LegSem.
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
 * Represents a generic host data content. Mainly a wrapper over a byte array.
 * 
 * @author Fady Moussallam
 * 
 */
public class HostData {

    /** Used for bytes to hex conversions. */
    private static final char[] HEX_CHARS = "0123456789abcdef".toCharArray();

    /** Internal buffer holding host data. */
    private final byte[] mhostData;

    /**
     * Contructor from existing host data.
     * 
     * @param hostData byte array representing host field content
     */
    public HostData(final byte[] hostData) {
        mhostData = hostData;
    }

    /**
     * Contructor from a hexadecimal String.
     * 
     * @param hexString A string of hex representations
     */
    public HostData(final String hexString) {
        mhostData = toByteArray(hexString);
    }

    /**
     * Helper method to dump field content in hexadecimal.
     * 
     * @return a string with hexadecimal representation of the field content
     */
    public String toHexString() {
        return toHexString(mhostData);
    }

    /**
     * Helper method to dump field content in hexadecimal.
     * 
     * @param hostBytes a byte array to get hexadecimal representation for
     * @return a string with hexadecimal representation of the field content
     */
    public static String toHexString(final byte[] hostBytes) {
        if (hostBytes == null) {
            return null;
        }
        return toHexString(hostBytes, 0, hostBytes.length);
    }

    /**
     * Helper method to dump field content in hexadecimal.
     * 
     * @param hostBytes a byte array to get hexadecimal representation for
     * @param start 0-based position of first byte to dump
     * @param length number of bytes to dump
     * @return a string with hexadecimal representation of the field content
     */
    public static String toHexString(
            final byte[] hostBytes, final int start, final int length) {

        if (hostBytes == null) {
            return null;
        }
        if (start + length > hostBytes.length) {
            throw new IllegalArgumentException(
                    "Invalid start or length parameter");
        }

        char[] chars = new char[2 * length];
        for (int i = 0; i < length; ++i) {
            chars[2 * i] = HEX_CHARS[(hostBytes[i + start] & 0xF0) >>> 4];
            chars[2 * i + 1] = HEX_CHARS[hostBytes[i + start] & 0x0F];
        }
        return new String(chars);
    }

    /**
     * Another pretty printing method but with a limit to the number of
     * bytes printed. This method will print an even number of starting
     * and ending bytes leaving a gap between them.
     * 
     * @param hostBytes byte array to print
     * @param maxBytes maximum number of bytes to process
     * @return a pretty string
     */
    public static String toHexString(
            final byte[] hostBytes, final int maxBytes) {
        if (hostBytes == null) {
            return null;
        }
        return toHexString(hostBytes, 0, hostBytes.length, maxBytes);
    }

    /**
     * Another pretty printing method but with a limit to the number of
     * bytes printed. This method will print an even number of starting
     * and ending bytes leaving a gap between them.
     * 
     * @param hostBytes byte array to print
     * @param start 0-based position of first byte to dump
     * @param length total number of bytes to dump
     * @param maxBytes maximum number of bytes to process
     * @return a pretty string
     */
    public static String toHexString(
            final byte[] hostBytes, final int start, final int length,
            final int maxBytes) {

        if (hostBytes == null) {
            return null;
        }

        if (start + length > hostBytes.length) {
            throw new IllegalArgumentException(
                    "Invalid start or length parameter");
        }

        if (length <= maxBytes) {
            return toHexString(hostBytes, start, length);
        }

        if (maxBytes < 2) {
            throw new IllegalArgumentException(
                    "maxBytes cannot be smaller than 2");
        }

        int gap = maxBytes / 2;
        StringBuffer hexString = new StringBuffer("");
        for (int i = start; i < start + gap; i++) {
            hexString.append(
                    Integer.toHexString(
                            hostBytes[i] & 0xFF | 0x100).substring(1, 3));
        }
        hexString.append("....");
        for (int i = (start + length - gap); i < start + length; i++) {
            hexString.append(
                    Integer.toHexString(
                            hostBytes[i] & 0xFF | 0x100).substring(1, 3));
        }

        return hexString.toString();
    }

    /**
     * Helper method to populate a byte array from a hex string representation.
     * 
     * @param string a string of hexadecimal characters to be turned
     *            into a byte array
     * @return an initialized byte array
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

    /**
     * @return Returns the hostData.
     */
    public byte[] getHostData() {
        return mhostData;
    }

    /**
     * @return Returns the hostData length.
     */
    public int length() {
        return mhostData.length;
    }

}
