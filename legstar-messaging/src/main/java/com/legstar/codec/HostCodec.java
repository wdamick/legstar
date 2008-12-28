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
package com.legstar.codec;

import java.io.UnsupportedEncodingException;

/**
 * General purpose utility class with coding encoding methods.
 *
 */
public final class HostCodec {

    /** Host code page used for header contents. EBCDIC Latin 1/Open Systems. */
    public static final String HEADER_CODE_PAGE = "IBM1047";

    /** Private constructor for utility class Codec. */
    private HostCodec() {

    }
    /**
     * Given a host charset, this method converts a string into host bytes and
     * either truncates or fills with spaces so that the result fits in the
     * requested hostLength.
     * 
     * @param clientString the string in unicode
     * @param hostBytes the host buffer to fill
     * @param startPos first byte to fill in hostBytes
     * @param hostLength total bytes to fill in hostBytes
     * @param hostCharset host character set
     * @throws UnsupportedEncodingException  if conversion fails
     */
    public static void toHostBytes(
            final String clientString,
            final byte[] hostBytes,
            final int startPos,
            final int hostLength,
            final String hostCharset) throws UnsupportedEncodingException {
        byte[] clientBytes;
        byte[] spaceBytes;
        clientBytes = clientString.getBytes(hostCharset);
        spaceBytes = " ".getBytes(hostCharset);
        int j = 0;
        for (int i = startPos; i < startPos + hostLength; i++) {
            if (j < clientBytes.length) {
                hostBytes[i] = clientBytes[j];
                j++;
            } else {
                hostBytes[i] = spaceBytes[0];
            }
        }
    }

}
