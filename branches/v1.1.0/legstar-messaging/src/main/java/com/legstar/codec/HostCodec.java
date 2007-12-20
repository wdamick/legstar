/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.codec;

import java.io.UnsupportedEncodingException;

/**
 * General purpose utility class with coding encoding methods.
 *
 */
public final class HostCodec {
	
	/** Host code page used for header contents. */
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
