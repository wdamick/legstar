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
package com.legstar.coxb.host;

/**
 * Represents a generic host data content. Mainly a wrapper over a byte array.
 * 
 * @author Fady Moussallam
 * 
 */
public class HostData {

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
	public final String toHexString() {
		return toHexString(mhostData);
	}

	/**
	 * Helper method to dump field content in hexadecimal.
	 * 
	 * @param hostBytes a byte array to get hexadecimal representation for
	 * @return a string with hexadecimal representation of the field content
	 */
	public static String toHexString(final byte[] hostBytes) {
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
		assert (start + length < hostBytes.length + 1);
		
		StringBuilder hexString = new StringBuilder("");
		for (int i = start; i < length; i++) {
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
	 *  into a byte array
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
	public final byte[] getHostData() {
		return mhostData;
	}

	/**
	 * @return Returns the hostData length.
	 */
	public final int length() {
		return mhostData.length;
	}
	
}
