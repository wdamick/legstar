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
package com.legstar.host;

/**
 * Represents a generic host data content. Mainly a wrapper over a byte array.
 * 
 * @author Fady Moussallam
 * 
 */
public class HostData {

	/** Internal buffer holding host data. */
	private byte[] mhostData;
    
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
	 * Helper method to populate a byte array from a hex string representation.
	 * 
	 * @param string a string of hexadecimal characters to be turned
	 *  into a byte array
	 * @return an initialized byte array
	 */
	public static byte[] toByteArray(final String string) {
		if (string == null) {
			return null;
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
	 * @param data The hostData to set.
	 */
	public final void setHostData(final byte[] data) {
		mhostData = data;
	}

	/**
	 * @return Returns the hostData length.
	 */
	public final int length() {
		return mhostData.length;
	}
	
}
