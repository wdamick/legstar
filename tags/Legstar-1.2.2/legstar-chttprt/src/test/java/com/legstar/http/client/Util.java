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
package com.legstar.http.client;

public class Util {
	
	public static final String CONFIG_FILE = "config.xml";
	/** Utility method to pretty pring a byte array content. */
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

}
