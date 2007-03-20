/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.host;

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
