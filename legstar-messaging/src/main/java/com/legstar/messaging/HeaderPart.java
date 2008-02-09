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
package com.legstar.messaging;

import java.util.Map;

/** @deprecated */
public class HeaderPart extends LegStarHeaderPart {

	/** Serial version ID. */
	private static final long serialVersionUID = -5049074592429902791L;

	/**
	 * Default constructor.
	 * @throws HeaderPartException if header part cannot be constructed
	 */
	public HeaderPart() throws HeaderPartException {
		super();
	}
	
	/** Creates a header part for a given number of input parts and a
	 * JSON string describing the expected host action.
	 * and an empty json host action description. 
	 * @param dataPartsNumber the number of data message parts
	 * @param jsonString the host action description
	 * @throws HeaderPartException if header code page is not supported
	 *  */
	public HeaderPart(
			final int dataPartsNumber, final String jsonString)
			throws HeaderPartException {
		super(dataPartsNumber, jsonString);
	}
	
	/**
	 * Convenience constructor takes the host action description as
	 * key/value pairs rather than a JSON string.
	 * 
	 * @param dataPartsNumber the number of data message parts
	 * @param keyValues the protocol elements
	 * @throws HeaderPartException if character set is invalid
	 */
	public HeaderPart(
			final Map < String, Object > keyValues,
			final int dataPartsNumber) throws HeaderPartException {
		super(keyValues, dataPartsNumber);
	}


}
