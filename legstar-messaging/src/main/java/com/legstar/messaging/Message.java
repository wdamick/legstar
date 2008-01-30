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

import java.util.List;

/** @deprecated */
public class Message extends LegStarMessage {
	
	/**
	 * Creates an empty message.
	 * @throws HeaderPartException if host encoding is wrong
	 */
	public Message() throws HeaderPartException {
		super();
	}
	
	/**
	 * Construct a message from its message parts.
	 * @param headerPart the header message part
	 * @param dataParts the data message parts
	 */
	public Message(
			final LegStarHeaderPart headerPart,
			final List < LegStarMessagePart > dataParts) {
		super(headerPart, dataParts);
	}

}
