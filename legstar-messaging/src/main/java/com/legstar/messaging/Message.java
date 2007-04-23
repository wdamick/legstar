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

/**
 * Messages represents the input and output of requests. A message is composed
 * of message parts: one header part and any number of data message parts.
 */
public class Message {
	
	/** Header message part. */
	private HeaderPart mHeaderPart;
	
	/** Data message parts. */
	private List < MessagePart > mDataParts;

	/**
	 * Construct a message from its message parts.
	 * @param headerPart the header message part
	 * @param dataParts the data message parts
	 */
	public Message(
			final HeaderPart headerPart,
			final List < MessagePart > dataParts) {
		mHeaderPart = headerPart;
		mDataParts = dataParts;
	}

	/**
	 * @return the list of data message parts
	 */
	public final List < MessagePart > getDataParts() {
		return mDataParts;
	}
	
	/**
	 * @param dataParts the list of data message parts to set
	 */
	public final void setDataParts(final List < MessagePart > dataParts) {
		mDataParts = dataParts;
	}

	/**
	 * @return the header message part
	 */
	public final HeaderPart getHeaderPart() {
		return mHeaderPart;
	}

	/**
	 * @param headerPart the header message part to set
	 */
	public final void setHeaderPart(final HeaderPart headerPart) {
		mHeaderPart = headerPart;
	}

}
