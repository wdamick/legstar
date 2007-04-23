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

/**
 * MessageParts are generic named, binary containers used to send
 * and receive from host.
 */
public class MessagePart {
	
	/** The message part identifier. */
	private String mID;
	
	/** The message content. */
	private byte[] mContent;
	
	/**
	 * Create a named message part from a content.
	 * @param id the message part identifier
	 * @param content a binary content
	 */
	public MessagePart(final String id, final byte[] content) {
		mID = id;
		mContent = content;
	}

	/**
	 * @return the message content
	 */
	public final byte[] getContent() {
		return mContent;
	}

	/**
	 * @param content the message content to set
	 */
	public final void setContent(final byte[] content) {
		mContent = content;
	}

	/**
	 * @return the message part identifier
	 */
	public final String getID() {
		return mID;
	}

	/**
	 * @param id the message part identifier to set
	 */
	public final void setID(final String id) {
		mID = id;
	}

}
