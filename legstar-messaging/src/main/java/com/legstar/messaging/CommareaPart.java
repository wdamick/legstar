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
 * MessagePart representing the content of a CICS commarea.
 */
public class CommareaPart extends LegStarMessagePart {

	/** Serial version ID. */
	private static final long serialVersionUID = -6186410695573533903L;
	
	/** The header identifier (used as an eye catcher by the CICS
	 *  counterpart).*/
	public static final String COMMAREA_PART_ID = "LSOKCOMMAREA";
	
	/**
	 * Create a commarea from a binary content.
	 * @param content binary content
	 */
	public CommareaPart(final byte[] content) {
		super(COMMAREA_PART_ID, content);
	}

}
