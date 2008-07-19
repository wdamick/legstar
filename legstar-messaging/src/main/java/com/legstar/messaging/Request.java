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
package com.legstar.messaging;

/** @deprecated */
public class Request extends LegStarRequest {
	
	/** Creates an empty request. */
	public Request() {
		super();
	}
	
	/**
	 * Constructor for a new request.
	 * @param id a unique ID for this request
	 * @param address the host destination
	 * @param requestMessage the input message
	 */
	public Request(
			final String id,
			final LegStarAddress address,
			final LegStarMessage requestMessage) {
		super(id, address, requestMessage);
	}


}
