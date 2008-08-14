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
package com.legstar.host.access;

import com.legstar.messaging.LegStarRequest;

/**
 * An abstract host access strategy providing capability to execute a request.
 */
public interface HostAccessStrategy {
	
	/**
	 * Synchronous execution for a given request.
	 * @param request the request details
	 * @throws HostAccessStrategyException if execution fails
	 */
	void invoke(LegStarRequest request) throws HostAccessStrategyException;

}
