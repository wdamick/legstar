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
package com.legstar.host.server;

import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarRequest;
import com.legstar.pool.manager.ConnectionPool;

import commonj.work.Work;
import commonj.work.WorkListener;

/**
 * Abstract factory shields engine from type of host connection used.
 *
 */
public interface WorkFactory {
	
	/**
	 * Create a new unit of work.
	 * @param request the request being serviced
	 * @param connection the host connection to use
	 * @return a Work element that can be submitted to a work manager
	 */
	Work createWork(LegStarRequest request, LegStarConnection connection);
	
	
	/**
	 * Create a new work unit listener. Such a listener gets notification from
	 * the work manager when the job completed.
	 * 
	 * @param request the request being serviced
	 * @param connection the connection used
	 * @param pool the connection pool into which to recycle the connection
	 *  when work is done
	 * @return a work listener
	 */
	WorkListener createWorkListener(
			LegStarRequest request, LegStarConnection connection,
			ConnectionPool pool);

}
