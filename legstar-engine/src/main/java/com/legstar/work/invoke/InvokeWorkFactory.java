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
package com.legstar.work.invoke;

import com.legstar.host.server.WorkFactory;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarRequest;
import com.legstar.pool.manager.ConnectionPool;
import commonj.work.Work;
import commonj.work.WorkListener;

/**
 * Concrete factory to create units of work and listeners.
  */
public class InvokeWorkFactory implements WorkFactory {

	/** (non-Javadoc).
	 * @see com.legstar.host.server.WorkFactory#createWork(
	 * com.legstar.messaging.Request, com.legstar.messaging.Connection)
	 * {@inheritDoc}
	 */
	public final Work createWork(
			final LegStarRequest request, final LegStarConnection connection) {
		return new InvokeWork(request, connection);
	}

	/** (non-Javadoc).
	 * @see com.legstar.host.server.WorkFactory#createWorkListener(
	 * com.legstar.messaging.Request, com.legstar.messaging.Connection,
	 *  com.legstar.pool.manager.ConnectionPoolInterface)
	 * {@inheritDoc}
	 */
	public final WorkListener createWorkListener(
			final LegStarRequest request,
			final LegStarConnection connection,
			final ConnectionPool pool) {
		return new InvokeWorkListener(request, connection, pool);
	}

}
