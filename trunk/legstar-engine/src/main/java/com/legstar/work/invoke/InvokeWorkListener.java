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

import org.apache.commons.logging.Log; 
import org.apache.commons.logging.LogFactory; 
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarRequest;
import com.legstar.pool.manager.ConnectionPool;
import com.legstar.pool.manager.ConnectionPoolException;

import commonj.work.WorkEvent;
import commonj.work.WorkListener;

/**
 * This class gets notifications from the work manager during a
 * unit of work lifecycle. It's responsibility is to notify clients
 * of a request outcome as well as host connections recycling.
  */
public class InvokeWorkListener implements WorkListener {
	
	/** Connection pool for connection recycling. */
	private ConnectionPool mConnectionPool;

	/** The connection used to service the request. */
	private LegStarConnection mConnection;
	
	/** The request being serviced. */
	private LegStarRequest mRequest;
	
	/** Logger. */
	private static final Log LOG =
		LogFactory.getLog(InvokeWorkListener.class);
	
	/**
	 * Constructor for a given connection.
	 * @param request the request being serviced
	 * @param connection connection used to service the request
	 * @param connectionPool the connection pool
	 */
	public InvokeWorkListener(
			final LegStarRequest request,
			final LegStarConnection connection,
			final ConnectionPool connectionPool) {
		mConnectionPool = connectionPool;
		mConnection = connection;
		mRequest = request;
	}

	/**
	 * The work manager has accepted a request.
	 * @param workEvent the work event
	 * */
	public final void workAccepted(final WorkEvent workEvent) {
		LOG.debug("Work accepted for Request:" + mRequest.getID()
			    + " on Connection:" + mConnection.getConnectionID());
	}

	/**
	 * A request has been serviced. Recycle connection and notify clients.
	 * @param workEvent the work event
	 */
	public final void workCompleted(final WorkEvent workEvent) {

		/* Notify requestor if connection cannot be recycled */
		try {
			mConnectionPool.put(mConnection);
		} catch (ConnectionPoolException e) {
			mRequest.setException(e);
		}
		if (workEvent.getException() != null) {
			LOG.debug("Work failed for Request:" + mRequest.getID()
					    + " on Connection:" + mConnection.getConnectionID(),
					    workEvent.getException());
		} else {
			LOG.debug("Work completed for Request:" + mRequest.getID()
					    + " on Connection:" + mConnection.getConnectionID());
		}
		/* Take ownership of request monitor so we can notify waiting threads */
		synchronized (mRequest) {
			mRequest.notifyAll();
		}
	}
	
	/**
	 * Manager rejected that work. Notify clients.
	 * @param workEvent the work event
	 *  */
	public final void workRejected(final WorkEvent workEvent) {
		/* Notify requestor if connection cannot be recycled */
		try {
			mConnectionPool.put(mConnection);
		} catch (ConnectionPoolException e) {
			mRequest.setException(e);
		}
		/* Take ownership of request monitor so we can notify waiting threads */
		synchronized (mRequest) {
			mRequest.notifyAll();
			LOG.debug("Work rejected for Request:" + mRequest.getID()
				    + " on Connection:" + mConnection.getConnectionID());
		}
	}

	/**
	 * Work unit has started.
	 * @param workEvent the work event
	 *  */
	public final void workStarted(final WorkEvent workEvent) {
		LOG.debug("Work started for Request:" + mRequest.getID()
			    + " on Connection:" + mConnection.getConnectionID());
	}
		
}
