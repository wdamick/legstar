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

import com.legstar.messaging.Connection;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.Request;
import com.legstar.messaging.RequestException;

import commonj.work.Work;

/**
 * This is a unit of work. It can be dispatched on its own thread.
 * It will obtain a connection to the host and then use it to
 * execute a remote program as described in a Request object.
 *
 */
public class InvokeWork implements Work {

	/** Logger. */
	private static final Log LOG = LogFactory.getLog(InvokeWork.class);
	
	/** A connection object to the host. */
	private Connection mConnection;
	
	/** The request to be serviced by this unit of work. */
	private Request mRequest;
	
	/**
	 * Constructor from a given request and a given connection.
	 * 
	 * @param request request to be serviced by this unit of work
	 * @param connection a connection to the host
	 */
	public InvokeWork(
			final Request request,
			final Connection connection) {
		mRequest = request;
		mConnection = connection;
	}
	
	/** (non-Javadoc).
	 * @see commonj.work.Work#isDaemon()
	 * {@inheritDoc}
	 */
	public final boolean isDaemon() {
		return false;
	}

	/** (non-Javadoc).
	 * @see commonj.work.Work#release()
	 */
	public void release() {
	}

	/** 
	 * When submitted asynchronously, this work unit will connect to
	 * the host, reusing an existing connection if possible, emit a
	 * request and then wait for a reply. Upon return, the Request object
	 * will hold the response, or exception if something went wrong.
	 */
	public final void run() {
		long startTime = System.currentTimeMillis();
		LOG.debug("Unit of Work started for Request:" + mRequest.getID());
		try {
			LOG.debug("Connecting to host for Request:" + mRequest.getID()
				    + " on Connection:" + mConnection.getConnectionID());
			mConnection.connectReuse(mRequest.getAddress().getHostPassword());
			mConnection.sendRequest(mRequest);
			mConnection.recvResponse(mRequest);
			mConnection.commitUOW();
		} catch (ConnectionException e) {
			mRequest.setException(e);
			throw new RuntimeException(e);
		} catch (RequestException e) {
			mRequest.setException(e);
			throw new RuntimeException(e);
		}
		long endTime = System.currentTimeMillis();
		LOG.debug("Request:" + mRequest.getID()
			    + " on Connection:" + mConnection.getConnectionID()
			    + " serviced in " + (endTime - startTime) + " msecs");
	}

	/**
	 * @return the host connection
	 */
	public final Connection getConnection() {
		return mConnection;
	}

	/**
	 * @param connection the host connection to set
	 */
	public final void setConnection(final Connection connection) {
		mConnection = connection;
	}

	/**
	 * @return the request object
	 */
	public final Request getRequest() {
		return mRequest;
	}

	/**
	 * @param request the request object to set
	 */
	public final void setRequest(final Request request) {
		mRequest = request;
	}
	
}
