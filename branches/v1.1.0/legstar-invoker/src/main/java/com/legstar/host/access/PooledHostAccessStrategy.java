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
package com.legstar.host.access;

import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.host.server.EngineHolder;
import com.legstar.host.server.EngineNotStartedException;
import com.legstar.messaging.Request;

/**
 * This class implements a host accessor over a pool of host connections
 * managed by a central engine.
 */
public class PooledHostAccessStrategy implements HostAccessStrategy {

	/** Logger. */
	private static final Log LOG =
		LogFactory.getLog(PooledHostAccessStrategy.class);
	
	/** Time out (in milliseconds) for invoke. */
	private static final long DEFAULT_INVOKE_TIMEOUT_MSEC = 3000L;
	
	/** Relative XPath location within enpoint for invoke timeout. */
	private static final String INVOKE_TIMEOUT_CFG = "pooledInvokeTimeout";
	
	/** Maximum time this invoker will wait for a reply.  */
	private long mInvokeTimeout;
	
	/**
	 * Construct a pool access strategy from a configuration sub-hierarchy.
	 * @param endpointConfig an XML configuration sub-hierarchy for an endpoint
	 */
	public PooledHostAccessStrategy(
			final HierarchicalConfiguration endpointConfig) {
		mInvokeTimeout = endpointConfig.getLong(
				INVOKE_TIMEOUT_CFG, DEFAULT_INVOKE_TIMEOUT_MSEC);
	}
	
	/**
	 * An engine, servicing pool of host connections, is assumed to be running
	 * in this VM. Rather than invoking the host directly, we submit a request
	 * asynchronously to the engine and then wait for a reply.
	 * 
	 * @see com.legstar.host.access.HostAccessStrategy#invoke(
	 * com.legstar.messaging.Request)
	 * {@inheritDoc}
	 */
	public final void invoke(
			final Request request) throws HostAccessStrategyException {
		
		long startTime = System.currentTimeMillis();
		if (LOG.isDebugEnabled()) {
			LOG.debug("Pooled invoke for Request:" + request.getID());
		}
		synchronized (request) {
			try {
				EngineHolder.getEngine().addRequest(request);
				request.wait(mInvokeTimeout);
				if (request.getException() != null) {
					throw new HostAccessStrategyException(
							request.getException());
				} else {
					if (request.getResponseMessage() == null) {
						throw new HostAccessStrategyException(
								"Timed out waiting for a response for Request:"
								+ request.getID());
					}
				}
			} catch (InterruptedException e) {
				throw new HostAccessStrategyException(e);
			} catch (EngineNotStartedException e) {
				throw new HostAccessStrategyException(e);
			}
		}
		if (LOG.isDebugEnabled()) {
			long endTime = System.currentTimeMillis();
			LOG.debug("Pooled invoke for Request:" + request.getID()
					+ " ended. elapse: "
					+ Long.toString(endTime - startTime) + " ms");
		}
	}

	/**
	 * @return the time this invoker will wait for a reply
	 */
	public final long getInvokeTimeout() {
		return mInvokeTimeout;
	}

	/**
	 * @param invokeTimeout the time this invoker will wait for a reply
	 */
	public final void setInvokeTimeout(final long invokeTimeout) {
		mInvokeTimeout = invokeTimeout;
	}

}
