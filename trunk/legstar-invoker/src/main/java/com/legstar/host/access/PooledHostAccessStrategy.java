/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.host.access;

import java.util.concurrent.TimeUnit;

import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.host.server.EngineHolder;
import com.legstar.host.server.EngineNotStartedException;
import com.legstar.messaging.LegStarRequest;

/**
 * This class implements a host accessor over a pool of host connections
 * managed by a central engine.
 */
public class PooledHostAccessStrategy implements HostAccessStrategy {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

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
            final LegStarRequest request) throws HostAccessStrategyException {

        long startTime = System.currentTimeMillis();
        if (_log.isDebugEnabled()) {
            _log.debug("Pooled invoke for Request:" + request.getID());
        }
        synchronized (request) {
            try {
                EngineHolder.getEngine().addRequest(request);
                request.await(mInvokeTimeout, TimeUnit.MILLISECONDS);
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
        if (_log.isDebugEnabled()) {
            long endTime = System.currentTimeMillis();
            _log.debug("Pooled invoke for Request:" + request.getID()
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
