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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.ConnectionFactory;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;

/**
 * This transport-independant accessor dynamically loads a connection
 * factory and issues a direct synchronous call to a host.
 */
public class DirectHostAccessStrategy implements HostAccessStrategy {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());
    
    /** The target host endpoint. */
    private HostEndpoint _hostEndpoint;

    /**
     * Construct a direct host accessor for an endpoint.
     * @param hostEndpoint the target host endpoint
     */
    public DirectHostAccessStrategy(
            final HostEndpoint hostEndpoint) {
        _hostEndpoint = hostEndpoint;
    }

    /** (non-Javadoc).
     * @see com.legstar.host.access.HostAccessStrategy#invoke(
     * com.legstar.messaging.Request)
     * {@inheritDoc}
     */
    public void invoke(
            final LegStarRequest request) throws HostAccessStrategyException {

        long startTime = System.currentTimeMillis();
        if (_log.isDebugEnabled()) {
            _log.debug("Direct invoke for Request:" + request.getID());
        }
        try {
            ConnectionFactory factory = getHostEndpoint().getHostConnectionfactory();
            LegStarConnection connection = factory.createConnection(
                    request.getID(), request.getAddress(), getHostEndpoint());
            connection.connect(request.getAddress().getHostPassword());
            connection.sendRequest(request);
            connection.recvResponse(request);
            connection.close();
        } catch (ConnectionException e) {
            request.setException(new RequestException(e));
            throw new HostAccessStrategyException(e);
        } catch (RequestException e) {
            request.setException(new RequestException(e));
            throw new HostAccessStrategyException(e);
        }
        if (_log.isDebugEnabled()) {
            long endTime = System.currentTimeMillis();
            _log.debug("Direct invoke for Request:" + request.getID()
                    + " ended. elapse: "
                    + Long.toString(endTime - startTime) + " ms");
        }
    }

    /**
     * @return the target host endpoint
     */
    public HostEndpoint getHostEndpoint() {
        return _hostEndpoint;
    }

}
