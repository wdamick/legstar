/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.pool.manager;

import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.RequestException;

/**
 * A simplistic sliding window that allow us keep track of opened connections in a given timespan.
 * <p/>
 * The windows slides when a new connection is added. When it slides, connections that are obsolete
 * are closed and removed from the sliding window.
 *
 */
public class SlidingWindowKeepAlivePolicy {

    /** Sliding window time span in milliseconds. */
    private long _timespan;

    /** Queue of available host connections. */
    private BlockingStack < LegStarConnection > _connections;
    
    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Construct a sliding window.
     * @param connections the connections queue
     * @param timespan time span in milliseconds
     */
    public SlidingWindowKeepAlivePolicy(
            final BlockingStack < LegStarConnection > connections, final long timespan) {
        _timespan = timespan;
        _connections = connections;
        if (timespan > 0) {
            _log.info("Connections will be closed if opened for more than " + timespan + " ms.");
        } else {
            _log.info("No connections monitoring.");
        }
    }

    /**
     * Close connections which have been opened for too long.
     * This is a no op if the timespan is negative. Observe that a timespan of zero is
     * equivalent with no reusing.
     * @throws ConnectionPoolException if obsolete connections cannot be closed
     */
    public synchronized void closeObsoleteConnections() throws ConnectionPoolException {
        if (_timespan < 0) {
            return;
        }
        long now = System.currentTimeMillis();
        try {
            Iterator < LegStarConnection > iter = _connections.iterator();
            while (iter.hasNext()) {
                LegStarConnection connection = iter.next();
                if (connection.getLastUsedTime() > 0) {
                    if ((now - connection.getLastUsedTime()) > _timespan) {
                        if (_log.isDebugEnabled()) {
                            _log.debug("Closing obsolete Connection:"
                                    + connection.getConnectionID()
                                    + ". Has been opened for: " + (now - connection.getLastUsedTime())
                                    + " ms");
                        }
                        /* Iterator does not reflect the content of the stack over time */
                        if (_connections.contains(connection)) {
                            connection.close();
                        }
                    }
                }
            }
        } catch (RequestException e) {
            throw new ConnectionPoolException(e);
        }
    }

    /**
     * @return sliding window time span in milliseconds
     */
    public long getTimespan() {
        return _timespan;
    }

    /**
     * @return the connections queue
     */
    public BlockingStack < LegStarConnection > getConnections() {
        return _connections;
    }

}
