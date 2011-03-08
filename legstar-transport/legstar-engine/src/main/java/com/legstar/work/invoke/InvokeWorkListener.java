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
    private final Log _log = LogFactory.getLog(getClass());

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
    public void workAccepted(final WorkEvent workEvent) {
        if (_log.isDebugEnabled()) {
            _log.debug("Work accepted for Request:" + mRequest.getID()
                    + " on Connection:" + mConnection.getConnectionID());
        }
    }

    /**
     * A request has been serviced. Recycle connection and notify clients.
     * @param workEvent the work event
     */
    public void workCompleted(final WorkEvent workEvent) {

        /* Take ownership of request monitor so we can notify waiting threads */
        synchronized (mRequest) {
            /* Notify requestor if connection cannot be recycled */
            try {
                mConnectionPool.put(mConnection);
            } catch (ConnectionPoolException e) {
                mRequest.setException(e);
            }
            if (_log.isDebugEnabled()) {
                if (workEvent.getException() != null) {
                    _log.debug("Work failed for Request:" + mRequest.getID()
                            + " on Connection:" + mConnection.getConnectionID(),
                            workEvent.getException());
                } else {
                    _log.debug("Work completed for Request:" + mRequest.getID()
                            + " on Connection:" + mConnection.getConnectionID());
                }
            }
            mRequest.signalProcessingStop();
        }
    }

    /**
     * Manager rejected that work. Notify clients.
     * @param workEvent the work event
     *  */
    public void workRejected(final WorkEvent workEvent) {
        /* Take ownership of request monitor so we can notify waiting threads */
        synchronized (mRequest) {
            /* Notify requestor if connection cannot be recycled */
            try {
                mConnectionPool.put(mConnection);
            } catch (ConnectionPoolException e) {
                mRequest.setException(e);
            }
            if (_log.isDebugEnabled()) {
                _log.debug("Work rejected for Request:" + mRequest.getID()
                        + " on Connection:" + mConnection.getConnectionID());
            }
            mRequest.signalProcessingStop();
        }
    }

    /**
     * Work unit has started.
     * @param workEvent the work event
     *  */
    public void workStarted(final WorkEvent workEvent) {
        if (_log.isDebugEnabled()) {
            _log.debug("Work started for Request:" + mRequest.getID()
                    + " on Connection:" + mConnection.getConnectionID());
        }
    }

}
