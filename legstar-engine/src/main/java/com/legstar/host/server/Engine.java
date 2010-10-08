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
package com.legstar.host.server;

import java.util.concurrent.LinkedBlockingQueue;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.pool.manager.ConnectionPool;
import com.legstar.pool.manager.ConnectionPoolException;
import com.legstar.pool.manager.ConnectionPoolManager;

import commonj.work.Work;
import commonj.work.WorkException;
import commonj.work.WorkManager;
import commonj.work.WorkListener;

/**
 * This engine services requests from a blocking queue and dispatches
 * work in a thread pool.
 * 
 */
public class Engine implements Work {

    /** Incoming requests waiting to be serviced. */
    private LinkedBlockingQueue < LegStarRequest > _requests;

    /** Will be true when shutdown is initiated. */
    private boolean _isShuttingDown;

    /** An implementation of a thread pool. */
    private WorkManager _workManager;

    /** Provides work items creation methods. */
    private WorkFactory _workFactory;

    /** Connection pool manager. */
    private ConnectionPoolManager _poolManager;

    /**
     * Maximum time (milliseconds) to wait for a pooled connection to become
     * available.
     */
    private int _takeTimeout;

    /** Logger. */
    private final Log _log = LogFactory.getLog(Engine.class);

    /**
     * Create the Engine for a maximum number of requests waiting to
     * be serviced.
     * 
     * @param maxRequests maximum number of requests waiting to be
     *            serviced. Past this number, the engine will start rejecting
     *            the new requests.
     * @param workManager an implementation of a thread pool
     * @param poolManager a host connections pool manager
     * @param workFactory provides methods to create work items
     * @param takeTimeout maximum time (milliseconds) to wait for a pooled
     *            connection to become
     *            available
     */
    public Engine(final int maxRequests,
            final WorkManager workManager,
            final ConnectionPoolManager poolManager,
            final WorkFactory workFactory,
            final int takeTimeout) {
        _requests = new LinkedBlockingQueue < LegStarRequest >(maxRequests);
        _isShuttingDown = false;
        _workManager = workManager;
        _poolManager = poolManager;
        _workFactory = workFactory;
        _takeTimeout = takeTimeout;
        _log.debug("Created engine instance:" + this);
    }

    /**
     * When the Engine is started in a thread, it will continuously wait
     * for requests until shutdown.
     */
    public void run() {

        while (!_isShuttingDown) {
            _log.debug("Waiting for requests");
            LegStarRequest request;
            try {
                request = _requests.take();
                if (!_isShuttingDown) {
                    scheduleWork(request);
                    _log.debug("Scheduled Request:" + request.getID());
                } else {
                    _log.info("Engine stopped.");
                }
            } catch (InterruptedException e) {
                _log.error("Failed to submit request", e);
            } catch (WorkException e) {
                _log.error("Failed to submit request", e);
            }
        }
    }

    /**
     * Take a connection from a connection pool and schedule work in the
     * thread pool.
     * 
     * @param request the request to be serviced
     * @throws WorkException if scheduling fails
     */
    private void scheduleWork(
            final LegStarRequest request) throws WorkException {
        LegStarConnection connection;
        ConnectionPool pool;
        try {
            /* Get a pool that matches this request criteria */
            pool = _poolManager.getPool(request.getAddress(), true);
            connection = pool.take(_takeTimeout);
        } catch (ConnectionPoolException e) {
            /*
             * Take ownership of request monitor so we can notify waiting
             * threads
             */
            synchronized (request) {
                request.setException(new RequestException(e));
                request.signalProcessingStop();
            }
            throw new WorkException(e);
        }
        Work work = _workFactory.createWork(request, connection);
        WorkListener workListener = _workFactory.createWorkListener(
                request, connection, pool);
        _workManager.schedule(work, workListener);
    }

    /**
     * Place a new request in request queue. Signal to all waiting thread
     * that the request is being processed and is not completed yet.
     * 
     * @param request the request to be added
     * */
    public void addRequest(final LegStarRequest request) {
        request.signalProcessingStart();
        if (!_isShuttingDown) {
            _requests.add(request);
        } else {
            /*
             * In theory we should create an exception for this request
             * and notify the client. In order to keep shutdown simple,
             * we simply log an error. Client will timeout waiting for
             * a reply.
             */
            _log.error("Request received while engine is shutting down.");
        }
    }

    /**
     * If no requests are pending, shutdown the engine by faking a request
     * to get it to check for the mShuttingDown flag.
     * If requests are pending, they are probably blocked waiting for
     * connections to become available.
     * */
    public void shutDown() {
        _isShuttingDown = true;
        _log.info("Attempting to shutdown...");
        if (_requests.size() == 0) {
            /* Empty request to get the engine to process shutdown */
            _requests.add(new LegStarRequest());
        } else {
            _log.warn("Shutdown requested. "
                    + _requests.size() + " requests are pending.");
        }
    }

    /**
     * @return the shutDown status
     */
    public boolean isShuttingDown() {
        return _isShuttingDown;
    }

    /**
     * (non-Javadoc).
     * 
     * @see commonj.work.Work#isDaemon()
     *      TODO need to revisit this. Maybe the engine should be a daemon.
     *      {@inheritDoc}
     */
    public boolean isDaemon() {
        return false;
    }

    /**
     * (non-Javadoc).
     * 
     * @see commonj.work.Work#release()
     */
    public void release() {
    }

}
