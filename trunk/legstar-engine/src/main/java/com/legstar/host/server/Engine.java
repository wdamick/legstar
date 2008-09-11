/*******************************************************************************
 * Copyright (c) 2008 LegSem.
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
	private LinkedBlockingQueue < LegStarRequest > mRequests;
	
	/** Will be true when shutdown is initiated. */
	private boolean mShuttingDown;
	
	/** An implementation of a thread pool. */
	private WorkManager mWorkManager;
	
	/** Provides work items creation methods. */
	private WorkFactory mWorkFactory;
	
	/** Connection pool manager. */
	private ConnectionPoolManager mPoolManager;
	
	/** Time out (in milliseconds) connection pool takes. */
	private static final int CONNECT_TAKE_MSEC = 1000;
	
	/** Logger. */
	private static final Log LOG = LogFactory.getLog(Engine.class);
	
	/**
	 * Create the Engine for a maximum number of requests waiting to
	 * be serviced.
	 * @param maxRequests maximum number of requests waiting to be
	 * serviced. Past this number, the engine will start rejecting
	 * the new requests.
	 * @param workManager an implementation of a thread pool
	 * @param poolManager a host connections pool manager
	 * @param workFactory provides methods to create work items
	 */
	public Engine(final int maxRequests,
			final WorkManager workManager,
			final ConnectionPoolManager poolManager,
			final WorkFactory workFactory) {
		mRequests = new LinkedBlockingQueue < LegStarRequest >(maxRequests);
		mShuttingDown = false;
		mWorkManager = workManager;
		mPoolManager = poolManager;
		mWorkFactory = workFactory;
		LOG.debug("Created engine instance:" + this);
	}
	
	/**
	 * When the Engine is started in a thread, it will continuously wait
	 * for requests until shutdown.
	 */
	public final void run() {
		
		while (!mShuttingDown) {
			LOG.debug("Waiting for requests");
			LegStarRequest request;
			try {
				request = mRequests.take();
				if (!mShuttingDown) {
					scheduleWork(request);
					LOG.debug("Scheduled Request:" + request.getID());
				} else {
					LOG.info("Engine stopped.");
				}
			} catch (InterruptedException e) {
				LOG.error("Failed to submit request", e);
			} catch (WorkException e) {
				LOG.error("Failed to submit request", e);
			}
		}
	}
	
	/**
	 * Take a connection from a connection pool and schedule work in the
	 * thread pool.
	 * @param request the request to be serviced
	 * @throws WorkException if scheduling fails
	 */
	private void scheduleWork(
			final LegStarRequest request) throws WorkException {
		LegStarConnection connection;
		ConnectionPool pool;
		try {
			/* Get a pool that matches this request criteria */
			pool = mPoolManager.getPool(request.getAddress(), true);
			connection = pool.take(CONNECT_TAKE_MSEC);
		} catch (ConnectionPoolException e) {
			/* Take ownership of request monitor so we can notify waiting
			 *  threads */
			synchronized (request) {
				request.setException(new RequestException(e));
				request.notifyAll();
			}
			throw new WorkException(e);
		}
		Work work = mWorkFactory.createWork(request, connection);
		WorkListener workListener = mWorkFactory.createWorkListener(
				request, connection, pool);
		mWorkManager.schedule(work, workListener);
	}
	
	/**
	 *  Place a new request in request queue.
	 *  @param request the request to be added
	 *   */
	public final void addRequest(final LegStarRequest request) {
		if (!mShuttingDown) {
			mRequests.add(request);
		} else {
			/* In theory we should create an exception for this request
			 * and notify the client. In order to keep shutdown simple,
			 * we simply log an error. Client will timeout waiting for
			 * a reply. */
			LOG.error("Request received while engine is shutting down.");
		}
	}
	
	/**
	 * If no requests are pending, shutdown the engine by faking a request
	 * to get it to check for the mShuttingDown flag.
	 * If requests are pending, they are probably blocked waiting for
	 * connections to become available.
	 *  */
	public final void shutDown() {
		mShuttingDown = true;
		LOG.info("Attempting to shutdown...");
		if (mRequests.size() == 0) {
			/* Empty request to get the engine to process shutdown */
			mRequests.add(new LegStarRequest());
		} else {
			LOG.warn("Shutdown requested. "
					+ mRequests.size() + " requests are pending.");
		}
	}

	/**
	 * @return the shutDown status
	 */
	public final boolean isShuttingDown() {
		return mShuttingDown;
	}
	
	/** (non-Javadoc).
	 * @see commonj.work.Work#isDaemon()
	 * TODO need to revisit this. Maybe the engine should be a daemon.
	 * {@inheritDoc}
	 */
	public final boolean isDaemon() {
		return false;
	}

	/** (non-Javadoc).
	 * @see commonj.work.Work#release()
	 */
	public final void release() {
	}
	
}
