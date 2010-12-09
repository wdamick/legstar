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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.RequestException;

/**
 * Test the SlidingWindowKeepAlivePolicy class.
 * 
 */
public class TimerIdleConnectionsPolicyTest extends
        AbstractConnectionPoolTester {

    /** Pool size. */
    public static final int POOL_SIZE = 20;

    /** Number of simultaneous threads. */
    private static final int THREAD_NUMBER = 3;

    /** Number of requests for each thread. */
    private static final int REQUESTS_PER_THREAD = 100;

    /** Delay between 2 consecutive requests (ms). */
    private static final long REQUESTS_DELAY = 100L;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** A connection pool. */
    private ConnectionPool _connectionPool;

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        _connectionPool = getConnectionPool(POOL_SIZE);

    }

    /** {@inheritDoc} */
    public void tearDown() {
        _connectionPool.shutDown();
        assertNull(_connectionPool.getIdleConnectionsPolicy().getTimer());
    }

    /**
     * Check instantiation.
     * 
     * @throws Exception if test fails
     */
    public void testInstantiation() throws Exception {
        assertNull(_connectionPool.getIdleConnectionsPolicy().getException());
    }

    /**
     * Check that an idle connection gets closed.
     * 
     * @throws Exception if test fails
     */
    public void testClose() throws Exception {
        LegStarConnection connection = _connectionPool.take(1);
        connection.connectReuse(null);
        assertTrue(connection.isOpen());
        _connectionPool.put(connection);
        // Fake an idle time for the connection we just released
        Thread.sleep(2 * KEEP_ALIVE_TIME);
        LegStarConnection connection2 = _connectionPool.take(1);
        assertEquals(connection, connection2);
        assertFalse(connection2.isOpen());
        assertNull(_connectionPool.getIdleConnectionsPolicy().getException());
    }

    /**
     * With a large number of pooled connections, it takes time to check each
     * connection for obsolescence.
     * If we have concurrency issues, we might end up trying to send
     * on a closed connection
     * 
     * @throws Exception if test fails
     */
    public void testConcurrent() throws Exception {
        ConnectionConsumer[] generators = new ConnectionConsumer[THREAD_NUMBER];
        Thread[] threads = new Thread[THREAD_NUMBER];

        for (int i = 0; i < THREAD_NUMBER; i++) {
            generators[i] = new ConnectionConsumer();
        }
        for (int i = 0; i < THREAD_NUMBER; i++) {
            threads[i] = new Thread(generators[i]);
            threads[i].start();
        }
        for (int i = 0; i < THREAD_NUMBER; i++) {
            threads[i].join();
        }

        /*
         * At the end of the test we expect opened connections to have been
         * idle for less than the maximum idle time.
         */
        long now = System.currentTimeMillis();
        for (LegStarConnection connection : _connectionPool.getConnections()) {
            long openTime = now - connection.getLastUsedTime();
            if (_log.isDebugEnabled()) {
                _log.debug("Connection: " + connection.getConnectionID()
                        + ", ltu: " + connection.getLastUsedTime()
                        + ", openTime: " + openTime
                        + ", opened:" + connection.isOpen());
            }
            /*
             * Allow for 5% slack as we cannot guarantee that threads
             * will be serviced within a millisecond
             */
            if (openTime < 105 * KEEP_ALIVE_TIME / 100) {
                assertTrue(connection.isOpen());
            } else {
                assertFalse(connection.isOpen());
            }
        }
        assertNull(_connectionPool.getIdleConnectionsPolicy().getException());
    }

    /**
     * A thread that consumes connections using them and returning them to the
     * stack.
     * 
     */
    private class ConnectionConsumer implements Runnable {

        /**
         * Constructor.
         * 
         */
        public ConnectionConsumer() {
        }

        /** {@inheritDoc} */
        public void run() {
            for (int i = 0; i < REQUESTS_PER_THREAD; i++) {
                try {
                    LegStarConnection connection = _connectionPool.take(1);
                    if (_log.isDebugEnabled()) {
                        _log.debug("Polling returned connection: "
                                + connection.getConnectionID()
                                + ", ltu: "
                                + connection.getLastUsedTime()
                                + ", duration: "
                                + (System.currentTimeMillis() - connection
                                        .getLastUsedTime()));
                    }
                    connection.connectReuse(null);
                    connection.sendRequest(null);
                    Thread.sleep(REQUESTS_DELAY);
                    _connectionPool.put(connection);

                } catch (InterruptedException e) {
                    e.printStackTrace();
                    fail(e.getMessage());
                } catch (ConnectionException e) {
                    e.printStackTrace();
                    fail(e.getMessage());
                } catch (ConnectionPoolException e) {
                    e.printStackTrace();
                    fail(e.getMessage());
                } catch (RequestException e) {
                    e.printStackTrace();
                    fail(e.getMessage());
                }
            }

        }

    }

}
