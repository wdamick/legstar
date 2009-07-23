package com.legstar.pool.manager;

import java.rmi.server.UID;
import java.util.Iterator;
import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.RequestException;
import com.legstar.mock.client.MockConnection;

import junit.framework.TestCase;

/**
 * Test the SlidingWindowKeepAlivePolicy class.
 *
 */
public class SlidingWindowKeepAlivePolicyTest extends TestCase {

    /** Size of the blocking queue. */
    private static final int QUEUE_SIZE = 20;

    /** Number of simultaneous threads. */
    private static final int THREAD_NUMBER = 3;

    /** Maximum connection time (ms).*/
    private static final long KEEP_ALIVE_TIME = 300L;

    /** Number of requests for each thread. */
    private static final int REQUESTS_PER_THREAD = 100;

    /** Delay between 2 consecutive requests (ms). */
    private static final long REQUESTS_DELAY = 100L;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Check instantiation.
     */
    public void testInstantiation() {
        SlidingWindowKeepAlivePolicy sw = new SlidingWindowKeepAlivePolicy(
                new BlockingStack < LegStarConnection >(QUEUE_SIZE), KEEP_ALIVE_TIME);
        assertEquals(KEEP_ALIVE_TIME, sw.getTimespan());
    }

    /**
     * Check the process of adding an event which should result in the window
     * sliding.
     * @throws Exception if test fails
     */
    public void testAdd() throws Exception {
        BlockingStack < LegStarConnection > queue =
            new BlockingStack < LegStarConnection >(QUEUE_SIZE);
        long timeBetweenAdds = KEEP_ALIVE_TIME;
        SlidingWindowKeepAlivePolicy sw = new SlidingWindowKeepAlivePolicy(queue, KEEP_ALIVE_TIME);
        LegStarConnection connection1 = new MockConnection();
        connection1.connect(null);
        queue.add(connection1);
        sw.closeObsoleteConnections();
        assertTrue(connection1.isOpen());
        Thread.sleep(timeBetweenAdds + 20L);
        sw.closeObsoleteConnections();
        assertFalse(connection1.isOpen());
    }

    /**
     * With a large number of pooled connections, it takes time to check each connection
     * for obsolescence. If we have concurrency issues, we might end up trying to send
     * on a closed connection
     * @throws Exception if test fails
     */
    public void testConcurrent() throws Exception {
        BlockingStack < LegStarConnection > queue =
            new BlockingStack < LegStarConnection >(QUEUE_SIZE);
        for (int i = 0; i < QUEUE_SIZE; i++) {
            LegStarConnection connection = new MockConnection(new UID().toString());
            queue.add(connection);
        }
        SlidingWindowKeepAlivePolicy sw = new SlidingWindowKeepAlivePolicy(queue, KEEP_ALIVE_TIME);
        ConnectionConsumer[] generators = new ConnectionConsumer[THREAD_NUMBER];
        Thread[] threads = new Thread[THREAD_NUMBER];

        for (int i = 0; i < THREAD_NUMBER; i++) {
            generators[i] = new ConnectionConsumer(sw);
        }
        for (int i = 0; i < THREAD_NUMBER; i++) {
            threads[i] = new Thread(generators[i]);
            threads[i].start();
        }
        for (int i = 0; i < THREAD_NUMBER; i++) {
            threads[i].join();
        }
        
        /* At the end of the test we expect opened connections to have keep alive times
         * smaller than the the threshold. */
        long now = System.currentTimeMillis();
        Iterator < LegStarConnection > iter = queue.iterator();
        while (iter.hasNext()) {
            LegStarConnection connection = iter.next();
            long openTime = now - connection.getLastUsedTime();
            if (_log.isDebugEnabled()) {
                _log.debug("Connection: " + connection.getConnectionID()
                        + ", ltu: " + connection.getLastUsedTime()
                        + ", openTime: " + openTime
                        + ", opened:" + connection.isOpen());
            }
            if (openTime < KEEP_ALIVE_TIME) {
                assertTrue(connection.isOpen());
            } else {
                assertFalse(connection.isOpen());
            }
        }
    }

    /**
     * A thread that consumes connections using them and returning them to the stack.
     *
     */
    private class ConnectionConsumer implements Runnable {

        /** The sliding window. */
        final SlidingWindowKeepAlivePolicy _slidingWindow;

        /**
         * Constructor.
         * @param slidingWindow the sliding window
         */
        public ConnectionConsumer(
                final SlidingWindowKeepAlivePolicy slidingWindow) {
            _slidingWindow = slidingWindow;
        }

        /** {@inheritDoc}*/
        public void run() {
            for (int i = 0; i < REQUESTS_PER_THREAD; i++) {
                try {
                    LegStarConnection connection = _slidingWindow.getConnections().poll(
                            3000L, TimeUnit.MILLISECONDS);
                    if (_log.isDebugEnabled()) {
                        _log.debug("Polling returned connection: " + connection.getConnectionID()
                                + ", ltu: " + connection.getLastUsedTime()
                                + ", duration: " + (System.currentTimeMillis() - connection.getLastUsedTime()));
                    }
                    connection.connectReuse(null);
                    connection.sendRequest(null);
                    Thread.sleep(REQUESTS_DELAY);
                    _slidingWindow.getConnections().add(connection);
                    _slidingWindow.closeObsoleteConnections();

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
