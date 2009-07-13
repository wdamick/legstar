package com.legstar.pool.manager;

import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarConnection;
import com.legstar.mock.client.MockConnection;

import junit.framework.TestCase;

/**
 * Test the SlidingWindowKeepAlivePolicy class.
 *
 */
public class SlidingWindowKeepAlivePolicyTest extends TestCase {
    
    /**
     * Check instantiation.
     */
    public void testInstantiation() {
        SlidingWindowKeepAlivePolicy sw = new SlidingWindowKeepAlivePolicy(10L);
        assertEquals(10L, sw.getTimespan());
    }
    
    /**
     * Check the process of adding an event which should result in the window
     * sliding.
     * @throws Exception if test fails
     */
    public void testAdd() throws Exception {
        long timeBetweenAdds = 100L;
        SlidingWindowKeepAlivePolicy sw = new SlidingWindowKeepAlivePolicy(100L);
        LegStarConnection connection1 = new MockConnection();
        connection1.connect(null);
        sw.add(connection1);
        assertEquals(1, sw.getEventsInWindow());
        Thread.sleep(timeBetweenAdds + 10L);
        LegStarConnection connection2 = new MockConnection();
        connection2.connect(null);
        sw.add(connection2);
        assertEquals(1, sw.getEventsInWindow());
        assertFalse(connection1.isOpen());
        assertTrue(connection2.isOpen());
    }
    
    /**
     * Have 10 concurrent threads generate 10 events per second. That should
     * produce a 100 events/second ratio.
     * @throws Exception if test fails
     */
    public void testConcurrent() throws Exception {
        SlidingWindowKeepAlivePolicy sw = new SlidingWindowKeepAlivePolicy(500L);
        final int threads = 10;
        final int eventsPerThread = 110;
        final long delay = 100L;
        EventGenerator[] generators = new EventGenerator[threads];
        for (int i = 0; i < threads; i++) {
            generators[i] = new EventGenerator(eventsPerThread, delay, sw);
        }
        for (int i = 0; i < threads; i++) {
            new Thread(generators[i]).start();
        }
        Thread.sleep(2000L);
        double ratio = sw.getEventsRatio();
        assertTrue(80.0 <= ratio &&  ratio <= 120.0);
    }
    
    /**
     * A thread that generate events.
     *
     */
    private class EventGenerator implements Runnable {

        /** Number of events to generate. */
        final int _events;

        /** How much time to wait between events. */
        final long _delay;
        
        /** The sliding window. */
        final SlidingWindowKeepAlivePolicy _slidingWindow;
        
        /**
         * Constructor.
         * @param events number of events to generate
         * @param delay how much time to wait between events
         * @param slidingWindow the sliding window
         */
        public EventGenerator(
                final int events,
                final long delay,
                final SlidingWindowKeepAlivePolicy slidingWindow) {
            _events = events;
            _delay = delay;
            _slidingWindow = slidingWindow;
        }
 
        /** {@inheritDoc}*/
        public void run() {
            for (int i = 0; i < _events; i++) {
                LegStarConnection connection = new MockConnection();
                try {
                    connection.connect(null);
                    _slidingWindow.add(connection);
                    Thread.sleep(_delay);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                    fail(e.getMessage());
                } catch (ConnectionException e) {
                    e.printStackTrace();
                    fail(e.getMessage());
                } catch (ConnectionPoolException e) {
                    e.printStackTrace();
                    fail(e.getMessage());
                }
            }
            
        }
        
    }

}
