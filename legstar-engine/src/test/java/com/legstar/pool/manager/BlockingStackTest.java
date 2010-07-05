package com.legstar.pool.manager;

import java.util.LinkedList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import junit.framework.TestCase;

/**
 * Test the BlockingStack class.
 *
 */
public class BlockingStackTest extends TestCase {

    /** Number of elements produced.*/
    private static final int ELEMENTS_PRODUCED = 100;
    /** Number of concurrent consumers. */
    private static final int CONSUMERS_NUMBER = 4;
    /** Delay between to additions.*/
    private static final long PRODUCER_DELAY = 10L;
    /** Delay between 2 polls. */
    private static final long CONSUMER_DELAY = 100L;
    /** Maximum time to wait for element to become available.*/
    private static final long CONSUMER_TIMEOUT = 500L;
    /** Elements are string using this prefix.*/
    private static final String ELEMENTS_SUFFIX = "elem";

    /**
     * Check instantiation.
     */
    public void testInstantiation() {
        BlockingStack < String > stack = new BlockingStack < String >(5);
        assertEquals(5, stack.getCapacity());
    }

    /**
     * Basic add operation. Stack must not be full, elements are added
     * at the head.
     */
    public void testAdd() {
        BlockingStack < String > stack = new BlockingStack < String >(0);
        try {
            stack.add("element");
        } catch (IllegalStateException e) {
            assertEquals("Stack is full.", e.getMessage());
        }
        assertEquals(0, stack.getElementsList().size());
        stack = new BlockingStack < String >(2);
        assertTrue(stack.add("elementB"));
        assertEquals(1, stack.getElementsList().size());
        /** A replaces B as head */
        assertTrue(stack.add("elementA"));
        assertEquals(2, stack.getElementsList().size());
        assertEquals("elementA", stack.getElementsList().get(0));
        assertEquals("elementB", stack.getElementsList().get(1));
    }

    /**
     * Basic polling. Polling on an empty queue should return null after the
     * timeout. A successful polling is destructive.
     * Element returned must be head of stack.
     * Order must be preserved.
     */
    public void testPoll() {
        try {
            BlockingStack < String > stack = new BlockingStack < String >(0);
            assertEquals(null, stack.poll(100, TimeUnit.MILLISECONDS));
            stack = new BlockingStack < String >(3);
            stack.add("elementX");
            assertEquals("elementX", stack.poll(100, TimeUnit.MILLISECONDS));
            assertEquals(0, stack.getElementsList().size());
            stack.add("elementC");
            stack.add("elementB");
            stack.add("elementA");
            assertEquals("elementA", stack.poll(100, TimeUnit.MILLISECONDS));
            assertEquals("elementB", stack.poll(100, TimeUnit.MILLISECONDS));
            assertEquals("elementC", stack.poll(100, TimeUnit.MILLISECONDS));

        } catch (InterruptedException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Check concurrency control mechanisms.
     * Separate add and remove activities in specialized threads.
     * @throws Exception if something goes wrong
     */
    public void testConcurrent() throws Exception {

        BlockingStack < String > stack = new BlockingStack < String >(ELEMENTS_PRODUCED);
        Thread[] threads = new Thread[CONSUMERS_NUMBER + 1];
        Producer producer = new Producer(stack);
        threads[0] = new Thread(producer);
        Consumer[] consumers = new Consumer[CONSUMERS_NUMBER];
        for (int i = 0; i < CONSUMERS_NUMBER; i++) {
            consumers[i] = new Consumer(stack);
            threads[i + 1] = new Thread(consumers[i]);
        }
        for (int i = 0; i < threads.length; i++) {
            threads[i].start();
        }
        /* wait for producer to finish */
        threads[0].join();
        
        /* wait till there are no more items to consume */
        if (stack.getElementsList().size() > 0) {
            Thread.sleep(stack.getElementsList().size() * CONSUMER_DELAY / CONSUMERS_NUMBER);
        }
        
        /* stop consumers */
        for (int i = 0; i < CONSUMERS_NUMBER; i++) {
            consumers[i].stop.compareAndSet(false, true);
            threads[i + 1].join();
        }
        
        /*  Check that there are no exceptions */
        assertNull(producer.getException());
        for (int i = 0; i < CONSUMERS_NUMBER; i++) {
            assertNull(consumers[i].getException());
        }
        
        /* check that stack is now empty*/
        assertEquals(0, stack.getElementsList().size());
        
        /* check that waiters list is empty*/
        for (int i = 0; i < stack.getWaiters().size(); i++) {
            System.out.println("LATCH LEFT : " + stack.getWaiters().get(i));
        }
        assertEquals(0, stack.getWaiters().size());

        /* check that all elements produced were consumed */
        for (int i = 0; i < ELEMENTS_PRODUCED; i++) {
            String elem = ELEMENTS_SUFFIX + i;
            boolean found = false;
            for (int j = 0; j < CONSUMERS_NUMBER; j++) {
                if (consumers[j].getConsumed().contains(elem)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                fail("Element " + elem + " was not consumed");
            }
        }
    }

    /**
     * Adds elements into a shared stack.
     *
     */
    private class Producer implements Runnable {

        /** Reference to the shared stack.*/
        private BlockingStack < String > _stack;

        /** Keep the last exception.*/
        private Throwable _exception;

        /**
         * @param stack the shared stack
         */
        public Producer(
                final BlockingStack < String > stack) {
            _stack = stack;
        }

        /** {@inheritDoc} */
        public void run() {
            try {
                for (int i = 0; i < ELEMENTS_PRODUCED; i++) {
                    _stack.add(ELEMENTS_SUFFIX + i);
                    Thread.sleep(PRODUCER_DELAY);
                }
            } catch (IllegalStateException e) {
                _exception = e;
            } catch (InterruptedException e) {
                _exception = e;
            }

        }

        /**
         * @return the last exception
         */
        public Throwable getException() {
            return _exception;
        }

    }

    /**
     * Removes elements from shared stack.
     *
     */
    private class Consumer implements Runnable {

        /** Reference to the shared stack.*/
        private BlockingStack < String > _stack;

        /** Stop indicator. */
        public AtomicBoolean stop = new AtomicBoolean(false);

        /** Keep the last exception.*/
        private Throwable _exception;

        /** List of elements consumed. */
        private LinkedList < String > _consumed = new LinkedList < String >();

        /**
         * @param stack the shared stack
         */
        public Consumer(
                final BlockingStack < String > stack) {
            _stack = stack;
        }

        /** {@inheritDoc} */
        public void run() {
            try {
                while (!stop.get()) {
                    String result = _stack.poll(CONSUMER_TIMEOUT, TimeUnit.MILLISECONDS);
                    if (result == null) {
                        /* Timed out waiting for work. Give a chance to stop before trying.*/
                        break;
                    }
                    _consumed.add(result);
                    Thread.sleep(CONSUMER_DELAY);
                }
            } catch (IllegalStateException e) {
                _exception = e;
            } catch (InterruptedException e) {
                _exception = e;
            }

        }

        /**
         * @return the last exception
         */
        public Throwable getException() {
            return _exception;
        }
        /**
         * @return list of elements consumed
         */
        public LinkedList < String > getConsumed() {
            return _consumed;
        }

    }

}
