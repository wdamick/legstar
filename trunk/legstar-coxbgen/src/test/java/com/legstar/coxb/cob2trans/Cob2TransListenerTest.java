package com.legstar.coxb.cob2trans;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Test the generator feedback machanism.
 * 
 */
public class Cob2TransListenerTest extends AbstractCob2TransTester implements
        Cob2TransListener {

    /** The list of events we received. */
    private List < Cob2TransEvent > _events;

    /** An instance of the generator. */
    private Cob2TransGenerator _generator;

    /** {@inheritDoc} */
    public void setUp() {
        _events = new ArrayList < Cob2TransEvent >();
        Cob2TransModel context = new Cob2TransModel();
        _generator = new Cob2TransGenerator(context);
        _generator.addCob2TransListener(this);
    }

    /**
     * Make sure we receive the corrrect number of events.
     * 
     * @throws Exception if something goes wrong
     */
    public void testNormal() throws Exception {

        GeneratorRunnable runnable = new GeneratorRunnable(_generator);
        runnable.getThread().join();

        assertNull(runnable.getException());
        assertNotNull(runnable.getJarFile());
        assertEquals(12, _events.size());

    }

    /**
     * Make sure the async process is interruptible.
     * 
     * @throws Exception if something goes wrong
     */
    public void testInterrupt() throws Exception {

        GeneratorRunnable runnable = new GeneratorRunnable(_generator);
        _generator.interrupt();
        runnable.getThread().join();

        assertEquals("interrupted", runnable.getException().getMessage());
        assertNull(runnable.getJarFile());

    }

    /**
     * A simple asynchronous execution for the generator.
     * 
     */
    class GeneratorRunnable implements Runnable {

        /** An instance of the generator. */
        private Cob2TransGenerator _generator;

        /** The thread running this runnable. */
        private Thread _thread;

        /** The result jar file. */
        private File _jarFile;

        /** In case something went wrong. */
        private Throwable _exception;

        /**
         * @param generator an instance of the generator
         */
        public GeneratorRunnable(final Cob2TransGenerator generator) {
            _generator = generator;
            _thread = new Thread(this, "Cob2TransGenerator");
            _thread.start();
        }

        /** {@inheritDoc} */
        public void run() {
            try {
                _jarFile = _generator.generate(newTcobwvb(), TARGET_DIR);
            } catch (Cob2TransException e) {
                _exception = e;
            }
        }

        /**
         * @return the _thread
         */
        public Thread getThread() {
            return _thread;
        }

        /**
         * @return the result jar file
         */
        public File getJarFile() {
            return _jarFile;
        }

        /**
         * @return the _exception
         */
        public Throwable getException() {
            return _exception;
        }
    }

    /** {@inheritDoc} */
    public void stepPerformed(final Cob2TransEvent e) {
        _events.add(e);
    }
}
