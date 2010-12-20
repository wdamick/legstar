package com.legstar.coxb.cob2trans;

/**
 * Listeners are notified of progress in the generation process.
 * 
 */
public interface Cob2TransListener {

    /**
     * Signals a step performed.
     * 
     * @param e the step event object
     */
    void stepPerformed(Cob2TransEvent e);
}
