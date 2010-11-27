package com.legstar.coxb.cob2trans;

/**
 * The COBOL to Transformers generator was interrupted (cancelled).
 * 
 */
public class Cob2TransInterruptedException extends Cob2TransException {

    /** Unique serial ID. */
    private static final long serialVersionUID = -1;

    /**
     * Build Exception from message.
     * 
     * @param message exception description
     */
    public Cob2TransInterruptedException() {
        super("COBOL to Transformers generator interrupted");
    }

}
