package com.legstar.test.cixs.redbotha;

import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class RedbothaHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public RedbothaHostProgram() {
        setName("REDBOTHA");
        setMaxDataLength(2);
        setDataLength(2);
    }

}
