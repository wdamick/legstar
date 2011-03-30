package com.legstar.test.cixs.fixarsim;

import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class FixarsimHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public FixarsimHostProgram() {
        setName("FIXARSIM");
        setMaxDataLength(15);
        setDataLength(15);
    }

}
