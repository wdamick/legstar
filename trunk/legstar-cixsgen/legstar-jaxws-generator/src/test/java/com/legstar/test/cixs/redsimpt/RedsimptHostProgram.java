package com.legstar.test.cixs.redsimpt;

import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class RedsimptHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public RedsimptHostProgram() {
        setName("REDSIMPT");
        setMaxDataLength(18);
        setDataLength(18);
    }

}
